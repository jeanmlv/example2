# example2

"""
Merge multiple ARD Excel files from the same clinical study.

Strategy
--------
1. Uses the ARD row key:
   USUBJID + AVISIT + AVISITN + AVISIT_ORDER
2. Creates the union of all rows and all columns.
3. For the same row/column:
   - equal values are kept once;
   - null + populated keeps the populated value;
   - different populated values are combined with " | ",
     removing repeated alternatives.
4. Consolidates PARAMCD_DICT without duplicate records.
5. Produces comparison and conflict reports for traceability.

Dependencies
------------
pip install pandas openpyxl
"""
# ======================================================
# Files to merge
# ======================================================

INPUT_FOLDER = Path(
    r"C:\Users\JMende95\OneDrive - JNJ\Desktop\ard_data"
)

OUTPUT_FOLDER = INPUT_FOLDER / "merged_xlsx"
OUTPUT_FOLDER.mkdir(exist_ok=True)

FILES_TO_MERGE = [

    "77242113UCO2001_anthem_wk12_ard_20260615.xlsx",
    "77242113UCO2001_anthem_wk28_ard_20260616.xlsx",
    "77242113UCO2001_anthem_wk78_ard_20260630.xlsx",

]

OUTPUT_NAME = "anthem_merged.xlsx"

from __future__ import annotations

import argparse
import math
from pathlib import Path
from typing import Any, Iterable

import pandas as pd


DEFAULT_KEYS = ["USUBJID", "AVISIT", "AVISITN", "AVISIT_ORDER"]


def is_missing(value: Any) -> bool:
    """Return True for None, NaN, NaT, and empty strings."""
    if value is None:
        return True

    try:
        if pd.isna(value):
            return True
    except (TypeError, ValueError):
        pass

    return isinstance(value, str) and not value.strip()


def display_value(value: Any) -> str:
    """Convert a value to a stable text representation."""
    if isinstance(value, pd.Timestamp):
        if value.hour == value.minute == value.second == value.microsecond == 0:
            return value.strftime("%Y-%m-%d")
        return value.strftime("%Y-%m-%d %H:%M:%S")

    return str(value).strip()


def split_unique_values(value: Any, separator: str = " | ") -> list[str]:
    """
    Split an already aggregated ARD value and return unique non-empty tokens.

    ARDs commonly store multiple values in one cell separated by ' | '.
    """
    if is_missing(value):
        return []

    text = display_value(value)

    values: list[str] = []
    for item in text.split(separator):
        item = item.strip()
        if item and item not in values:
            values.append(item)

    return values


def merge_cell_values(left: Any, right: Any, separator: str = " | ") -> Any:
    """
    Merge two cell values without losing information.

    Equal values are not repeated. Different values are combined.
    """
    if is_missing(left):
        return right

    if is_missing(right):
        return left

    left_text = display_value(left)
    right_text = display_value(right)

    if left_text == right_text:
        return left

    merged_values: list[str] = []

    for value in split_unique_values(left, separator) + split_unique_values(
        right, separator
    ):
        if value not in merged_values:
            merged_values.append(value)

    return separator.join(merged_values)


def validate_keys(df: pd.DataFrame, keys: list[str], filename: str) -> None:
    """Validate required key columns and duplicated row keys."""
    missing_keys = [key for key in keys if key not in df.columns]
    if missing_keys:
        raise ValueError(
            f"{filename}: missing required key columns: {missing_keys}"
        )

    duplicated = df.duplicated(keys, keep=False)
    if duplicated.any():
        examples = df.loc[duplicated, keys].head(10)
        raise ValueError(
            f"{filename}: duplicated ARD keys were found.\n"
            f"Examples:\n{examples.to_string(index=False)}"
        )


def compare_files(
    ard_tables: dict[str, pd.DataFrame],
    keys: list[str],
) -> pd.DataFrame:
    """Create a file-level comparison summary."""
    all_columns: set[str] = set()
    for df in ard_tables.values():
        all_columns.update(df.columns)

    records: list[dict[str, Any]] = []

    for filename, df in ard_tables.items():
        records.append(
            {
                "FILE": filename,
                "ROWS": len(df),
                "COLUMNS": len(df.columns),
                "SUBJECTS": df["USUBJID"].nunique(dropna=True),
                "VISITS": df["AVISIT"].nunique(dropna=True),
                "UNIQUE_KEYS": df[keys].drop_duplicates().shape[0],
                "NULL_KEY_ROWS": int(df[keys].isna().any(axis=1).sum()),
                "COLUMNS_ONLY_IN_THIS_FILE": len(
                    set(df.columns)
                    - set.intersection(
                        *[
                            set(other.columns)
                            for other in ard_tables.values()
                        ]
                    )
                ),
                "MISSING_FROM_GLOBAL_COLUMN_UNION": len(
                    all_columns - set(df.columns)
                ),
            }
        )

    return pd.DataFrame(records)


def merge_ard_tables(
    ard_tables: dict[str, pd.DataFrame],
    keys: list[str],
    separator: str = " | ",
) -> tuple[pd.DataFrame, pd.DataFrame]:
    """
    Merge ARD tables cell by cell and return:
    - consolidated ARD
    - detailed conflict report
    """
    merged: pd.DataFrame | None = None
    conflict_records: list[dict[str, Any]] = []

    for filename, current in ard_tables.items():
        current = current.copy()
        validate_keys(current, keys, filename)
        current = current.set_index(keys)

        if merged is None:
            merged = current
            continue

        # Keep original column order and append newly discovered columns.
        all_columns = list(merged.columns)
        all_columns.extend(
            column for column in current.columns if column not in all_columns
        )

        all_index = merged.index.union(current.index)

        merged = merged.reindex(index=all_index, columns=all_columns)
        current = current.reindex(index=all_index, columns=all_columns)

        for column in all_columns:
            left_series = merged[column]
            right_series = current[column]

            right_present = right_series.notna()
            if not right_present.any():
                continue

            # Fill cells that were empty in the accumulated table.
            fill_mask = left_series.isna() & right_present
            merged.loc[fill_mask, column] = right_series.loc[fill_mask]

            # Resolve cells populated in both files.
            both_mask = left_series.notna() & right_present
            if not both_mask.any():
                continue

            for row_key in merged.index[both_mask]:
                left_value = merged.at[row_key, column]
                right_value = current.at[row_key, column]

                left_text = display_value(left_value)
                right_text = display_value(right_value)

                if left_text == right_text:
                    continue

                combined_value = merge_cell_values(
                    left_value, right_value, separator
                )

                conflict = {
                    key: value
                    for key, value in zip(keys, row_key)
                }
                conflict.update(
                    {
                        "COLUMN": column,
                        "ACCUMULATED_VALUE": left_text,
                        "INCOMING_FILE": filename,
                        "INCOMING_VALUE": right_text,
                        "MERGED_VALUE": combined_value,
                    }
                )
                conflict_records.append(conflict)

                merged.at[row_key, column] = combined_value

    if merged is None:
        raise ValueError("No ARD tables were loaded.")

    merged = merged.reset_index()

    # Stable clinical ordering.
    sort_columns = [
        column
        for column in ["USUBJID", "AVISITN", "AVISIT_ORDER", "AVISIT"]
        if column in merged.columns
    ]
    merged = merged.sort_values(
        sort_columns,
        kind="stable",
        na_position="last",
    ).reset_index(drop=True)

    conflicts = pd.DataFrame(conflict_records)

    return merged, conflicts


def merge_paramcd_dict(
    dictionaries: Iterable[pd.DataFrame],
) -> pd.DataFrame:
    """Consolidate PARAMCD dictionaries without exact duplicates."""
    available = [df.copy() for df in dictionaries if not df.empty]

    if not available:
        return pd.DataFrame(columns=["PARAMCD", "PARAM", "SOURCE"])

    merged = pd.concat(available, ignore_index=True, sort=False)

    expected_order = [
        column
        for column in ["PARAMCD", "PARAM", "SOURCE"]
        if column in merged.columns
    ]
    other_columns = [
        column for column in merged.columns if column not in expected_order
    ]

    merged = merged[expected_order + other_columns]
    merged = merged.drop_duplicates().reset_index(drop=True)

    sort_columns = [
        column
        for column in ["SOURCE", "PARAMCD", "PARAM"]
        if column in merged.columns
    ]
    if sort_columns:
        merged = merged.sort_values(
            sort_columns,
            kind="stable",
            na_position="last",
        ).reset_index(drop=True)

    return merged


def find_excel_files(input_folder: Path, pattern: str) -> list[Path]:
    """Find input workbooks, excluding previously generated merged files."""
    files = sorted(input_folder.glob(pattern))

    return [
        file
        for file in files
        if not file.name.lower().endswith("_merged.xlsx")
        and not file.name.startswith("~$")
    ]


def main() -> None:
    parser = argparse.ArgumentParser(
        description="Compare and merge ARD Excel files from the same study."
    )
    parser.add_argument(
        "--input-folder",
        type=Path,
        required=True,
        help="Folder containing the ARD Excel files.",
    )
    parser.add_argument(
        "--pattern",
        default="*anthem*_ard_*.xlsx",
        help='Input filename pattern. Default: "*anthem*_ard_*.xlsx"',
    )
    parser.add_argument(
        "--output",
        type=Path,
        default=None,
        help="Output workbook path.",
    )
    parser.add_argument(
        "--keys",
        nargs="+",
        default=DEFAULT_KEYS,
        help="ARD row-key columns.",
    )
    args = parser.parse_args()

    input_folder = args.input_folder.expanduser().resolve()
    files = find_excel_files(input_folder, args.pattern)

    if len(files) < 2:
        raise FileNotFoundError(
            f"At least two matching files are required. Found: {len(files)}"
        )

    output = (
        args.output.expanduser().resolve()
        if args.output
        else input_folder / "anthem_ard_merged.xlsx"
    )

    ard_tables: dict[str, pd.DataFrame] = {}
    dictionaries: list[pd.DataFrame] = []

    print(f"Found {len(files)} ARD files:\n")

    for file in files:
        print(f"  - {file.name}")

        excel = pd.ExcelFile(file)

        if "ARD" not in excel.sheet_names:
            raise ValueError(f"{file.name}: sheet 'ARD' was not found.")

        ard_tables[file.name] = pd.read_excel(file, sheet_name="ARD")

        if "PARAMCD_DICT" in excel.sheet_names:
            dictionaries.append(
                pd.read_excel(file, sheet_name="PARAMCD_DICT")
            )

    comparison = compare_files(ard_tables, args.keys)
    merged_ard, conflicts = merge_ard_tables(
        ard_tables,
        args.keys,
    )
    merged_dict = merge_paramcd_dict(dictionaries)

    source_files = pd.DataFrame(
        {
            "LOAD_ORDER": range(1, len(files) + 1),
            "FILE": [file.name for file in files],
        }
    )

    output.parent.mkdir(parents=True, exist_ok=True)

    with pd.ExcelWriter(output, engine="openpyxl") as writer:
        merged_ard.to_excel(writer, sheet_name="ARD", index=False)
        merged_dict.to_excel(
            writer,
            sheet_name="PARAMCD_DICT",
            index=False,
        )
        comparison.to_excel(
            writer,
            sheet_name="FILE_COMPARISON",
            index=False,
        )
        conflicts.to_excel(
            writer,
            sheet_name="VALUE_CONFLICTS",
            index=False,
        )
        source_files.to_excel(
            writer,
            sheet_name="SOURCE_FILES",
            index=False,
        )

    print("\nMerge completed.")
    print(f"Output: {output}")
    print(f"ARD rows: {len(merged_ard):,}")
    print(f"ARD columns: {len(merged_ard.columns):,}")
    print(f"PARAMCD dictionary rows: {len(merged_dict):,}")
    print(f"Different populated cells combined: {len(conflicts):,}")


if __name__ == "__main__":
    main()
