"""
Merge selected ARD Excel files from the same clinical study.

Dependencies
------------
pip install pandas openpyxl
"""

from __future__ import annotations

from pathlib import Path
from typing import Any, Iterable

import pandas as pd


# ==========================================================
# Configuration
# ==========================================================

INPUT_FOLDER = Path(
    r"C:\Users\JMende95\OneDrive - JNJ\Desktop\ard_data"
)

OUTPUT_FOLDER = INPUT_FOLDER / "merged_xlsx"
OUTPUT_FOLDER.mkdir(parents=True, exist_ok=True)

FILES_TO_MERGE = [
    "77242113UCO2001_anthem_wk12_ard_20260615.xlsx",
    "77242113UCO2001_anthem_wk28_ard_20260616.xlsx",
    "77242113UCO2001_anthem_wk78_ard_20260630.xlsx",
]

OUTPUT_NAME = "anthem_merged.xlsx"

DEFAULT_KEYS = ["USUBJID", "AVISIT", "AVISITN", "AVISIT_ORDER"]


def is_missing(value: Any) -> bool:
    if value is None:
        return True
    try:
        if pd.isna(value):
            return True
    except (TypeError, ValueError):
        pass
    return isinstance(value, str) and not value.strip()


def display_value(value: Any) -> str:
    if isinstance(value, pd.Timestamp):
        if value.hour == value.minute == value.second == value.microsecond == 0:
            return value.strftime("%Y-%m-%d")
        return value.strftime("%Y-%m-%d %H:%M:%S")
    return str(value).strip()


def split_unique_values(value: Any, separator: str = " | ") -> list[str]:
    if is_missing(value):
        return []
    values: list[str] = []
    for item in display_value(value).split(separator):
        item = item.strip()
        if item and item not in values:
            values.append(item)
    return values


def merge_cell_values(left: Any, right: Any, separator: str = " | ") -> Any:
    if is_missing(left):
        return right
    if is_missing(right):
        return left

    left_text = display_value(left)
    right_text = display_value(right)

    if left_text == right_text:
        return left

    merged_values: list[str] = []
    candidates = split_unique_values(left, separator) + split_unique_values(right, separator)

    for value in candidates:
        if value not in merged_values:
            merged_values.append(value)

    return separator.join(merged_values)


def validate_keys(df: pd.DataFrame, keys: list[str], filename: str) -> None:
    missing_keys = [key for key in keys if key not in df.columns]
    if missing_keys:
        raise ValueError(f"{filename}: missing required key columns: {missing_keys}")

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
    all_columns: set[str] = set()
    for df in ard_tables.values():
        all_columns.update(df.columns)

    common_columns = set.intersection(*[set(df.columns) for df in ard_tables.values()])

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
                "COLUMNS_ONLY_IN_THIS_FILE": len(set(df.columns) - common_columns),
                "MISSING_FROM_GLOBAL_COLUMN_UNION": len(all_columns - set(df.columns)),
            }
        )

    return pd.DataFrame(records)


def merge_ard_tables(
    ard_tables: dict[str, pd.DataFrame],
    keys: list[str],
    separator: str = " | ",
) -> tuple[pd.DataFrame, pd.DataFrame]:
    merged: pd.DataFrame | None = None
    conflict_records: list[dict[str, Any]] = []

    for filename, current in ard_tables.items():
        current = current.copy()
        validate_keys(current, keys, filename)
        current = current.set_index(keys)

        if merged is None:
            merged = current
            continue

        all_columns = list(merged.columns)
        all_columns.extend(column for column in current.columns if column not in all_columns)
        all_index = merged.index.union(current.index)

        merged = merged.reindex(index=all_index, columns=all_columns)
        current = current.reindex(index=all_index, columns=all_columns)

        for column in all_columns:
            left_series = merged[column]
            right_series = current[column]

            right_present = right_series.notna()
            if not right_present.any():
                continue

            fill_mask = left_series.isna() & right_present
            merged.loc[fill_mask, column] = right_series.loc[fill_mask]

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

                combined_value = merge_cell_values(left_value, right_value, separator)

                conflict = {key: value for key, value in zip(keys, row_key)}
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

    return merged, pd.DataFrame(conflict_records)


def merge_paramcd_dict(dictionaries: Iterable[pd.DataFrame]) -> pd.DataFrame:
    available = [df.copy() for df in dictionaries if not df.empty]

    if not available:
        return pd.DataFrame(columns=["PARAMCD", "PARAM", "SOURCE"])

    merged = pd.concat(available, ignore_index=True, sort=False)
    expected_order = [
        column
        for column in ["PARAMCD", "PARAM", "SOURCE"]
        if column in merged.columns
    ]
    other_columns = [column for column in merged.columns if column not in expected_order]

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


def main() -> None:
    input_files = [INPUT_FOLDER / filename for filename in FILES_TO_MERGE]

    missing_files = [file for file in input_files if not file.exists()]
    if missing_files:
        missing_text = "\n".join(f"  - {file}" for file in missing_files)
        raise FileNotFoundError(
            "The following input files were not found:\n"
            f"{missing_text}"
        )

    if len(input_files) < 2:
        raise ValueError("At least two files must be listed in FILES_TO_MERGE.")

    output_file = OUTPUT_FOLDER / OUTPUT_NAME

    ard_tables: dict[str, pd.DataFrame] = {}
    dictionaries: list[pd.DataFrame] = []

    print(f"Files selected for merge: {len(input_files)}\n")

    for file in input_files:
        print(f"Reading: {file.name}")
        excel = pd.ExcelFile(file)

        if "ARD" not in excel.sheet_names:
            raise ValueError(f"{file.name}: sheet 'ARD' was not found.")

        ard_tables[file.name] = pd.read_excel(file, sheet_name="ARD")

        if "PARAMCD_DICT" in excel.sheet_names:
            dictionaries.append(pd.read_excel(file, sheet_name="PARAMCD_DICT"))

    print("\nComparing files...")
    comparison = compare_files(ard_tables, DEFAULT_KEYS)

    print("Merging ARD sheets...")
    merged_ard, conflicts = merge_ard_tables(ard_tables, DEFAULT_KEYS)

    print("Merging PARAMCD dictionaries...")
    merged_dict = merge_paramcd_dict(dictionaries)

    source_files = pd.DataFrame(
        {
            "LOAD_ORDER": range(1, len(input_files) + 1),
            "FILE": [file.name for file in input_files],
        }
    )

    print("Writing final workbook...")
    with pd.ExcelWriter(output_file, engine="openpyxl") as writer:
        merged_ard.to_excel(writer, sheet_name="ARD", index=False)
        merged_dict.to_excel(writer, sheet_name="PARAMCD_DICT", index=False)
        comparison.to_excel(writer, sheet_name="FILE_COMPARISON", index=False)
        conflicts.to_excel(writer, sheet_name="VALUE_CONFLICTS", index=False)
        source_files.to_excel(writer, sheet_name="SOURCE_FILES", index=False)

    print("\nMerge completed successfully.")
    print(f"Output file: {output_file}")
    print(f"ARD rows: {len(merged_ard):,}")
    print(f"ARD columns: {len(merged_ard.columns):,}")
    print(f"PARAMCD dictionary rows: {len(merged_dict):,}")
    print(f"Different populated cells combined: {len(conflicts):,}")


if __name__ == "__main__":
    main()
