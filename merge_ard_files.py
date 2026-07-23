"""
Merge selected ARD Excel files from the same clinical study.

The script:
1. merges the union of rows and columns from the selected ARDs;
2. fills empty cells with available values;
3. preserves different populated values using " | ";
4. consolidates repeated complete ARD keys inside each source file;
5. keeps rows with incomplete keys separate to avoid an unsafe merge;
6. consolidates PARAMCD_DICT and creates merge/QC reports.

Dependencies
------------
pip install pandas openpyxl
"""

from __future__ import annotations

from numbers import Number
from pathlib import Path
from typing import Any, Iterable

import pandas as pd


# ==========================================================
# Configuration - edit only this section
# ==========================================================

INPUT_FOLDER = Path(
    r"C:\Users\JMende95\OneDrive - JNJ\Desktop\ard_data"
)

OUTPUT_FOLDER = INPUT_FOLDER / "merged_xlsx"

FILES_TO_MERGE = [
    "77242113UCO2001_anthem_wk12_ard_20260615.xlsx",
    "77242113UCO2001_anthem_wk28_ard_20260616.xlsx",
    "77242113UCO2001_anthem_wk78_ard_20260630.xlsx",
]

OUTPUT_NAME = "anthem_merged.xlsx"

DEFAULT_KEYS = [
    "USUBJID",
    "AVISIT",
    "AVISITN",
    "AVISIT_ORDER",
]

VALUE_SEPARATOR = " | "


# ==========================================================
# Internal constants
# ==========================================================

INTERNAL_ROW_KEY = "__ARD_MERGE_INTERNAL_ROW_KEY__"

CONFLICT_COLUMNS = [
    *DEFAULT_KEYS,
    "CONFLICT_STAGE",
    "COLUMN",
    "ACCUMULATED_FILES",
    "ACCUMULATED_VALUE",
    "INCOMING_FILE",
    "INCOMING_VALUE",
    "MERGED_VALUE",
]

DUPLICATE_REPORT_COLUMNS = [
    "FILE",
    *DEFAULT_KEYS,
    "ROW_COUNT",
    "KEY_STATUS",
    "ACTION",
    "CONFLICT_COLUMNS",
]


# ==========================================================
# Value utilities
# ==========================================================

def is_missing(value: Any) -> bool:
    """Return True for None, NaN, NaT, pd.NA, and empty strings."""
    if value is None:
        return True

    if isinstance(value, str):
        return not value.strip()

    try:
        result = pd.isna(value)
        if isinstance(result, bool):
            return result
        return bool(result)
    except (TypeError, ValueError):
        return False


def display_value(value: Any) -> str:
    """Convert a value to a stable text representation for reports."""
    if is_missing(value):
        return ""

    if isinstance(value, pd.Timestamp):
        if value.hour == value.minute == value.second == value.microsecond == 0:
            return value.strftime("%Y-%m-%d")
        return value.strftime("%Y-%m-%d %H:%M:%S")

    return str(value).strip()


def values_equal(left: Any, right: Any) -> bool:
    """Compare ARD cell values while treating 1 and 1.0 as equal."""
    if is_missing(left) and is_missing(right):
        return True

    if is_missing(left) or is_missing(right):
        return False

    if (
        isinstance(left, Number)
        and not isinstance(left, bool)
        and isinstance(right, Number)
        and not isinstance(right, bool)
    ):
        return float(left) == float(right)

    return display_value(left) == display_value(right)


def split_unique_values(
    value: Any,
    separator: str = VALUE_SEPARATOR,
) -> list[str]:
    """Return the unique alternatives already stored in an ARD cell."""
    if is_missing(value):
        return []

    text = display_value(value)
    unique_values: list[str] = []

    for item in text.split(separator):
        item = item.strip()
        if item and item not in unique_values:
            unique_values.append(item)

    return unique_values


def merge_cell_values(
    left: Any,
    right: Any,
    separator: str = VALUE_SEPARATOR,
) -> Any:
    """
    Merge two cell values without discarding information.

    Equal values are retained once. Different populated values are converted
    to text and combined with the configured separator.
    """
    if is_missing(left):
        return right

    if is_missing(right):
        return left

    if values_equal(left, right):
        return left

    merged_values: list[str] = []

    for value in (
        split_unique_values(left, separator)
        + split_unique_values(right, separator)
    ):
        if value not in merged_values:
            merged_values.append(value)

    return separator.join(merged_values)


def merge_series_values(
    series: pd.Series,
    separator: str = VALUE_SEPARATOR,
) -> Any:
    """Consolidate all nonmissing values from one duplicated-key column."""
    merged_value: Any = None

    for value in series:
        merged_value = merge_cell_values(
            merged_value,
            value,
            separator,
        )

    return merged_value


def has_missing_key(
    df: pd.DataFrame,
    keys: list[str],
) -> pd.Series:
    """Return a mask identifying rows with at least one incomplete key."""
    return df[keys].map(is_missing).any(axis=1)


# ==========================================================
# Validation and within-file consolidation
# ==========================================================

def validate_required_keys(
    df: pd.DataFrame,
    keys: list[str],
    filename: str,
) -> None:
    """Validate that all required ARD key columns are available."""
    missing_keys = [key for key in keys if key not in df.columns]

    if missing_keys:
        raise ValueError(
            f"{filename}: missing required key columns: {missing_keys}"
        )

    if INTERNAL_ROW_KEY in df.columns:
        raise ValueError(
            f"{filename}: reserved internal column already exists: "
            f"{INTERNAL_ROW_KEY}"
        )


def find_conflicting_columns(
    group: pd.DataFrame,
    keys: list[str],
    separator: str,
) -> list[str]:
    """List non-key columns containing more than one distinct value."""
    conflicts: list[str] = []

    for column in group.columns:
        if column in keys:
            continue

        merged_value: Any = None
        observed_alternatives: list[str] = []

        for value in group[column]:
            if is_missing(value):
                continue

            merged_value = merge_cell_values(
                merged_value,
                value,
                separator,
            )

            for alternative in split_unique_values(value, separator):
                if alternative not in observed_alternatives:
                    observed_alternatives.append(alternative)

        if len(observed_alternatives) > 1:
            conflicts.append(column)

    return conflicts


def build_within_file_conflicts(
    duplicated_rows: pd.DataFrame,
    keys: list[str],
    filename: str,
    separator: str,
) -> list[dict[str, Any]]:
    """Create conflict records for repeated complete keys in one file."""
    records: list[dict[str, Any]] = []

    if duplicated_rows.empty:
        return records

    grouped = duplicated_rows.groupby(
        keys,
        dropna=False,
        sort=False,
    )

    for group_key, group in grouped:
        if not isinstance(group_key, tuple):
            group_key = (group_key,)

        for column in group.columns:
            if column in keys:
                continue

            alternatives: list[str] = []
            merged_value: Any = None

            for value in group[column]:
                if is_missing(value):
                    continue

                merged_value = merge_cell_values(
                    merged_value,
                    value,
                    separator,
                )

                for alternative in split_unique_values(value, separator):
                    if alternative not in alternatives:
                        alternatives.append(alternative)

            if len(alternatives) <= 1:
                continue

            record = {
                key: value
                for key, value in zip(keys, group_key)
            }
            record.update(
                {
                    "CONFLICT_STAGE": "WITHIN_FILE_DUPLICATE",
                    "COLUMN": column,
                    "ACCUMULATED_FILES": filename,
                    "ACCUMULATED_VALUE": alternatives[0],
                    "INCOMING_FILE": filename,
                    "INCOMING_VALUE": separator.join(alternatives[1:]),
                    "MERGED_VALUE": display_value(merged_value),
                }
            )
            records.append(record)

    return records


def consolidate_duplicate_keys(
    df: pd.DataFrame,
    keys: list[str],
    filename: str,
    separator: str = VALUE_SEPARATOR,
) -> tuple[
    pd.DataFrame,
    pd.DataFrame,
    list[dict[str, Any]],
    dict[str, int],
]:
    """
    Consolidate repeated complete keys inside one ARD.

    Rows with incomplete keys are deliberately kept separate. Without a
    complete key, combining them could merge unrelated clinical observations.
    """
    working = df.copy().astype(object)
    incomplete_mask = has_missing_key(working, keys)

    complete = working.loc[~incomplete_mask].copy()
    incomplete = working.loc[incomplete_mask].copy()

    complete_duplicate_mask = complete.duplicated(keys, keep=False)
    duplicated_complete_rows = complete.loc[complete_duplicate_mask].copy()

    duplicate_records: list[dict[str, Any]] = []

    if not duplicated_complete_rows.empty:
        for group_key, group in duplicated_complete_rows.groupby(
            keys,
            dropna=False,
            sort=False,
        ):
            if not isinstance(group_key, tuple):
                group_key = (group_key,)

            conflicting_columns = find_conflicting_columns(
                group,
                keys,
                separator,
            )

            record = {
                "FILE": filename,
                **{
                    key: value
                    for key, value in zip(keys, group_key)
                },
                "ROW_COUNT": len(group),
                "KEY_STATUS": "COMPLETE",
                "ACTION": "CONSOLIDATED",
                "CONFLICT_COLUMNS": separator.join(conflicting_columns),
            }
            duplicate_records.append(record)

    incomplete_duplicate_mask = incomplete.duplicated(
        keys,
        keep=False,
    )
    duplicated_incomplete_rows = incomplete.loc[
        incomplete_duplicate_mask
    ].copy()

    if not duplicated_incomplete_rows.empty:
        for group_key, group in duplicated_incomplete_rows.groupby(
            keys,
            dropna=False,
            sort=False,
        ):
            if not isinstance(group_key, tuple):
                group_key = (group_key,)

            record = {
                "FILE": filename,
                **{
                    key: value
                    for key, value in zip(keys, group_key)
                },
                "ROW_COUNT": len(group),
                "KEY_STATUS": "INCOMPLETE",
                "ACTION": "KEPT_SEPARATE",
                "CONFLICT_COLUMNS": "",
            }
            duplicate_records.append(record)

    value_columns = [
        column
        for column in working.columns
        if column not in keys
    ]

    unique_complete = complete.loc[
        ~complete_duplicate_mask
    ].copy()

    if not duplicated_complete_rows.empty:
        aggregation_rules = {
            column: (
                lambda series, sep=separator: merge_series_values(series, sep)
            )
            for column in value_columns
        }

        consolidated_duplicates = (
            duplicated_complete_rows.groupby(
                keys,
                dropna=False,
                sort=False,
                as_index=False,
            )
            .agg(aggregation_rules)
        )

        if unique_complete.empty:
            consolidated_complete = consolidated_duplicates
        else:
            consolidated_complete = pd.concat(
                [unique_complete, consolidated_duplicates],
                ignore_index=True,
                sort=False,
            )
    else:
        consolidated_complete = complete.copy()

    if consolidated_complete.empty:
        consolidated = incomplete.copy()
    elif incomplete.empty:
        consolidated = consolidated_complete.copy()
    else:
        consolidated = pd.concat(
            [consolidated_complete, incomplete],
            ignore_index=True,
            sort=False,
        )
    consolidated = consolidated.reindex(columns=working.columns)
    consolidated = consolidated.astype(object)

    conflicts = build_within_file_conflicts(
        duplicated_complete_rows,
        keys,
        filename,
        separator,
    )

    duplicate_report = pd.DataFrame(
        duplicate_records,
        columns=[
            "FILE",
            *keys,
            "ROW_COUNT",
            "KEY_STATUS",
            "ACTION",
            "CONFLICT_COLUMNS",
        ],
    )

    statistics = {
        "INPUT_ROWS": len(working),
        "NULL_KEY_ROWS": int(incomplete_mask.sum()),
        "DUPLICATE_COMPLETE_KEY_ROWS": int(
            complete_duplicate_mask.sum()
        ),
        "DUPLICATE_COMPLETE_KEY_GROUPS": int(
            len(
                duplicated_complete_rows[keys].drop_duplicates()
            )
        ),
        "ROWS_AFTER_INTERNAL_CONSOLIDATION": len(consolidated),
    }

    return consolidated, duplicate_report, conflicts, statistics


def add_internal_row_key(
    df: pd.DataFrame,
    keys: list[str],
    filename: str,
) -> pd.DataFrame:
    """
    Add an internal discriminator.

    Complete keys receive the same blank discriminator and can match across
    files. Incomplete keys receive a unique file/row value and stay separate.
    """
    result = df.copy().astype(object)
    incomplete_mask = has_missing_key(result, keys)

    result[INTERNAL_ROW_KEY] = ""

    incomplete_positions = result.index[incomplete_mask]
    for sequence, row_index in enumerate(incomplete_positions, start=1):
        result.at[
            row_index,
            INTERNAL_ROW_KEY,
        ] = f"{filename}::incomplete_key_row::{sequence}"

    return result


# ==========================================================
# Cross-file merge
# ==========================================================

def merge_ard_tables(
    ard_tables: dict[str, pd.DataFrame],
    keys: list[str],
    separator: str = VALUE_SEPARATOR,
) -> tuple[
    pd.DataFrame,
    pd.DataFrame,
    pd.DataFrame,
    dict[str, dict[str, int]],
]:
    """
    Merge ARD tables cell by cell.

    Returns:
    - consolidated ARD;
    - VALUE_CONFLICTS report;
    - DUPLICATE_KEYS report;
    - within-file consolidation statistics.
    """
    merged: pd.DataFrame | None = None
    processed_files: list[str] = []
    conflict_records: list[dict[str, Any]] = []
    duplicate_reports: list[pd.DataFrame] = []
    file_statistics: dict[str, dict[str, int]] = {}

    internal_keys = [*keys, INTERNAL_ROW_KEY]

    for filename, original in ard_tables.items():
        validate_required_keys(original, keys, filename)

        (
            consolidated,
            duplicate_report,
            within_file_conflicts,
            statistics,
        ) = consolidate_duplicate_keys(
            original,
            keys,
            filename,
            separator,
        )

        file_statistics[filename] = statistics
        duplicate_reports.append(duplicate_report)
        conflict_records.extend(within_file_conflicts)

        current = add_internal_row_key(
            consolidated,
            keys,
            filename,
        )
        current = current.set_index(internal_keys).astype(object)

        print(
            f"  Prepared {filename}: "
            f"{statistics['INPUT_ROWS']:,} -> "
            f"{statistics['ROWS_AFTER_INTERNAL_CONSOLIDATION']:,} rows"
        )

        if statistics["DUPLICATE_COMPLETE_KEY_GROUPS"]:
            print(
                "    Complete duplicated-key groups consolidated: "
                f"{statistics['DUPLICATE_COMPLETE_KEY_GROUPS']:,}"
            )

        if statistics["NULL_KEY_ROWS"]:
            print(
                "    Incomplete-key rows preserved separately: "
                f"{statistics['NULL_KEY_ROWS']:,}"
            )

        if merged is None:
            merged = current.copy().astype(object)
            processed_files.append(filename)
            continue

        all_columns = list(merged.columns)
        all_columns.extend(
            column
            for column in current.columns
            if column not in all_columns
        )

        # Append/drop_duplicates preserves load order and avoids sorting mixed
        # key data types during the index union.
        all_index = merged.index.append(current.index).drop_duplicates()

        merged = merged.reindex(
            index=all_index,
            columns=all_columns,
        ).astype(object)

        current = current.reindex(
            index=all_index,
            columns=all_columns,
        ).astype(object)

        for column in all_columns:
            left_series = merged[column]
            right_series = current[column]

            left_present = left_series.map(
                lambda value: not is_missing(value)
            )
            right_present = right_series.map(
                lambda value: not is_missing(value)
            )

            fill_mask = ~left_present & right_present
            if fill_mask.any():
                merged.loc[fill_mask, column] = right_series.loc[fill_mask]

            both_mask = left_present & right_present
            if not both_mask.any():
                continue

            for row_key in merged.index[both_mask]:
                left_value = merged.at[row_key, column]
                right_value = current.at[row_key, column]

                if values_equal(left_value, right_value):
                    continue

                combined_value = merge_cell_values(
                    left_value,
                    right_value,
                    separator,
                )

                clinical_key = row_key[: len(keys)]
                conflict = {
                    key: value
                    for key, value in zip(keys, clinical_key)
                }
                conflict.update(
                    {
                        "CONFLICT_STAGE": "BETWEEN_FILES",
                        "COLUMN": column,
                        "ACCUMULATED_FILES": separator.join(
                            processed_files
                        ),
                        "ACCUMULATED_VALUE": display_value(left_value),
                        "INCOMING_FILE": filename,
                        "INCOMING_VALUE": display_value(right_value),
                        "MERGED_VALUE": display_value(combined_value),
                    }
                )
                conflict_records.append(conflict)

                merged.at[row_key, column] = combined_value

        processed_files.append(filename)

    if merged is None:
        raise ValueError("No ARD tables were loaded.")

    merged = merged.reset_index()
    merged = merged.drop(columns=[INTERNAL_ROW_KEY])
    merged = sort_merged_ard(merged)

    conflicts = pd.DataFrame(
        conflict_records,
        columns=[
            *keys,
            "CONFLICT_STAGE",
            "COLUMN",
            "ACCUMULATED_FILES",
            "ACCUMULATED_VALUE",
            "INCOMING_FILE",
            "INCOMING_VALUE",
            "MERGED_VALUE",
        ],
    )

    nonempty_duplicate_reports = [
        report
        for report in duplicate_reports
        if not report.empty
    ]

    if nonempty_duplicate_reports:
        duplicates = pd.concat(
            nonempty_duplicate_reports,
            ignore_index=True,
            sort=False,
        )
    else:
        duplicates = pd.DataFrame(
            columns=[
                "FILE",
                *keys,
                "ROW_COUNT",
                "KEY_STATUS",
                "ACTION",
                "CONFLICT_COLUMNS",
            ]
        )

    return merged, conflicts, duplicates, file_statistics


def sort_merged_ard(df: pd.DataFrame) -> pd.DataFrame:
    """Apply a stable clinical sort without failing on mixed data types."""
    result = df.copy()
    helper_columns: list[str] = []

    if "USUBJID" in result.columns:
        helper = "__SORT_USUBJID__"
        result[helper] = result["USUBJID"].map(display_value)
        helper_columns.append(helper)

    if "AVISITN" in result.columns:
        numeric_helper = "__SORT_AVISITN_NUMERIC__"
        text_helper = "__SORT_AVISITN_TEXT__"

        result[numeric_helper] = pd.to_numeric(
            result["AVISITN"],
            errors="coerce",
        )
        result[numeric_helper] = result[numeric_helper].fillna(float("inf"))
        result[text_helper] = result["AVISITN"].map(display_value)

        helper_columns.extend([numeric_helper, text_helper])

    for source_column, helper in [
        ("AVISIT_ORDER", "__SORT_AVISIT_ORDER__"),
        ("AVISIT", "__SORT_AVISIT__"),
    ]:
        if source_column in result.columns:
            result[helper] = result[source_column].map(display_value)
            helper_columns.append(helper)

    if helper_columns:
        result = result.sort_values(
            helper_columns,
            kind="stable",
            na_position="last",
        )
        result = result.drop(columns=helper_columns)

    return result.reset_index(drop=True)


# ==========================================================
# Reports
# ==========================================================

def compare_files(
    ard_tables: dict[str, pd.DataFrame],
    keys: list[str],
    file_statistics: dict[str, dict[str, int]],
) -> pd.DataFrame:
    """Create a file-level comparison and QC summary."""
    all_columns: set[str] = set()
    common_columns: set[str] | None = None

    for df in ard_tables.values():
        current_columns = set(df.columns)
        all_columns.update(current_columns)

        if common_columns is None:
            common_columns = current_columns
        else:
            common_columns &= current_columns

    common_columns = common_columns or set()
    records: list[dict[str, Any]] = []

    for filename, df in ard_tables.items():
        validate_required_keys(df, keys, filename)

        incomplete_mask = has_missing_key(df, keys)
        complete = df.loc[~incomplete_mask]
        statistics = file_statistics[filename]

        records.append(
            {
                "FILE": filename,
                "ROWS": len(df),
                "ROWS_AFTER_INTERNAL_CONSOLIDATION": statistics[
                    "ROWS_AFTER_INTERNAL_CONSOLIDATION"
                ],
                "COLUMNS": len(df.columns),
                "SUBJECTS": df["USUBJID"].nunique(dropna=True),
                "VISITS": df["AVISIT"].nunique(dropna=True),
                "UNIQUE_COMPLETE_KEYS": complete[
                    keys
                ].drop_duplicates().shape[0],
                "NULL_KEY_ROWS_KEPT_SEPARATE": statistics[
                    "NULL_KEY_ROWS"
                ],
                "DUPLICATE_COMPLETE_KEY_ROWS": statistics[
                    "DUPLICATE_COMPLETE_KEY_ROWS"
                ],
                "DUPLICATE_COMPLETE_KEY_GROUPS": statistics[
                    "DUPLICATE_COMPLETE_KEY_GROUPS"
                ],
                "COLUMNS_ONLY_IN_THIS_FILE": len(
                    set(df.columns) - common_columns
                ),
                "MISSING_FROM_GLOBAL_COLUMN_UNION": len(
                    all_columns - set(df.columns)
                ),
            }
        )

    return pd.DataFrame(records)


def merge_paramcd_dict(
    dictionaries: Iterable[pd.DataFrame],
) -> pd.DataFrame:
    """Consolidate PARAMCD dictionaries, removing exact duplicates only."""
    available = [
        df.copy()
        for df in dictionaries
        if df is not None and not df.empty
    ]

    if not available:
        return pd.DataFrame(
            columns=["PARAMCD", "PARAM", "SOURCE"]
        )

    merged = pd.concat(
        available,
        ignore_index=True,
        sort=False,
    )

    expected_order = [
        column
        for column in ["PARAMCD", "PARAM", "SOURCE"]
        if column in merged.columns
    ]
    other_columns = [
        column
        for column in merged.columns
        if column not in expected_order
    ]

    merged = merged[expected_order + other_columns]
    merged = merged.drop_duplicates().reset_index(drop=True)

    sort_columns = [
        column
        for column in ["SOURCE", "PARAMCD", "PARAM"]
        if column in merged.columns
    ]

    if sort_columns:
        temporary_sort_columns: list[str] = []

        for position, column in enumerate(sort_columns):
            helper = f"__DICT_SORT_{position}__"
            merged[helper] = merged[column].map(display_value)
            temporary_sort_columns.append(helper)

        merged = merged.sort_values(
            temporary_sort_columns,
            kind="stable",
            na_position="last",
        )
        merged = merged.drop(columns=temporary_sort_columns)
        merged = merged.reset_index(drop=True)

    return merged


def create_merge_summary(
    files: list[Path],
    ard_tables: dict[str, pd.DataFrame],
    merged_ard: pd.DataFrame,
    merged_dict: pd.DataFrame,
    conflicts: pd.DataFrame,
    duplicates: pd.DataFrame,
    file_statistics: dict[str, dict[str, int]],
) -> pd.DataFrame:
    """Create a compact overview of the completed merge."""
    consolidated_groups = 0

    if not duplicates.empty:
        consolidated_groups = int(
            (duplicates["ACTION"] == "CONSOLIDATED").sum()
        )

    metrics = [
        ("FILES_MERGED", len(files)),
        (
            "INPUT_ROWS_TOTAL",
            sum(len(df) for df in ard_tables.values()),
        ),
        ("OUTPUT_ARD_ROWS", len(merged_ard)),
        ("OUTPUT_ARD_COLUMNS", len(merged_ard.columns)),
        (
            "OUTPUT_SUBJECTS",
            merged_ard["USUBJID"].nunique(dropna=True),
        ),
        (
            "OUTPUT_VISITS",
            merged_ard["AVISIT"].nunique(dropna=True),
        ),
        (
            "INCOMPLETE_KEY_ROWS_KEPT_SEPARATE",
            sum(
                statistics["NULL_KEY_ROWS"]
                for statistics in file_statistics.values()
            ),
        ),
        (
            "DUPLICATE_KEY_GROUPS_CONSOLIDATED",
            consolidated_groups,
        ),
        ("VALUE_CONFLICT_RECORDS", len(conflicts)),
        ("PARAMCD_DICT_ROWS", len(merged_dict)),
    ]

    return pd.DataFrame(metrics, columns=["METRIC", "VALUE"])


# ==========================================================
# Excel input/output
# ==========================================================

def validate_configuration() -> tuple[list[Path], Path]:
    """Validate configured paths and return input/output files."""
    if not INPUT_FOLDER.exists():
        raise FileNotFoundError(
            f"Input folder was not found:\n{INPUT_FOLDER}"
        )

    if len(FILES_TO_MERGE) < 2:
        raise ValueError(
            "FILES_TO_MERGE must contain at least two Excel files."
        )

    if len(FILES_TO_MERGE) != len(set(FILES_TO_MERGE)):
        raise ValueError(
            "FILES_TO_MERGE contains repeated filenames."
        )

    if not OUTPUT_NAME.lower().endswith(".xlsx"):
        raise ValueError(
            "OUTPUT_NAME must end with '.xlsx'."
        )

    input_files = [
        INPUT_FOLDER / filename
        for filename in FILES_TO_MERGE
    ]

    missing_files = [
        file
        for file in input_files
        if not file.is_file()
    ]

    if missing_files:
        formatted = "\n".join(
            f"  - {file.name}"
            for file in missing_files
        )
        raise FileNotFoundError(
            "The following configured files were not found:\n"
            f"{formatted}\n\n"
            f"Expected folder:\n{INPUT_FOLDER}"
        )

    output_file = OUTPUT_FOLDER / OUTPUT_NAME

    if output_file in input_files:
        raise ValueError(
            "The output file cannot also be an input file."
        )

    return input_files, output_file


def load_workbooks(
    files: list[Path],
) -> tuple[dict[str, pd.DataFrame], list[pd.DataFrame]]:
    """Read ARD and optional PARAMCD_DICT worksheets."""
    ard_tables: dict[str, pd.DataFrame] = {}
    dictionaries: list[pd.DataFrame] = []

    print(f"Files selected: {len(files)}\n")

    for file in files:
        print(f"  Reading: {file.name}")

        with pd.ExcelFile(file, engine="openpyxl") as excel:
            if "ARD" not in excel.sheet_names:
                raise ValueError(
                    f"{file.name}: sheet 'ARD' was not found."
                )

            ard = pd.read_excel(
                excel,
                sheet_name="ARD",
            )
            ard_tables[file.name] = ard

            if "PARAMCD_DICT" in excel.sheet_names:
                dictionary = pd.read_excel(
                    excel,
                    sheet_name="PARAMCD_DICT",
                )
                dictionaries.append(dictionary)
            else:
                print(
                    "    Warning: PARAMCD_DICT sheet was not found."
                )

    return ard_tables, dictionaries


def write_output_workbook(
    output_file: Path,
    merged_ard: pd.DataFrame,
    merged_dict: pd.DataFrame,
    merge_summary: pd.DataFrame,
    comparison: pd.DataFrame,
    conflicts: pd.DataFrame,
    duplicates: pd.DataFrame,
    source_files: pd.DataFrame,
) -> None:
    """Write the final ARD and all traceability reports."""
    output_file.parent.mkdir(
        parents=True,
        exist_ok=True,
    )

    try:
        with pd.ExcelWriter(
            output_file,
            engine="openpyxl",
        ) as writer:
            merged_ard.to_excel(
                writer,
                sheet_name="ARD",
                index=False,
            )
            merged_dict.to_excel(
                writer,
                sheet_name="PARAMCD_DICT",
                index=False,
            )
            merge_summary.to_excel(
                writer,
                sheet_name="MERGE_SUMMARY",
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
            duplicates.to_excel(
                writer,
                sheet_name="DUPLICATE_KEYS",
                index=False,
            )
            source_files.to_excel(
                writer,
                sheet_name="SOURCE_FILES",
                index=False,
            )
    except PermissionError as error:
        raise PermissionError(
            f"Could not write the output file:\n{output_file}\n\n"
            "Close the workbook in Excel and run the script again."
        ) from error


# ==========================================================
# Main
# ==========================================================

def main() -> None:
    input_files, output_file = validate_configuration()

    ard_tables, dictionaries = load_workbooks(input_files)

    print("\nValidating and merging ARD tables...")

    (
        merged_ard,
        conflicts,
        duplicates,
        file_statistics,
    ) = merge_ard_tables(
        ard_tables,
        DEFAULT_KEYS,
        VALUE_SEPARATOR,
    )

    comparison = compare_files(
        ard_tables,
        DEFAULT_KEYS,
        file_statistics,
    )
    merged_dict = merge_paramcd_dict(dictionaries)

    source_files = pd.DataFrame(
        {
            "LOAD_ORDER": range(1, len(input_files) + 1),
            "FILE": [file.name for file in input_files],
        }
    )

    merge_summary = create_merge_summary(
        input_files,
        ard_tables,
        merged_ard,
        merged_dict,
        conflicts,
        duplicates,
        file_statistics,
    )

    print("\nWriting output workbook...")

    write_output_workbook(
        output_file,
        merged_ard,
        merged_dict,
        merge_summary,
        comparison,
        conflicts,
        duplicates,
        source_files,
    )

    print("\nMerge completed successfully.")
    print(f"Output: {output_file}")
    print(f"ARD rows: {len(merged_ard):,}")
    print(f"ARD columns: {len(merged_ard.columns):,}")
    print(f"Subjects: {merged_ard['USUBJID'].nunique(dropna=True):,}")
    print(f"Visits: {merged_ard['AVISIT'].nunique(dropna=True):,}")
    print(f"PARAMCD dictionary rows: {len(merged_dict):,}")
    print(f"Conflict records: {len(conflicts):,}")
    print(f"Duplicated-key groups reported: {len(duplicates):,}")


if __name__ == "__main__":
    main()
