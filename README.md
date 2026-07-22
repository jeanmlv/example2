# example2

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
