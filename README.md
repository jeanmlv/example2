# example2

import pandas as pd
from pathlib import Path

input_file = Path(
    "54781532UC02001_jak_uc_ard_20260622.xlsx"
)

output_folder = Path("csv")
output_folder.mkdir(exist_ok=True)

ard_df = pd.read_excel(
    input_file,
    sheet_name="ARD"
)

dictionary_df = pd.read_excel(
    input_file,
    sheet_name="PARAMCD_DICT"
)

ard_df.to_csv(
    output_folder / "jak_uc_ard.csv",
    index=False,
    encoding="utf-8-sig"
)

dictionary_df.to_csv(
    output_folder / "jak_uc_paramcd_dict.csv",
    index=False,
    encoding="utf-8-sig"
)

print("CSV files created successfully.")
