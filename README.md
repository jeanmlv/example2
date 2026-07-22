# example2

import pandas as pd
from pathlib import Path

# ==========================================================
# Configuration
# ==========================================================

input_folder = Path(
    r"C:\Users\JMende95\OneDrive - JNJ\Desktop\ard_data"
)

output_folder = input_folder / "csv"
output_folder.mkdir(exist_ok=True)

# ==========================================================
# Convert all ARD Excel files
# ==========================================================

xlsx_files = sorted(input_folder.glob("*.xlsx"))

if not xlsx_files:
    print("No Excel files found.")
    exit()

print(f"Found {len(xlsx_files)} Excel files.\n")

converted = 0
failed = 0

for file in xlsx_files:

    print(f"Processing: {file.name}")

    try:

        # Read worksheets
        ard_df = pd.read_excel(file, sheet_name="ARD")
        dict_df = pd.read_excel(file, sheet_name="PARAMCD_DICT")

        # Base filename (without extension)
        base_name = file.stem

        # Output filenames
        ard_output = output_folder / f"{base_name}_ARD.csv"
        dict_output = output_folder / f"{base_name}_PARAMCD_DICT.csv"

        # Export CSV
        ard_df.to_csv(
            ard_output,
            index=False,
            encoding="utf-8-sig"
        )

        dict_df.to_csv(
            dict_output,
            index=False,
            encoding="utf-8-sig"
        )

        print("   ✓ ARD exported")
        print("   ✓ PARAMCD_DICT exported\n")

        converted += 1

    except Exception as e:

        failed += 1
        print(f"   ✗ Error: {e}\n")

# ==========================================================
# Summary
# ==========================================================

print("=" * 50)
print("Conversion completed")
print("=" * 50)
print(f"Files converted : {converted}")
print(f"Files failed    : {failed}")
print(f"CSV folder      : {output_folder}")
