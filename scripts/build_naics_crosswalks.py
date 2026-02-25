# =============================================================================
# build_naics_crosswalks.py
# =============================================================================
#
# One-time script to build two crosswalk files:
#   1. resources/hs10_naics_crosswalk.csv  (HS10 -> 6-digit NAICS)
#   2. resources/naics_bea_summary_crosswalk.csv  (NAICS -> BEA summary code)
#
# Sources:
#   - CBO commodity crosswalk (HS10 -> NAICS)
#   - BEA NAICS concordance Excel (NAICS -> BEA summary code)
#
# =============================================================================

import csv
import openpyxl
import os

PROJECT_ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))

# =============================================================================
# 1. Extract HS10 -> NAICS from CBO crosswalk
# =============================================================================

cbo_path = os.path.join(
    PROJECT_ROOT, '..', 'conventional-tariff-analysis-model',
    'inputs', 'commodity_crosswalks',
    'commodity_crosswalk_Jan_2023_with_cmac.csv'
)

print(f'Reading CBO crosswalk: {cbo_path}')

hs10_naics_rows = []
with open(cbo_path, newline='') as f:
    reader = csv.DictReader(f)
    for row in reader:
        hs10 = row['hts10'].strip()
        naics = row['naics'].strip()
        if hs10 and naics:
            hs10_naics_rows.append((hs10, naics))

# Deduplicate (some HS10 codes may appear multiple times with same NAICS)
hs10_naics_rows = sorted(set(hs10_naics_rows))

out_path_1 = os.path.join(PROJECT_ROOT, 'resources', 'hs10_naics_crosswalk.csv')
with open(out_path_1, 'w', newline='') as f:
    writer = csv.writer(f)
    writer.writerow(['hs10', 'naics'])
    writer.writerows(hs10_naics_rows)

print(f'Wrote {len(hs10_naics_rows)} rows to {out_path_1}')

# =============================================================================
# 2. Extract NAICS -> BEA summary code from BEA concordance Excel
# =============================================================================

excel_path = os.path.join(
    PROJECT_ROOT, 'resources', 'bea',
    'BEA-NAICS-Concordance.xlsx'
)

print(f'\nReading BEA concordance: {excel_path}')

wb = openpyxl.load_workbook(excel_path, read_only=True)
ws = wb['NAICS Codes']

# Row structure (0-indexed from data start at row 6):
#   col 2: BEA Summary code
#   col 11: Related 2017 NAICS code
naics_bea_rows = []
current_summary = None

for row in ws.iter_rows(min_row=6, values_only=True):
    summary = row[2]
    naics = row[11]

    # BEA summary code is only populated on the first row of each group
    if summary is not None:
        current_summary = str(summary)

    if naics is None or current_summary is None:
        continue

    # Convert NAICS to string (it may be int or float from Excel)
    naics_str = str(int(naics)) if isinstance(naics, (int, float)) else str(naics).strip()
    if not naics_str:
        continue

    naics_bea_rows.append((naics_str, current_summary))

wb.close()

# Deduplicate
naics_bea_rows = sorted(set(naics_bea_rows))

out_path_2 = os.path.join(PROJECT_ROOT, 'resources', 'naics_bea_summary_crosswalk.csv')
with open(out_path_2, 'w', newline='') as f:
    writer = csv.writer(f)
    writer.writerow(['naics', 'bea_summary_code'])
    writer.writerows(naics_bea_rows)

print(f'Wrote {len(naics_bea_rows)} rows to {out_path_2}')

# Summary stats
unique_naics = len(set(r[0] for r in naics_bea_rows))
unique_bea = len(set(r[1] for r in naics_bea_rows))
print(f'Unique NAICS codes: {unique_naics}')
print(f'Unique BEA summary codes: {unique_bea}')
