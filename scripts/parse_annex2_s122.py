"""
Parse the SCOTUS Section 122 Annex II document and generate s122.yaml config files
with product exemptions (rate = 0) for all listed HTS codes.

Usage:
    python scripts/parse_annex2_s122.py

Input:
    C:/Users/jar335/Downloads/2026Section122_ANNEX2_Final_extracted.txt

Output:
    config/2-20_with_s122-preempt-temp_annex/2026-02-20/s122.yaml
    config/2-20_with_s122-preempt-perm_annex/2026-02-20/s122.yaml
"""

import re
import os
import shutil


def parse_hts_codes(filepath):
    """
    Parse HTS codes from the Annex II extracted text file.

    HTS codes appear at the start of lines in formats like:
      0201.10.05
      8505.11.0070  (10-digit with statistical suffix)
      8543.70.60

    We need to distinguish actual HTS code lines from descriptions that
    happen to mention HTS subheadings (e.g., "subheading 8543.70").
    """
    codes = set()

    # Pattern for HTS codes at the START of a line (after optional whitespace):
    #   - 8-digit: NNNN.NN.NN (e.g., 0201.10.05)
    #   - 10-digit: NNNN.NN.NNNN (e.g., 8505.11.0070)
    # The code must start at the beginning of the line (possibly after whitespace)
    # and be followed by whitespace or end of line.
    hts_pattern = re.compile(
        r'^\s*(\d{4}\.\d{2}\.\d{2,4})\b',
        re.MULTILINE
    )

    with open(filepath, 'r', encoding='utf-8') as f:
        text = f.read()

    for match in hts_pattern.finditer(text):
        raw_code = match.group(1)
        # Strip dots to get pure numeric string
        numeric_code = raw_code.replace('.', '')

        # Validate lengths: should be 8 or 10 digits
        if len(numeric_code) in (8, 10):
            codes.add(numeric_code)
        else:
            print(f'  WARNING: Unexpected code length {len(numeric_code)} for {raw_code} -> {numeric_code}')

    return sorted(codes)


def write_s122_yaml(codes, output_path):
    """Write the s122.yaml file with all exempt product codes."""
    header = """\
# =============================================================================
# Section 122 Tariffs - ANNEX II PRODUCT EXEMPTIONS
# =============================================================================
#
# Flat 10% rate for ALL countries with product exemptions from the official
# Section 122 Annex II list (SCOTUS ruling implementation).
#
# =============================================================================

headline_rates:
  default: 0.10

product_rates:
"""

    os.makedirs(os.path.dirname(output_path), exist_ok=True)

    with open(output_path, 'w', encoding='utf-8', newline='\n') as f:
        f.write(header)
        for code in codes:
            f.write(f"  '{code}': 0\n")
        f.write('\nproduct_country_rates: []\n')


def main():
    input_path = 'C:/Users/jar335/Downloads/2026Section122_ANNEX2_Final_extracted.txt'
    repo_root = 'C:/Users/jar335/Documents/Repositories/Tariff-ETRs'

    output_temp = os.path.join(
        repo_root,
        'config/2-20_with_s122-preempt-temp_annex/2026-02-20/s122.yaml'
    )
    output_perm = os.path.join(
        repo_root,
        'config/2-20_with_s122-preempt-perm_annex/2026-02-20/s122.yaml'
    )

    print(f'Reading: {input_path}')
    codes = parse_hts_codes(input_path)
    print(f'Total HTS codes extracted: {len(codes)}')

    # Show digit-length breakdown
    len_counts = {}
    for c in codes:
        l = len(c)
        len_counts[l] = len_counts.get(l, 0) + 1
    for l in sorted(len_counts):
        print(f'  {l}-digit codes: {len_counts[l]}')

    # Show sample codes
    print(f'\nFirst 10 codes:')
    for code in codes[:10]:
        print(f'  {code}')
    print(f'\nLast 10 codes:')
    for code in codes[-10:]:
        print(f'  {code}')

    # Write to first destination
    print(f'\nWriting: {output_temp}')
    write_s122_yaml(codes, output_temp)

    # Copy to second destination
    print(f'Copying to: {output_perm}')
    os.makedirs(os.path.dirname(output_perm), exist_ok=True)
    shutil.copy2(output_temp, output_perm)

    print('\nDone.')


if __name__ == '__main__':
    main()
