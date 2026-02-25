#!/usr/bin/env python3
import yaml
import re
import glob
import os

REPO = r'C:\Users\jar335\Documents\Repositories\Tariff-ETRs'

def load_zero_product_codes(yaml_path):
    with open(yaml_path, 'r', encoding='utf-8') as f:
        data = yaml.safe_load(f)
    pr = data.get('product_rates', {})
    if not pr:
        return set()
    codes = set()
    for k, v in pr.items():
        if isinstance(v, (int, float)) and v == 0:
            codes.add(str(k))
    return codes

def load_all_product_codes(yaml_path):
    with open(yaml_path, 'r', encoding='utf-8') as f:
        data = yaml.safe_load(f)
    pr = data.get('product_rates', {})
    if not pr:
        return {}
    result = {}
    for k, v in pr.items():
        result[str(k)] = v
    return result

def load_annex_txt(txt_path):
    with open(txt_path, 'r', encoding='utf-8', errors='replace') as f:
        text = f.read()
    pattern = r'\d{4}\.\d{2}\.\d{2,4}'
    matches = re.findall(pattern, text)
    codes = set(m.replace('.', '') for m in matches)
    return codes

def subset_report(name_a, set_a, name_b, set_b):
    only_a = set_a - set_b
    only_b = set_b - set_a
    common = set_a & set_b
    print('  ' + name_a + ': ' + str(len(set_a)) + ' codes')
    print('  ' + name_b + ': ' + str(len(set_b)) + ' codes')
    print('  In common: ' + str(len(common)))
    print('  Only in ' + name_a + ': ' + str(len(only_a)))
    print('  Only in ' + name_b + ': ' + str(len(only_b)))
    if set_a == set_b:
        print('  ==> IDENTICAL sets')
    elif set_a <= set_b:
        print('  ==> ' + name_a + ' IS a strict subset of ' + name_b)
    elif set_b <= set_a:
        print('  ==> ' + name_b + ' IS a strict subset of ' + name_a)
    else:
        print('  ==> Neither is a subset of the other (partial overlap)')
    print()

def show_diff(title, diff_set, limit=15):
    diff_sorted = sorted(diff_set)
    print(title + ' (' + str(len(diff_sorted)) + '):')
    for c in diff_sorted[:limit]:
        print('    ' + c)
    if len(diff_sorted) > limit:
        print('    ... and ' + str(len(diff_sorted) - limit) + ' more')
    print()

def main():
    print('=' * 70)
    print('STEP 1: Load product_rates (rate==0) from the two annex YAML configs')
    print('=' * 70)

    s122_annex_yaml = os.path.join(REPO, 'config', '2-20_with_s122-preempt-temp_annex', '2026-01-01', 'ieepa_reciprocal.yaml')
    nov_annex_yaml = os.path.join(REPO, 'config', '2-20_with_s122-preempt-temp_annexnov', '2026-01-01', 'ieepa_reciprocal.yaml')

    s122_yaml_codes = load_zero_product_codes(s122_annex_yaml)
    nov_yaml_codes = load_zero_product_codes(nov_annex_yaml)

    print('S122 annex YAML (temp_annex):    ' + str(len(s122_yaml_codes)) + ' zero-rated codes')
    print('Nov annex YAML  (temp_annexnov): ' + str(len(nov_yaml_codes)) + ' zero-rated codes')
    print()

    print('=' * 70)
    print('STEP 2: Load HTS codes from source annex text files')
    print('=' * 70)

    nov_txt_path = r'C:\Users\jar335\Downloads\2025Nov_ReciprocalTariff_ANNEX_II_extracted.txt'
    s122_txt_path = r'C:\Users\jar335\Downloads\2026Section122_ANNEX2_Final_extracted.txt'

    nov_txt_codes = load_annex_txt(nov_txt_path)
    s122_txt_codes = load_annex_txt(s122_txt_path)

    print('November annex TXT: ' + str(len(nov_txt_codes)) + ' codes')
    print('S122 annex TXT:     ' + str(len(s122_txt_codes)) + ' codes')
    print()

    print('=' * 70)
    print('STEP 3: Relationship analysis')
    print('=' * 70)
    print()

    print('--- (A) S122 annex YAML vs S122 annex TXT ---')
    subset_report('S122_yaml', s122_yaml_codes, 'S122_txt', s122_txt_codes)

    print('--- (B) Nov annex YAML vs November annex TXT ---')
    subset_report('Nov_yaml', nov_yaml_codes, 'Nov_txt', nov_txt_codes)

    print('--- (C) S122 annex YAML vs November annex TXT ---')
    subset_report('S122_yaml', s122_yaml_codes, 'Nov_txt', nov_txt_codes)

    print('--- (D) S122 annex YAML vs Nov annex YAML ---')
    subset_report('S122_yaml', s122_yaml_codes, 'Nov_yaml', nov_yaml_codes)

    print('--- (E) S122 annex TXT vs November annex TXT ---')
    subset_report('S122_txt', s122_txt_codes, 'Nov_txt', nov_txt_codes)

    print('=' * 70)
    print('STEP 4: All ieepa_reciprocal.yaml files in the repo')
    print('=' * 70)
    print()

    search_patterns = [
        os.path.join(REPO, 'config', '*', 'ieepa_reciprocal.yaml'),
        os.path.join(REPO, 'config', '*', '*', 'ieepa_reciprocal.yaml'),
    ]

    all_yamls = set()
    for p in search_patterns:
        all_yamls.update(glob.glob(p))

    all_yamls = sorted(all_yamls)

    for yp in all_yamls:
        rel = os.path.relpath(yp, REPO)
        zero_codes = load_zero_product_codes(yp)
        all_pr = load_all_product_codes(yp)
        total_pr = len(all_pr)
        nonzero = {k for k, v in all_pr.items() if isinstance(v, (int, float)) and v != 0}
        complex_entries = {k for k, v in all_pr.items() if isinstance(v, dict)}
        print('  ' + rel)
        print('    total product_rates entries: ' + str(total_pr))
        print('    zero-rated (exemptions):     ' + str(len(zero_codes)))
        print('    nonzero simple rates:        ' + str(len(nonzero)))
        print('    complex (dict) entries:       ' + str(len(complex_entries)))
        print()

    print('=' * 70)
    print('STEP 5: Detailed differences')
    print('=' * 70)
    print()

    d1 = s122_yaml_codes - s122_txt_codes
    if d1:
        show_diff('Codes in S122 YAML but NOT in S122 TXT', d1)
    else:
        print('S122 YAML has NO codes missing from S122 TXT.')
        print()

    d2 = s122_txt_codes - s122_yaml_codes
    if d2:
        show_diff('Codes in S122 TXT but NOT in S122 YAML', d2)

    d3 = nov_yaml_codes - nov_txt_codes
    if d3:
        show_diff('Codes in Nov YAML but NOT in Nov TXT', d3)
    else:
        print('Nov YAML has NO codes missing from Nov TXT.')
        print()

    d4 = nov_txt_codes - nov_yaml_codes
    if d4:
        show_diff('Codes in Nov TXT but NOT in Nov YAML', d4)

    d5 = nov_yaml_codes - s122_yaml_codes
    show_diff('Codes in Nov YAML but NOT in S122 YAML', d5)

    d6 = s122_yaml_codes - nov_yaml_codes
    if d6:
        show_diff('Codes in S122 YAML but NOT in Nov YAML', d6)
    else:
        print('S122 YAML has NO codes that are missing from Nov YAML.')
        print()

    print('=' * 70)
    print('STEP 6: Code length distribution')
    print('=' * 70)
    print()

    for name, codes in [('S122_yaml', s122_yaml_codes), ('Nov_yaml', nov_yaml_codes),
                         ('S122_txt', s122_txt_codes), ('Nov_txt', nov_txt_codes)]:
        lengths = {}
        for c in codes:
            l = len(c)
            lengths[l] = lengths.get(l, 0) + 1
        print('  ' + name + ' (' + str(len(codes)) + ' total): ' + str(dict(sorted(lengths.items()))))

    print()
    print('Done.')

if __name__ == '__main__':
    main()
