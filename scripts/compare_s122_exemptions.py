"""
Compare HTS exemption codes between our s122.yaml config and
the official SCOTUS Section 122 ANNEX 2 document.
"""

import re
import yaml
from collections import defaultdict

# ---- A) Extract HTS codes from the official document ----
official_codes = set()
with open('C:/Users/jar335/Downloads/2026Section122_ANNEX2_Final_extracted.txt', 'r', encoding='utf-8') as f:
    text = f.read()

# Match patterns like 0201.10.05, 8542.39, 2710.19 etc. (with dots)
# HTS codes appear as standalone patterns: XXXX.XX.XX or XXXX.XX
pattern = re.compile(r'\b(\d{4}\.\d{2}(?:\.\d{2})?)\b')
for match in pattern.finditer(text):
    code = match.group(1).replace('.', '')
    # Only keep codes that are 6 or 8 digits (valid HTS subheadings/tariff lines)
    if len(code) in (6, 8):
        official_codes.add(code)

print(f'=== OFFICIAL DOCUMENT ===')
print(f'Total unique codes extracted: {len(official_codes)}')

# Break down by digit length
official_6 = {c for c in official_codes if len(c) == 6}
official_8 = {c for c in official_codes if len(c) == 8}
print(f'  6-digit codes: {len(official_6)}')
print(f'  8-digit codes: {len(official_8)}')

# ---- B) Extract HTS codes from our s122.yaml ----
with open('C:/Users/jar335/Documents/Repositories/Tariff-ETRs/config/2-20_with_s122-preempt-temp/2026-02-20/s122.yaml', 'r') as f:
    config = yaml.safe_load(f)

our_codes = set()
for code_str in config.get('product_rates', {}).keys():
    clean = code_str.replace('.', '').replace("'", '').strip()
    our_codes.add(clean)

print(f'\n=== OUR CONFIG (s122.yaml) ===')
print(f'Total unique 10-digit codes: {len(our_codes)}')

# Verify all are 10-digit
non_10 = {c for c in our_codes if len(c) != 10}
if non_10:
    print(f'  WARNING: Non-10-digit codes found: {non_10}')

# ---- C) Truncate our 10-digit codes to 8 digits for comparison ----
our_8digit = {c[:8] for c in our_codes}
print(f'Unique 8-digit prefixes from our config: {len(our_8digit)}')

# Also get 6-digit prefixes
our_6digit = {c[:6] for c in our_codes}

# ---- D) Find differences ----

# D1: Official 8-digit codes that have NO matching 10-digit codes in our config
# For each 8-digit code in official, check if any of our 10-digit codes start with it
official_not_in_ours = set()
for oc in official_8:
    # Check if any of our 10-digit codes start with this 8-digit code
    has_match = any(c.startswith(oc) for c in our_codes)
    if not has_match:
        official_not_in_ours.add(oc)

# For 6-digit official codes, check if any of our 10-digit codes start with that 6-digit prefix
official_6_not_in_ours = set()
for oc in official_6:
    has_match = any(c.startswith(oc) for c in our_codes)
    if not has_match:
        official_6_not_in_ours.add(oc)

# D2: Our 10-digit codes whose first 8 digits don't appear in official 8-digit list
# Also handle case where official has 6-digit codes (our code's first 6 should match)
ours_not_in_official = set()
for c in our_codes:
    c8 = c[:8]
    c6 = c[:6]
    # Match if either the 8-digit prefix is in official_8 OR the 6-digit prefix is in official_6
    if c8 not in official_8 and c6 not in official_6:
        ours_not_in_official.add(c)

# ---- E) Report differences organized by chapter ----

def chapter_of(code):
    return code[:2]

def organize_by_chapter(codes):
    by_ch = defaultdict(list)
    for c in sorted(codes):
        by_ch[chapter_of(c)].append(c)
    return dict(sorted(by_ch.items()))

print('\n' + '='*80)
print('COMPARISON RESULTS')
print('='*80)

# Summary
print(f'\nOfficial 8-digit codes with NO match in our config: {len(official_not_in_ours)}')
print(f'Official 6-digit codes with NO match in our config: {len(official_6_not_in_ours)}')
print(f'Our 10-digit codes with NO match in official list: {len(ours_not_in_official)}')

# Matching stats
official_8_matched = official_8 - official_not_in_ours
official_6_matched = official_6 - official_6_not_in_ours
print(f'\nMatched official 8-digit codes: {len(official_8_matched)} / {len(official_8)}')
print(f'Matched official 6-digit codes: {len(official_6_matched)} / {len(official_6)}')
our_matched = our_codes - ours_not_in_official
print(f'Matched our 10-digit codes: {len(our_matched)} / {len(our_codes)}')

# Detail: Official codes missing from our config
print('\n' + '-'*80)
print('MISSING FROM OUR CONFIG (in official but not in ours)')
print('-'*80)

if official_not_in_ours:
    by_ch = organize_by_chapter(official_not_in_ours)
    for ch, codes in by_ch.items():
        print(f'\n  Chapter {ch} ({len(codes)} codes):')
        for c in codes:
            print(f'    {c}')

if official_6_not_in_ours:
    print('\n  [6-digit codes from official with no match]:')
    by_ch = organize_by_chapter(official_6_not_in_ours)
    for ch, codes in by_ch.items():
        print(f'\n  Chapter {ch} ({len(codes)} codes):')
        for c in codes:
            print(f'    {c}')

if not official_not_in_ours and not official_6_not_in_ours:
    print('  (none - all official codes are covered)')

# Detail: Our codes not in official
print('\n' + '-'*80)
print('EXTRA IN OUR CONFIG (in ours but not in official)')
print('-'*80)

if ours_not_in_official:
    by_ch = organize_by_chapter(ours_not_in_official)
    for ch, codes in by_ch.items():
        print(f'\n  Chapter {ch} ({len(codes)} codes):')
        for c in codes:
            print(f'    {c}')
else:
    print('  (none - all our codes match official codes)')

# ---- Additional: Chapter-level summary ----
print('\n' + '-'*80)
print('CHAPTER-LEVEL SUMMARY')
print('-'*80)

all_chapters = sorted(set(
    [chapter_of(c) for c in official_codes] +
    [chapter_of(c) for c in our_codes]
))

print(f'\n{"Ch":>4} | {"Official 8d":>12} | {"Our 8d pfx":>12} | {"Missing":>8} | {"Extra":>8}')
print(f'{"-"*4:>4}-+-{"-"*12:>12}-+-{"-"*12:>12}-+-{"-"*8:>8}-+-{"-"*8:>8}')

for ch in all_chapters:
    off_ch = len([c for c in official_8 if c[:2] == ch])
    off_6_ch = len([c for c in official_6 if c[:2] == ch])
    our_ch = len([c for c in our_8digit if c[:2] == ch])
    miss_ch = len([c for c in official_not_in_ours if c[:2] == ch])
    miss_6_ch = len([c for c in official_6_not_in_ours if c[:2] == ch])
    extra_ch = len([c for c in ours_not_in_official if c[:2] == ch])
    off_str = str(off_ch)
    if off_6_ch > 0:
        off_str += f'(+{off_6_ch}x6d)'
    miss_str = str(miss_ch)
    if miss_6_ch > 0:
        miss_str += f'(+{miss_6_ch}x6d)'
    print(f'{ch:>4} | {off_str:>12} | {our_ch:>12} | {miss_str:>8} | {extra_ch:>8}')
