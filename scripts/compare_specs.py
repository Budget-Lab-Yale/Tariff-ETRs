"""Compare CBO and Budget Lab tariff specifications.

Joins the two tariff specs on (HTS10, country_code) and identifies differences.
CBO rates already in percentage points; BL rates converted to percentage points.
"""

import pandas as pd
import numpy as np

# Load specs
print('Loading tariff specifications...')
cbo = pd.read_csv('scripts/cbo_tariff_spec.csv', dtype={'CTY_CODE': str, 'I_COMMODITY': str})
bl = pd.read_csv('scripts/bl_tariff_spec.csv', dtype={'CTY_CODE': str, 'I_COMMODITY': str})

print(f'CBO spec: {cbo.shape[0]:,} rows, {cbo["CTY_CODE"].nunique()} countries, {cbo["I_COMMODITY"].nunique()} HTS codes')
print(f'BL spec:  {bl.shape[0]:,} rows, {bl["CTY_CODE"].nunique()} countries, {bl["I_COMMODITY"].nunique()} HTS codes')

# Rename columns for merge
cbo = cbo.rename(columns={'new_rate': 'cbo_rate'})
bl = bl.rename(columns={'new_rate': 'bl_rate'})

# Full outer join
merged = cbo.merge(bl, on=['I_COMMODITY', 'CTY_CODE'], how='outer', indicator=True)
print(f'\nMerged: {merged.shape[0]:,} rows')
print(f'  Both:     {(merged["_merge"] == "both").sum():,}')
print(f'  CBO only: {(merged["_merge"] == "left_only").sum():,}')
print(f'  BL only:  {(merged["_merge"] == "right_only").sum():,}')

# Fill NAs for comparison
merged['cbo_rate'] = merged['cbo_rate'].fillna(0)
merged['bl_rate'] = merged['bl_rate'].fillna(0)
merged['imports'] = merged['imports'].fillna(0)

# Calculate difference
merged['diff'] = merged['bl_rate'] - merged['cbo_rate']
merged['abs_diff'] = merged['diff'].abs()

# ============================================================================
# Overall comparison
# ============================================================================
print('\n' + '='*80)
print('OVERALL COMPARISON')
print('='*80)

# Focus on matched rows
both = merged[merged['_merge'] == 'both'].copy()
print(f'\nMatched rows: {len(both):,}')
print(f'Exact matches (diff = 0): {(both["abs_diff"] == 0).sum():,} ({(both["abs_diff"] == 0).mean()*100:.1f}%)')
print(f'Close matches (diff < 0.1pp): {(both["abs_diff"] < 0.1).sum():,} ({(both["abs_diff"] < 0.1).mean()*100:.1f}%)')
print(f'Moderate diff (0.1-5pp): {((both["abs_diff"] >= 0.1) & (both["abs_diff"] < 5)).sum():,}')
print(f'Large diff (5-20pp): {((both["abs_diff"] >= 5) & (both["abs_diff"] < 20)).sum():,}')
print(f'Very large diff (>20pp): {(both["abs_diff"] >= 20).sum():,}')

# ============================================================================
# Key country names for readability
# ============================================================================
country_names = {
    '5700': 'China', '1220': 'Canada', '2010': 'Mexico',
    '4120': 'UK', '5880': 'Japan', '3510': 'Brazil',
    '4280': 'Germany', '4279': 'France', '4700': 'Finland',
    '5800': 'S. Korea', '4621': 'Russia', '1221': 'USMCA-CA',
    '2011': 'USMCA-MX', '6021': 'Australia', '5590': 'Singapore'
}

# ============================================================================
# Country-level comparison (import-weighted)
# ============================================================================
print('\n' + '='*80)
print('COUNTRY-LEVEL COMPARISON (import-weighted average rates)')
print('='*80)

country_summary = both.groupby('CTY_CODE').agg(
    total_imports=('imports', 'sum'),
    cbo_avg=('cbo_rate', lambda x: np.average(x, weights=both.loc[x.index, 'imports']) if both.loc[x.index, 'imports'].sum() > 0 else x.mean()),
    bl_avg=('bl_rate', lambda x: np.average(x, weights=both.loc[x.index, 'imports']) if both.loc[x.index, 'imports'].sum() > 0 else x.mean()),
    n_exact_match=('abs_diff', lambda x: (x == 0).sum()),
    n_products=('I_COMMODITY', 'nunique'),
    n_diff=('abs_diff', lambda x: (x > 0.01).sum()),
    avg_abs_diff=('abs_diff', 'mean'),
    max_abs_diff=('abs_diff', 'max'),
).reset_index()
country_summary['wt_diff'] = country_summary['bl_avg'] - country_summary['cbo_avg']
country_summary['name'] = country_summary['CTY_CODE'].map(country_names).fillna('')

# Show key countries
print('\nKey countries:')
key_codes = ['5700', '1220', '2010', '4120', '5880', '3510', '4280', '5800', '6021', '4621', '1221', '2011']
key_summary = country_summary[country_summary['CTY_CODE'].isin(key_codes)].copy()
key_summary = key_summary.sort_values('total_imports', ascending=False)

header = f'{"Code":<6} {"Name":<10} {"Imports($M)":>12} {"CBO avg":>8} {"BL avg":>8} {"Diff":>8} {"#Exact":>7} {"#Diff":>6} {"MaxDiff":>8}'
print(header)
print('-' * len(header))
for _, row in key_summary.iterrows():
    print(f'{row["CTY_CODE"]:<6} {row["name"]:<10} {row["total_imports"]/1e6:>12,.0f} {row["cbo_avg"]:>8.2f} {row["bl_avg"]:>8.2f} {row["wt_diff"]:>+8.2f} {row["n_exact_match"]:>7,} {row["n_diff"]:>6,} {row["max_abs_diff"]:>8.2f}')

# ============================================================================
# Top countries by absolute weighted difference
# ============================================================================
print('\n' + '='*80)
print('TOP 20 COUNTRIES BY ABSOLUTE WEIGHTED DIFFERENCE (import-weighted)')
print('='*80)

top_diff = country_summary.sort_values('wt_diff', key=abs, ascending=False).head(20)
print(f'\n{"Code":<6} {"Name":<10} {"Imports($M)":>12} {"CBO avg":>8} {"BL avg":>8} {"Diff":>8} {"#Diff":>6}')
print('-' * 70)
for _, row in top_diff.iterrows():
    name = row['name'] if row['name'] else row['CTY_CODE']
    print(f'{row["CTY_CODE"]:<6} {name:<10} {row["total_imports"]/1e6:>12,.0f} {row["cbo_avg"]:>8.2f} {row["bl_avg"]:>8.2f} {row["wt_diff"]:>+8.2f} {row["n_diff"]:>6,}')

# ============================================================================
# Breakdown of differences by type
# ============================================================================
print('\n' + '='*80)
print('WHERE DO THE DIFFERENCES COME FROM?')
print('='*80)

# USMCA synthetic codes (1221, 2011) - CBO has them, BL doesn't
usmca_synth = merged[merged['CTY_CODE'].isin(['1221', '2011'])]
print(f'\n1. USMCA synthetic codes (1221/2011):')
print(f'   CBO has {len(usmca_synth):,} rows for synthetic USMCA codes')
print(f'   BL does not split Canada/Mexico into USMCA vs non-USMCA')

# China: compare rates
china = both[both['CTY_CODE'] == '5700'].copy()
print(f'\n2. China (5700) comparison:')
print(f'   Total products: {len(china):,}')
print(f'   CBO avg rate: {china["cbo_rate"].mean():.2f}%')
print(f'   BL avg rate: {china["bl_rate"].mean():.2f}%')
print(f'   Exact match: {(china["abs_diff"] == 0).sum():,}')
print(f'   Differ: {(china["abs_diff"] > 0.01).sum():,}')

# China rate distribution
print(f'\n   CBO China rate distribution:')
cbo_china_rates = china['cbo_rate'].value_counts().sort_index()
for rate, count in cbo_china_rates.items():
    if count > 100:
        print(f'     {rate:>6.1f}%: {count:>6,} products')

print(f'\n   BL China rate distribution:')
bl_china_rates = china['bl_rate'].value_counts().sort_index()
for rate, count in bl_china_rates.items():
    if count > 100:
        print(f'     {rate:>6.1f}%: {count:>6,} products')

# Canada comparison
canada = both[both['CTY_CODE'] == '1220'].copy()
print(f'\n3. Canada (1220) comparison:')
print(f'   Total products: {len(canada):,}')
print(f'   CBO avg rate: {canada["cbo_rate"].mean():.2f}%')
print(f'   BL avg rate: {canada["bl_rate"].mean():.2f}%')
print(f'   Products where both zero: {((canada["cbo_rate"]==0) & (canada["bl_rate"]==0)).sum():,}')
print(f'   Products where CBO=0, BL>0: {((canada["cbo_rate"]==0) & (canada["bl_rate"]>0)).sum():,}')
print(f'   Products where CBO>0, BL=0: {((canada["cbo_rate"]>0) & (canada["bl_rate"]==0)).sum():,}')
print(f'   Products where both>0: {((canada["cbo_rate"]>0) & (canada["bl_rate"]>0)).sum():,}')

# Mexico comparison
mexico = both[both['CTY_CODE'] == '2010'].copy()
print(f'\n4. Mexico (2010) comparison:')
print(f'   Total products: {len(mexico):,}')
print(f'   CBO avg rate: {mexico["cbo_rate"].mean():.2f}%')
print(f'   BL avg rate: {mexico["bl_rate"].mean():.2f}%')

# EU (using Germany as representative)
germany = both[both['CTY_CODE'] == '4280'].copy()
print(f'\n5. Germany/EU (4280) comparison:')
print(f'   Total products: {len(germany):,}')
print(f'   CBO avg rate: {germany["cbo_rate"].mean():.2f}%')
print(f'   BL avg rate: {germany["bl_rate"].mean():.2f}%')
print(f'   Exact match: {(germany["abs_diff"] == 0).sum():,}')
print(f'   Differ: {(germany["abs_diff"] > 0.01).sum():,}')

# Show EU rate distribution
print(f'\n   CBO Germany rate distribution:')
cbo_de_rates = germany['cbo_rate'].value_counts().sort_index()
for rate, count in cbo_de_rates.items():
    if count > 50:
        print(f'     {rate:>6.1f}%: {count:>6,} products')

print(f'\n   BL Germany rate distribution:')
bl_de_rates = germany['bl_rate'].value_counts().sort_index()
for rate, count in bl_de_rates.items():
    if count > 50:
        print(f'     {rate:>6.1f}%: {count:>6,} products')

# Japan comparison
japan = both[both['CTY_CODE'] == '5880'].copy()
print(f'\n6. Japan (5880) comparison:')
print(f'   Total products: {len(japan):,}')
print(f'   CBO avg rate: {japan["cbo_rate"].mean():.2f}%')
print(f'   BL avg rate: {japan["bl_rate"].mean():.2f}%')
print(f'   Exact match: {(japan["abs_diff"] == 0).sum():,}')
print(f'   Differ: {(japan["abs_diff"] > 0.01).sum():,}')

# Brazil comparison
brazil = both[both['CTY_CODE'] == '3510'].copy()
print(f'\n7. Brazil (3510) comparison:')
print(f'   Total products: {len(brazil):,}')
print(f'   CBO avg rate: {brazil["cbo_rate"].mean():.2f}%')
print(f'   BL avg rate: {brazil["bl_rate"].mean():.2f}%')
print(f'   Exact match: {(brazil["abs_diff"] == 0).sum():,}')
print(f'   Differ: {(brazil["abs_diff"] > 0.01).sum():,}')

# UK comparison
uk = both[both['CTY_CODE'] == '4120'].copy()
print(f'\n8. UK (4120) comparison:')
print(f'   Total products: {len(uk):,}')
print(f'   CBO avg rate: {uk["cbo_rate"].mean():.2f}%')
print(f'   BL avg rate: {uk["bl_rate"].mean():.2f}%')
print(f'   Exact match: {(uk["abs_diff"] == 0).sum():,}')

# ============================================================================
# Product-level deep dive: biggest differences
# ============================================================================
print('\n' + '='*80)
print('LARGEST PRODUCT-LEVEL DIFFERENCES (by import value × rate diff)')
print('='*80)

both['impact'] = both['imports'] * both['abs_diff'] / 100
top_product_diffs = both.nlargest(30, 'impact')

print(f'\n{"HTS10":<12} {"Country":<8} {"CBO":>6} {"BL":>6} {"Diff":>7} {"Imports($M)":>12} {"Impact($M)":>11}')
print('-' * 70)
for _, row in top_product_diffs.iterrows():
    name = country_names.get(row['CTY_CODE'], row['CTY_CODE'])
    print(f'{row["I_COMMODITY"]:<12} {name:<8} {row["cbo_rate"]:>6.1f} {row["bl_rate"]:>6.1f} {row["diff"]:>+7.1f} {row["imports"]/1e6:>12,.1f} {row["impact"]/1e6:>11,.1f}')

# ============================================================================
# HTS chapter analysis
# ============================================================================
print('\n' + '='*80)
print('DIFFERENCES BY 2-DIGIT HTS CHAPTER (import-weighted)')
print('='*80)

both['hts2'] = both['I_COMMODITY'].str[:2]
chapter_summary = both.groupby('hts2').agg(
    total_imports=('imports', 'sum'),
    cbo_wt_rate=('cbo_rate', lambda x: np.average(x, weights=both.loc[x.index, 'imports']) if both.loc[x.index, 'imports'].sum() > 0 else 0),
    bl_wt_rate=('bl_rate', lambda x: np.average(x, weights=both.loc[x.index, 'imports']) if both.loc[x.index, 'imports'].sum() > 0 else 0),
    n_diff=('abs_diff', lambda x: (x > 0.01).sum()),
).reset_index()
chapter_summary['diff'] = chapter_summary['bl_wt_rate'] - chapter_summary['cbo_wt_rate']

# Show chapters with biggest differences
big_chapter_diffs = chapter_summary[chapter_summary['diff'].abs() > 0.5].sort_values('diff', key=abs, ascending=False)
print(f'\nChapters with weighted avg rate diff > 0.5pp:')
print(f'{"Ch":<4} {"Imports($B)":>12} {"CBO wt":>8} {"BL wt":>8} {"Diff":>8} {"#Diff":>6}')
print('-' * 50)
for _, row in big_chapter_diffs.iterrows():
    print(f'{row["hts2"]:<4} {row["total_imports"]/1e9:>12,.1f} {row["cbo_wt_rate"]:>8.2f} {row["bl_wt_rate"]:>8.2f} {row["diff"]:>+8.2f} {row["n_diff"]:>6,}')

# ============================================================================
# Systematic pattern detection
# ============================================================================
print('\n' + '='*80)
print('SYSTEMATIC PATTERN ANALYSIS')
print('='*80)

# Check for consistent rate offsets
print('\n1. Products where CBO > BL (CBO has higher rate):')
cbo_higher = both[both['diff'] < -0.01]
print(f'   Count: {len(cbo_higher):,} product-country pairs')
if len(cbo_higher) > 0:
    print(f'   Avg diff: {cbo_higher["diff"].mean():.2f}pp')
    print(f'   Total import value: ${cbo_higher["imports"].sum()/1e9:.1f}B')
    # By country
    cbo_higher_cty = cbo_higher.groupby('CTY_CODE').agg(
        count=('diff', 'size'), avg_diff=('diff', 'mean'), imports=('imports', 'sum')
    ).sort_values('imports', ascending=False).head(10)
    print(f'   Top countries:')
    for cty, row in cbo_higher_cty.iterrows():
        name = country_names.get(cty, cty)
        print(f'     {name:<10}: {row["count"]:>6,} products, avg diff {row["avg_diff"]:+.2f}pp, ${row["imports"]/1e6:,.0f}M imports')

print(f'\n2. Products where BL > CBO (BL has higher rate):')
bl_higher = both[both['diff'] > 0.01]
print(f'   Count: {len(bl_higher):,} product-country pairs')
if len(bl_higher) > 0:
    print(f'   Avg diff: {bl_higher["diff"].mean():.2f}pp')
    print(f'   Total import value: ${bl_higher["imports"].sum()/1e9:.1f}B')
    # By country
    bl_higher_cty = bl_higher.groupby('CTY_CODE').agg(
        count=('diff', 'size'), avg_diff=('diff', 'mean'), imports=('imports', 'sum')
    ).sort_values('imports', ascending=False).head(10)
    print(f'   Top countries:')
    for cty, row in bl_higher_cty.iterrows():
        name = country_names.get(cty, cty)
        print(f'     {name:<10}: {row["count"]:>6,} products, avg diff {row["avg_diff"]:+.2f}pp, ${row["imports"]/1e6:,.0f}M imports')

# Check 232 product overlap
print(f'\n3. Products at 50% (steel/aluminum rate) - check coverage overlap:')
cbo_at_50 = both[both['cbo_rate'] == 50]
bl_at_50 = both[both['bl_rate'] == 50]
print(f'   CBO products at 50%: {len(cbo_at_50):,}')
print(f'   BL products at 50%:  {len(bl_at_50):,}')
both_50 = both[(both['cbo_rate'] == 50) & (both['bl_rate'] == 50)]
print(f'   Both at 50%:         {len(both_50):,}')
cbo_50_not_bl = both[(both['cbo_rate'] == 50) & (both['bl_rate'] != 50)]
bl_50_not_cbo = both[(both['bl_rate'] == 50) & (both['cbo_rate'] != 50)]
print(f'   CBO=50% but BL≠50%: {len(cbo_50_not_bl):,}')
print(f'   BL=50% but CBO≠50%: {len(bl_50_not_cbo):,}')

# Check 25% products (autos)
print(f'\n4. Products at 25% (auto rate) comparison:')
cbo_at_25 = both[both['cbo_rate'] == 25]
bl_at_25 = both[both['bl_rate'] == 25]
print(f'   CBO products at 25%: {len(cbo_at_25):,}')
print(f'   BL products at 25%:  {len(bl_at_25):,}')

# ============================================================================
# Export detailed diff for inspection
# ============================================================================
diffs = both[both['abs_diff'] > 0.01].sort_values('impact', ascending=False)
diffs.to_csv('scripts/tariff_spec_differences.csv', index=False)
print(f'\nExported {len(diffs):,} differing product-country pairs to scripts/tariff_spec_differences.csv')

# Summary
print('\n' + '='*80)
print('EXECUTIVE SUMMARY')
print('='*80)
total_imports = both['imports'].sum()
matched_imports = both[both['abs_diff'] < 0.01]['imports'].sum()
print(f'\nTotal matched import value: ${total_imports/1e9:.1f}B')
print(f'Import value where rates agree (within 0.01pp): ${matched_imports/1e9:.1f}B ({matched_imports/total_imports*100:.1f}%)')
print(f'Import value where rates differ: ${(total_imports-matched_imports)/1e9:.1f}B ({(1-matched_imports/total_imports)*100:.1f}%)')

# Import-weighted overall rates
cbo_overall = np.average(both['cbo_rate'], weights=both['imports'])
bl_overall = np.average(both['bl_rate'], weights=both['imports'])
print(f'\nImport-weighted overall tariff rate:')
print(f'  CBO: {cbo_overall:.2f}%')
print(f'  BL:  {bl_overall:.2f}%')
print(f'  Gap: {bl_overall - cbo_overall:+.2f}pp')
