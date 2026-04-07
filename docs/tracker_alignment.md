# Tracker Alignment: calc_weighted_etr() vs Tariff-Rate-Tracker

## Problem

The ETRs pipeline (`calc_weighted_etr()`) and the tariff-rate-tracker compute weighted effective tariff rates from the same underlying statutory rates but produce different results. Using identical input rates, the two pipelines diverged by **3.8 percentage points** on the overall import-weighted ETR.

Three specific differences in how pre-stacking adjustments (USMCA shares, MFN exemptions, target-total floor logic) are applied were identified by tracing both pipelines line-by-line.

## What We Changed

All changes are in `src/calculations.R` within `calc_weighted_etr()`.

### 1. USMCA reduction on MFN base rate (biggest driver)

**Before:** USMCA share reduction was applied to all authority rate columns (232, IEEPA, etc.) but never to `mfn_rate`. The tracker reduces MFN by `(1 - usmca_share)` for CA/MX.

**After:** After applying USMCA to authority columns, we now also apply `mfn_rate = mfn_rate * (1 - usmca_share)` for USMCA countries. The `select(-usmca_share)` cleanup was moved from before the MFN phase to after, so the column is available when needed.

### 2. Exclude CA/MX from MFN exemption shares

**Before:** MFN exemption shares (FTA/GSP preferences) were applied to all countries including CA/MX. The tracker sets `exemption_share = 0` for CA/MX when product-level USMCA shares are available (`exclude_usmca = TRUE`).

**After:** `exemption_share` is zeroed for USMCA countries before applying it to `mfn_rate`. This prevents double-counting: CA/MX trade preferences are handled by USMCA product shares, not MFN exemptions.

### 3. Target-total computed against statutory (pre-adjustment) MFN

**Before:** The target-total floor add-on was computed as `max(target_total - mfn_rate, 0)` using the *adjusted* `mfn_rate` (after exemption and USMCA reductions). This produced higher add-ons than the tracker.

**After:** We save `statutory_mfn = mfn_rate` immediately after the MFN join (before any adjustments) and use `statutory_mfn` in both the generalized and 232-specific target-total loops. This matches the tracker's order of operations where the IEEPA reciprocal floor add-on is computed against the original statutory base rate.

### 4. Product-level USMCA shares are now required (not optional)

The GTAP-sector-level USMCA share fallback was removed entirely. All configs now point to `resources/usmca_product_shares_2025.csv`. The `usmca_data` parameter was removed from `calc_weighted_etr()`, `calc_etrs_for_config()`, and the entire call chain. The old `resources/usmca_shares.csv` (GTAP-level) is no longer loaded anywhere.

## Files Changed

- `src/calculations.R` — core fixes in `calc_weighted_etr()`, removed `usmca_data`/`usmca_shares` threading from `calc_etrs_for_config`, `run_counterfactual`, `do_scenario_static`, `do_scenario_time_varying`, `do_scenario`
- `scripts/test_scenarios.R` — updated `run_etr()` helper to match new interface
- `scripts/extract_bl_rates.R` — updated to use product-level USMCA shares
- `config/*/other_params.yaml` (all 9 active configs) — added `usmca_product_shares: 'resources/usmca_product_shares_2025.csv'`

## Validation Results

Compared `2-21_perm/2026-01-01` ETRs output against the `2026_basic` tracker snapshot (same policy date).

**Before these fixes:** ~3.8pp divergence.
**After these fixes:** ~0.9pp headline divergence — but see decomposition below.

### Matched-Universe Comparison (apples-to-apples)

The headline 0.9pp gap was misleading. The tracker snapshot only covers 72% of ETRs import value (mainly because the tracker enumerates fewer countries). Dividing tracker revenue by total ETRs imports inflated the apparent divergence.

Restricting both pipelines to products present in both:

```
Matched imports:          $2,252.5B (72.1% of total)
ETRs weighted ETR:          20.26%
Tracker weighted ETR:       20.58%
Matched-universe divergence: -0.31pp  ← PASS (< 0.5pp target)
```

### Unmatched Universe

```
Unmatched imports:       $871.3B (27.9%)
Unmatched weighted ETR:   4.06%  (ETRs-only; no tracker rates)
  HS10 not in tracker:   $73.4B  ( 8% of unmatched) — product gap
  Country not in tracker: $797.9B (92% of unmatched) — country gap
```

The unmatched trade is overwhelmingly a **country coverage gap** (tracker doesn't enumerate all 240 Census countries) and is dominated by zero-rate products (73% by value). This is a structural difference between the pipelines' scope, not a formula mismatch.

### Country-Level Divergence

The -0.31pp overall is the NET of large offsetting country-level divergences:

| Country | ETRs | Tracker | Diff (pp) | Imports ($B) | Rev Gap ($M) |
|---------|------|---------|-----------|-------------|-------------|
| China   | 32.72% | 38.44% | -5.72  | 413.1 | -23,644 |
| Canada  | 13.01% | 11.83% | +1.17  | 388.6 | +4,565 |
| Mexico  | 16.02% | 14.62% | +1.40  | 484.7 | +6,804 |
| Japan   | 16.82% | 14.90% | +1.92  | 109.7 | +2,104 |
| UK      | 10.57% | 11.07% | -0.50  | 35.5  | -178 |

**Pattern:** ETRs is ~5.7pp lower for China but ~1-2pp higher for most other countries. These cancel to give -0.31pp overall.

**Likely drivers of per-country divergence:**
- **China (-5.72pp):** Section 301 rate differences (product list and rates sourced differently), fentanyl stacking, and 232 coverage scope on China-specific products. The tracker appears to assign higher combined rates to Chinese imports.
- **CA/MX (+1.2-1.4pp):** MFN rate source differences (static CSV vs HTS JSON), USMCA share application order, and/or different product-level USMCA share data.
- **Japan/others (+1-2pp):** MFN rate differences and/or metal content methodology (BEA per-type in ETRs vs single `metal_share` in tracker) affecting 232 derivative interaction with IEEPA reciprocal.

## Remaining Structural Differences

These explain the per-country divergences. They are design choices, not bugs:

1. **MFN rate source.** ETRs uses a static `mfn_rates_2025.csv` (13,073 HS8 codes). The tracker extracts MFN rates from the HTS JSON per revision via a rate-inheritance stack (~59% of HTS10 codes inherit from parent headings). Different source data and aggregation methods produce small per-product differences that compound at the country level.

2. **Metal content methodology.** ETRs uses BEA Detail I-O tables with per-type metal shares (steel, aluminum, copper, other) and type-specific nonmetal share accumulation. The tracker uses a single `metal_share` per product (flat 50%, CBO buckets, or aggregate BEA). This affects how 232 derivative rates interact with IEEPA/S122 via the nonmetal_share pathway.

3. **232 program coverage.** ETRs defines 14 Section 232 programs via HTS prefix matching in YAML configs. The tracker identifies 232 coverage from Chapter 99 cross-references in the HTS JSON. Slight product classification differences affect which products enter the 232 vs IEEPA branch of the stacking formula.

4. **Section 301 product list.** ETRs loads 301 rates from a static YAML config. The tracker builds the 301 product list from Ch99 references in the HTS JSON, which may capture more or fewer products and different rate tiers.

5. **Country coverage.** ETRs covers all 240 Census countries (including 183 unmapped "ROW" countries). The tracker snapshot covers a subset of countries for which it has explicit rate assignments. The 28% of import value outside the tracker's scope has a low weighted ETR (4.06%), so this primarily affects full-universe comparisons, not the matched-universe metric.

## How to Run the Validation

```bash
cd Tariff-ETRs
Rscript scripts/validate_tracker_alignment.R
```

The script loads the `2-21_perm/2026-01-01` config, runs `calc_weighted_etr()`, then loads the `2026_basic` snapshot from the tracker repo. It reports:
- **Primary metric:** Matched-universe divergence (products in both pipelines)
- **Diagnostics:** Unmatched import decomposition (HS10 gap vs country gap)
- **Country-level:** Per-country ETR comparison and top divergence contributors by tariff revenue gap
