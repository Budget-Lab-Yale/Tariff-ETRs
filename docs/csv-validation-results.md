# CSV Interface: All-Revision Validation Results

*2026-03-27 (updated end-of-day)*

## Summary

We ran the CSV export-load-compare loop across 5 key revisions spanning the full policy timeline (rev_10 through rev_32). Seven bugs were found and fixed across both repos during this session. The interface now achieves **99.6-100% exact match** on revisions through rev_18, with a remaining **2.2% gap on rev_32** caused by a known exporter issue with heading-only 232 programs.

## Current Results (after all fixes)

Tested at `|diff| < 0.001` (0.1pp) tolerance:

| Revision | Date | Matched | Exact % | Divergence | Max diff | Status |
|----------|------|---------|---------|------------|----------|--------|
| rev_10 | 2025-04-09 | 277,145 | 100.0% | -0.00pp | 0.0000 | PASS |
| rev_11 | 2025-04-11 | 280,629 | 99.8% | -0.01pp | 0.0124 | CLOSE |
| rev_14 | 2025-05-02 | 280,629 | 99.8% | -0.01pp | 0.0124 | CLOSE |
| rev_18 | 2025-08-07 | 272,108 | 99.6% | -0.06pp | 0.2276 | CLOSE |
| rev_32 | 2025-11-15 | 271,987 | 97.8% | +0.70pp | 0.2376 | FAIL |

For comparison, the starting point (before any fixes):

| Revision | Before | After |
|----------|--------|-------|
| rev_10 | 100.0%, max 0.0000 | 100.0%, max 0.0000 |
| rev_11 | 99.8%, max 0.1040 | 99.8%, max 0.0124 |
| rev_14 | 99.8%, max 0.2004 | 99.8%, max 0.0124 |
| rev_18 | 89.0%, max 0.4250 | 99.6%, max 0.2276 |
| rev_32 | 89.8%, max 0.4250 | 97.8%, max 0.2376 |

## Bugs Found and Fixed

### Fix 1: Generalized target_total floor — replace vs pmax (ETRs)

**File**: `Tariff-ETRs/src/calculations.R:1493`

**Bug**: The target_total floor logic for non-s232 authorities (IEEPA reciprocal, etc.) unconditionally replaced the rate with `max(target_total - MFN, 0)` instead of taking the maximum of the existing rate and the floor.

**Example**: Taiwan Phase 2 surcharge at 39%, floor at 15%, MFN at 20.8%:
- Before: `max(0.15 - 0.208, 0) = 0` — zeroed out a 39% tariff
- After: `max(0.39, max(0.15 - 0.208, 0)) = 0.39` — floor doesn't bind

### Fix 2: s232 target_total floor — replace vs pmax (ETRs)

**File**: `Tariff-ETRs/src/calculations.R:1515`

**Bug**: Same replace-instead-of-floor pattern, but for the s232-specific target_total path (auto deal rates).

**Example**: UK auto deal rate at 50%, target_total at 10%, MFN at 2.5%:
- Before: `max(0.10 - 0.025, 0) = 0.075` — replaced 50% with 7.5%
- After: `max(0.50, 0.075) = 0.50` — floor doesn't bind

### Fix 3: Exclude blanket chapter products from auto_products (Tracker)

**File**: `tariff-rate-tracker/src/06_calculate_rates.R:968`

**Bug**: The `auto_products` list included Ch72/73/76 products that matched auto_parts HTS prefixes (e.g., 7320 steel springs). These steel products incorrectly received the auto rebate (-1.24pp) and auto deal rate overrides.

**Fix**: Filter `auto_products` to exclude products in `STEEL_CHAPTERS` and `ALUM_CHAPTERS` after building the list from heading prefix matches.

### Fix 4: s232_usmca_eligible gate on blanket chapters (Tracker)

**File**: `tariff-rate-tracker/src/06_calculate_rates.R:1106,1159`

**Bug**: `s232_usmca_eligible` was set to TRUE for blanket chapter products (Ch73 steel) that matched auto_parts heading prefixes with `usmca_exempt = TRUE`. Even though the product's actual 232 rate came from the steel blanket (not the heading), it inherited the heading's USMCA eligibility. This caused the full USMCA content share reduction to be applied to steel products for CA/MX, reducing their 232 rate from 25% to ~1%.

**Fix**: Add `!(chapter %in% c(STEEL_CHAPTERS, ALUM_CHAPTERS))` to the `s232_usmca_eligible` condition, ensuring blanket chapter products never inherit heading-level USMCA eligibility.

### Fix 5: Fentanyl stacking — content-based split (Tracker + ETRs)

**Files**: `tariff-rate-tracker/src/helpers.R:924`, `Tariff-ETRs/src/calculations.R:1736`

**Bug**: For non-China countries with active Section 232, fentanyl (`rate_ieepa_fent`) was added at its full statutory rate instead of being scaled by `nonmetal_share`. This stacked fentanyl on top of 232 for products where 232 already covers the full value (auto_parts, copper, autos), violating the mutual-exclusion rule.

**Legal basis**: The copper proclamation (EO, July 2025) states the 232 duty applies "only to the copper content" and that non-copper content "remains subject to reciprocal tariffs and any other applicable duties, including the Canada IEEPA order." The White House fact sheet adds: "These tariffs do not stack." This establishes a content-based split, not full stacking.

**Fix**: Scale `rate_ieepa_fent * nonmetal_share` in the non-China 232 branch of both `apply_stacking_rules()` (tracker) and the ETRs stacking case_when. Also updated `compute_net_authority_contributions()` in tracker.

### Fix 6: IEEPA floor recomputation not exported (Tracker exporter)

**File**: `tariff-rate-tracker/src/generate_etrs_config.R:312`

**Bug**: The CSV exported `statutory_rate_ieepa_recip` (floor deducted against statutory MFN), but the tracker's `total_rate` uses `rate_ieepa_recip` (floor deducted against effective post-FTA MFN). For floor countries (EU, Japan, Korea, Switzerland) with FTA exemption shares, the effective MFN is lower, making the floor gap wider.

**Example**: Product with statutory MFN = 5%, exemption share = 80%, floor = 15%:
- CSV (before): `max(0.15 - 0.05, 0) = 0.10`
- Tracker total_rate: `max(0.15 - 0.01, 0) = 0.14`
- Diff: 4pp on this product

**Fix**: Export `rate_ieepa_recip` (post-recomputation) instead of `statutory_rate_ieepa_recip`. ETRs receives the same IEEPA rate the tracker uses.

### Fix 7: Test script function collisions (Test infrastructure)

**File**: `tariff-rate-tracker/scripts/test_all_revisions.R`

**Bug**: Both repos define `load_mfn_exemption_shares` and `load_metal_content` with incompatible signatures. When sourced into the same R session, ETRs' version overwrites the tracker's, breaking the tracker's no-arg calls.

**Fix**: Save/restore `load_mfn_exemption_shares` (tracker version compatible with both callers). Swap `load_metal_content` between tracker and ETRs versions at the appropriate points in the test loop.

## Remaining Gap: Heading-Only 232 Programs in Exporter (rev_32)

### Symptom

Rev_32 has 2.2% mismatches (5,888 products) at 0.1pp tolerance. The diff distribution shows discrete clusters at +0.113 (1,790 products), +0.0876 (1,364 products), and -0.0124 (529 products, auto rebate). Products are concentrated in HTS 8708 (auto parts, 1,544), 8471 (computers, 515), 8501 (motors, 501), 8703 (passenger vehicles, 325). Countries are spread broadly (Germany, Japan, UK, India, Taiwan, etc.).

### Root Cause

The exporter writes s232 heading rates to the CSV for ALL countries, but the tracker's rate calculation only applies heading rates to countries with deals or blanket rates.

**How it happens:**

1. The tracker's `calculate_rates_for_revision()` sets up heading rates (e.g., `autos_light_trucks` at 25%) and assigns them to all matched products. The `statutory_rate_232` is captured here.

2. For programs with "no blanket rate" (only country-specific deals), the heading rate is assigned to all countries at step 4, then deal overrides apply to specific countries at step 4c. Non-deal countries keep the heading rate.

3. The exporter's `export_statutory_rates()` uses `statutory_rate_232` and `classify_s232_program()` to build per-program columns. It writes the heading rate (0.25) for ALL countries, including those where the tracker's final `total_rate` may not include it.

4. ETRs loads the CSV and sees `s232_autos_light_trucks = 0.25` for all countries, applies the rate, and gets a higher final rate than the tracker.

**Why it only appears in rev_32**: Earlier revisions have fewer active heading programs. Rev_32 (November 2025) has more heading programs active (softwood, wood_furniture, kitchen_cabinets, buses, MHD) in addition to auto_parts/autos. The exporter writes rates for all of these, but the tracker may only apply some of them for certain country subsets.

### Impact

- +0.70pp import-weighted divergence on rev_32
- Max product-level diff of 0.2376 (= 0.25 - 0.0124, a rebated auto rate)
- Concentrated on auto/vehicle products and ITA-adjacent electronics

### Fix Options

1. **Export post-deal `rate_232` instead of `statutory_rate_232`**: The exporter would capture the tracker's final rate after deal overrides. Products with deals get the deal rate; products without deals get the heading rate (if a blanket rate exists) or 0 (if only deals). This requires checking whether `rate_232` is available at the right point in the pipeline (post-deals but pre-USMCA/pre-MFN-exemption).

2. **Export deal country masks**: Add columns or metadata indicating which countries have active deals for each heading program. ETRs would use these to decide whether to apply the heading rate. More complex but preserves the "statutory rate + ETRs applies adjustments" separation.

3. **Gate heading rates on blanket availability**: In the exporter, only write non-zero s232 heading rates when the blanket rate for that program is > 0. When there's no blanket rate (deals only), write 0 for non-deal countries and the deal rate for deal countries. This requires the exporter to know the blanket rate per heading program.

Option 1 is simplest but blurs the statutory/effective distinction. Option 3 is cleanest but requires threading deal-country information through the exporter.

## What's Validated

Across all tested revisions, the following are confirmed correct:

- **MFN loading, exemption shares, and USMCA reduction** (identical input files, aligned processing)
- **IEEPA reciprocal rates and Phase 2 surcharges** (including floor recomputation)
- **IEEPA fentanyl stacking** (China additive, non-China content-based split)
- **Section 232 steel/aluminum blanket rates** (country exemptions, metal content scaling)
- **Section 232 derivatives** (per-type metal shares, nonmetal IEEPA fill)
- **Section 301** (China-specific, correct across all revisions)
- **Section 122** (nonmetal_share scaling matches)
- **Target-total floor logic** (correct pmax semantics for both s232 and IEEPA)
- **USMCA product-level shares** (same file, same application)
- **Auto rebate** (correct amount, correctly gated on auto programs)
- **Country-level architecture** (240 countries, 8 partner aggregation)

## Commits

| Commit | Repo | Description |
|--------|------|-------------|
| `b2c09cf` | Tariff-ETRs | Fix target_total floor logic and fentanyl stacking (Fixes 1, 2, 5b) |
| `5745cca` | tariff-rate-tracker | Fix auto_products, USMCA eligibility, fentanyl stacking (Fixes 3, 4, 5a) |
| `1296d4c` | tariff-rate-tracker | Export post-recomputation IEEPA rate (Fix 6) |
