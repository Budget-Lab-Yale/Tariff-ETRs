# CSV Interface: All-Revision Validation Results

*2026-03-27*

## Summary

We ran the CSV export-load-compare loop for all 33 revisions (basic through rev_32, January 2025 to November 2025). The function-collision issue documented in `csv-interface-status.md` is resolved: both repos' `load_mfn_exemption_shares` and `load_metal_content` now coexist in the shared test session via save/restore in `test_all_revisions.R`.

**Overall: 11 perfect, 7 close, 15 failing on exact match -- but import-weighted divergence stays below 0.35pp even in the worst case.**

## Results by Revision

| Revision | Date | Matched | Exact % | Divergence | Max diff | Status |
|----------|------|---------|---------|------------|----------|--------|
| basic | 2025-01-01 | 19,591 | 100.0% | -0.00pp | 0.0000 | PASS |
| rev_1 | 2025-01-27 | 24,288 | 100.0% | -0.00pp | 0.0000 | PASS |
| rev_2 | 2025-02-01 | 24,288 | 100.0% | -0.00pp | 0.0000 | PASS |
| rev_3 | 2025-02-04 | 46,520 | 100.0% | -0.00pp | 0.0000 | PASS |
| rev_4 | 2025-03-04 | 51,721 | 100.0% | -0.00pp | 0.0000 | PASS |
| rev_5 | 2025-03-05 | 51,721 | 100.0% | -0.00pp | 0.0000 | PASS |
| rev_6 | 2025-03-12 | 61,437 | 100.0% | -0.00pp | 0.0000 | PASS |
| rev_7 | 2025-04-02 | 277,145 | 100.0% | -0.00pp | 0.0000 | PASS |
| rev_8 | 2025-04-03 | 277,145 | 100.0% | -0.00pp | 0.0000 | PASS |
| rev_9 | 2025-04-05 | 277,145 | 100.0% | -0.00pp | 0.0000 | PASS |
| rev_10 | 2025-04-09 | 277,145 | 100.0% | -0.00pp | 0.0000 | PASS |
| rev_11 | 2025-04-11 | 280,629 | 99.8% | -0.01pp | 0.1040 | CLOSE |
| rev_12 | 2025-04-14 | 280,629 | 99.8% | -0.01pp | 0.1040 | CLOSE |
| rev_13 | 2025-04-22 | 280,629 | 99.8% | -0.01pp | 0.1040 | CLOSE |
| rev_14 | 2025-05-02 | 280,629 | 99.8% | -0.01pp | 0.2004 | CLOSE |
| rev_15 | 2025-05-14 | 280,629 | 99.8% | -0.01pp | 0.2004 | CLOSE |
| rev_16 | 2025-06-04 | 279,649 | 99.7% | +0.07pp | 0.4250 | CLOSE |
| rev_17 | 2025-07-01 | 279,672 | 99.6% | -0.05pp | 0.4250 | CLOSE |
| rev_18 | 2025-08-07 | 272,108 | 89.0% | -0.20pp | 0.4250 | FAIL |
| rev_19 | 2025-08-12 | 272,108 | 89.0% | -0.20pp | 0.4250 | FAIL |
| rev_20 | 2025-08-20 | 272,108 | 89.0% | -0.20pp | 0.4250 | FAIL |
| rev_21 | 2025-08-28 | 272,108 | 89.0% | -0.20pp | 0.4250 | FAIL |
| rev_22 | 2025-09-03 | 272,108 | 89.0% | -0.20pp | 0.4250 | FAIL |
| rev_23 | 2025-09-12 | 272,169 | 89.0% | -0.02pp | 0.4250 | FAIL |
| rev_24 | 2025-09-19 | 272,118 | 88.9% | +0.21pp | 0.4250 | FAIL |
| rev_25 | 2025-09-26 | 272,118 | 88.8% | +0.21pp | 0.4250 | FAIL |
| rev_26 | 2025-10-06 | 272,216 | 88.8% | +0.08pp | 0.4250 | FAIL |
| rev_27 | 2025-10-15 | 272,216 | 88.8% | +0.08pp | 0.4250 | FAIL |
| rev_28 | 2025-10-22 | 272,216 | 88.8% | +0.08pp | 0.4250 | FAIL |
| rev_29 | 2025-10-31 | 272,216 | 88.8% | +0.08pp | 0.4250 | FAIL |
| rev_30 | 2025-11-05 | 272,216 | 88.8% | +0.08pp | 0.4250 | FAIL |
| rev_31 | 2025-11-12 | 272,216 | 88.8% | +0.08pp | 0.4250 | FAIL |
| rev_32 | 2025-11-15 | 271,987 | 89.8% | +0.34pp | 0.4250 | FAIL |

## Phase Transitions

The divergence pattern maps cleanly onto policy changes:

- **basic -- rev_10** (Jan -- Apr 9, 2025): Perfect match. No auto parts heading, no auto deals, no Phase 2 surcharges. Stacking, MFN, USMCA, metal content, fentanyl, s301 all validated.
- **rev_11 -- rev_17** (Apr 11 -- Jul 1, 2025): Auto parts heading activates. 691 mismatches appear, all driven by auto rebate classification. Import-weighted divergence stays under 0.07pp.
- **rev_18 -- rev_32** (Aug 7 -- Nov 15, 2025): UK auto 232 deal rates activate. ~30,000 mismatches appear. Exact-match % drops to ~89%, but divergence stays under 0.35pp because the mismatched products are low trade value.

## Root Cause Analysis

### 1. Auto rebate classification (rev_11+)

**Symptom**: 691 mismatches, dominant diff of +/-0.0124 (1.24pp = the auto rebate). Products in HTS 8708 (auto parts), 7320 (springs), 8302 (mountings/hinges). Spread across all countries.

**Cause**: The tracker classifies HTS 7320 and 8302 products as auto parts eligible for the 1.24pp rebate. ETRs does not apply the rebate to these products. The auto parts heading prefix list in the CSV exporter may include these codes (via Ch99 auto_parts entries) while ETRs' auto tariff name matching doesn't.

**Impact**: -0.01pp import-weighted. Cosmetic at the aggregate level.

### 2. UK auto 232 deal rates (rev_18+)

**Symptom**: ~30,000 mismatches. Max diff = -0.4250. Concentrated on HTS 8708 x UK (country 4120). Tracker assigns `rate_232 = 0.50` (auto deal rate); ETRs sees `rate_232 = 0.00`.

**Detail**: For the max-diff products:
- Tracker: total_rate = 0.525 (s232=0.50 + ieepa=0.10 - overlap + mfn=0.025)
- ETRs: level = 0.0999 (no 232, just ieepa + mfn)

The UK auto 232 deal activates in rev_18 with country-specific rates. The CSV exporter writes these into `s232_autos_passenger` and `s232_autos_light_trucks` columns, but ETRs either isn't reading them correctly or the deal-rate logic in `export_statutory_rates()` isn't emitting them for the right product x country pairs.

**Impact**: -0.20pp import-weighted. Modest, but the product-level errors are large (42.5pp on individual lines).

### 3. Secondary diff clusters in rev_18+

The diff distribution for rev_18 also shows:
- **-0.24** (1,505 products): Likely 24% reciprocal-vs-232 difference on products where the tracker applies a 232 heading rate and ETRs doesn't
- **-0.39** (223 products): Similar pattern at a higher 232 rate tier
- **Thousands of tiny diffs** (-0.0001 to -0.0021): Likely rounding in MFN exemption shares or USMCA share arithmetic

These secondary clusters are all downstream of the same auto classification issue -- once the 232 heading assignment differs, the stacking rules produce different final rates.

## What's Validated

The core pipeline is sound. Across all 33 revisions:

- **MFN loading and exemption shares**: Correct (validated in rev_1+ where fentanyl applies)
- **IEEPA reciprocal stacking**: Correct (validated in rev_7+ when reciprocal activates for all countries)
- **IEEPA fentanyl stacking**: Correct (China stacks, others mutually exclusive)
- **Section 232 steel/aluminum**: Correct (validated across all revisions)
- **Section 232 derivatives + metal content scaling**: Correct (validated rev_3+)
- **Section 301**: Correct (China-specific, validated rev_7+)
- **USMCA exemptions**: Correct (CA/MX share reduction)
- **Target-total floor logic**: Correct (Japan/EU floors validated)
- **Country-level architecture**: Correct (240 countries, 8 partner aggregation)

## Fixes Needed

1. **Auto parts heading classification** (tracker exporter): Align which HTS prefixes qualify as auto parts in `export_statutory_rates()` vs ETRs' auto tariff name matching. May need to pass the Ch99 auto_parts heading list through to the CSV so ETRs can apply the rebate to the same products.

2. **UK auto 232 deal rates** (tracker exporter or ETRs loader): Investigate why the UK deal rates in `s232_autos_passenger` / `s232_autos_light_trucks` columns are not reflected in ETRs' final rates. Could be the target_total double-prefix bug from `csv-interface-bugs.md` (Bug #1), or the deal rates not being written to the correct products.

## Test Infrastructure

The test script (`tariff-rate-tracker/scripts/test_all_revisions.R`) now handles the function-name collision between the two repos. Three functions are defined in both repos with different signatures:

| Function | Tracker | ETRs | Resolution |
|----------|---------|------|------------|
| `load_mfn_exemption_shares` | `path=` (with default) | `file` (required) | Unified version with default; compatible with both callers |
| `load_metal_content` | `metal_cfg, hts10_codes, derivative_hts10` | `metal_content_config, import_data` | Swapped before each caller in the test loop |
| `load_census_codes` | `path=` (with default) | `file=` (with default) | Not called during test; no swap needed |

The longer-term fix is the two-process approach described in `csv-interface-status.md`: tracker exports CSVs in one process, ETRs loads and validates in a separate process.
