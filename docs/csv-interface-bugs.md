# CSV Interface Bugs (found 2026-03-26)

Three bugs discovered during code review of the new statutory rate CSV interface between tariff-rate-tracker and Tariff-ETRs. All three are already fixed in the working tree but not yet committed.

---

## 1. s232 target_total floor rates are dead in CSV mode

**Problem**: When the tracker writes a column like `target_total_s232_autos_passenger` in the CSV, ETRs' `load_statutory_csv()` strips only the `target_total_` prefix, leaving the program name as `s232_autos_passenger`. Then `calc_weighted_etr()` prepends `s232_` again when looking for the matching rate column, producing `s232_s232_autos_passenger_rate` — which doesn't exist. The floor logic silently does nothing.

**Impact**: Any country-specific 232 deal (e.g., EU/Japan auto floor at 15%) is ignored in CSV mode. The full 232 rate stacks on top of MFN instead of being capped at the deal level.

**Fix**: In `load_statutory_csv()` (config_parsing.R:746), change `str_replace(tt_col, '^target_total_', '')` to `str_replace(tt_col, '^target_total_s232_', '')` so the program name is `autos_passenger`, not `s232_autos_passenger`.

---

## 2. s232 YAML overlay cannot zero out CSV baseline rates

**Problem**: When a counterfactual YAML overlay sets all 232 rates to zero (`default: 0`), `load_s232_rates()` filters to `country_rate > 0` and returns an empty rate matrix. `merge_overlay()` then has no rows to anti-join against the CSV baseline, so the original positive rates survive unchanged.

**Impact**: The "CSV baseline + YAML counterfactual" workflow cannot remove 232 tariffs. You can add or change rates, but "turn this program off" silently fails for Section 232.

**Fix**: In `merge_overlay()` (calculations.R:462), detect when the overlay's s232 rate_matrix is empty (meaning "zero everything") and NULL out the base config's `params_s232` entirely.

---

## 3. Exporter misclassifies products when ch99_data is missing

**Problem**: `generate_etrs_config()` loads Chapter 99 data from a hardcoded path (`data/processed/chapter99_rates.rds`) that doesn't exist in the current checkout — the tracker stores per-revision files at `data/timeseries/ch99_{revision}.rds`. When the file is missing, `ch99_data = NULL` and `export_statutory_rates()` skips all heading gates. This treats every 232 heading program (auto_parts, copper, MHD, etc.) as active regardless of the actual revision date.

**Impact**: Products in the overlap zone between heading programs and derivatives (e.g., HTS 8708 products that match both auto_parts prefixes and aluminum derivative prefixes) get classified into the wrong program. This changes downstream metal content scaling and USMCA treatment — a product classified as `s232_auto_parts` gets no metal scaling and gets USMCA auto content share, while the same product classified as `s232_aluminum_derivatives` gets aluminum_share scaling and no USMCA.

**Fix**: Make `ch99_data` a parameter of `generate_etrs_config()` so callers pass the revision-specific data. Warn (instead of silently defaulting) when the fallback path doesn't exist.
