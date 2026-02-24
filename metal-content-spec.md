# Metal Content Shares for Section 232 Derivative Tariffs

## 1. Motivation

Since June 2025, the U.S. has imposed a 50% Section 232 tariff on steel, aluminum, and derivative products. For derivative products classified outside HTS Chapters 72/73 (steel) and 76 (aluminum), the tariff applies only to the *value of the steel or aluminum content*, not the full entered value. The non-metal portion of the product is instead subject to other applicable tariffs (IEEPA reciprocal, fentanyl, etc.).

The effective tariff burden of Section 232 on derivative products therefore depends on the **metal content share** --- the fraction of the product's customs value attributable to steel or aluminum. This share varies enormously across products (a steel trailer vs. an electronic appliance vs. a piece of furniture).

### Formulas

For each derivative HTS code *h* from country *c*:

```
section_232_duty[h,c] = s232_rate[h,c] * metal_share[h] * customs_value[h,c]
ieepa_duty[h,c]       = ieepa_rate[c]  * (1 - metal_share[h]) * customs_value[h,c]
total_duty[h,c]       = section_232_duty[h,c] + ieepa_duty[h,c]
```

For primary products (chapters 72, 73, 76), `metal_share = 1.0` always --- the tariff applies to the full customs value.

---

## 2. Implemented Methods

We implement three methods for assigning metal content shares, plus two flat benchmarks. All are selectable via the `metal_content` block in `other_params.yaml`.

### 2a. Flat (benchmarks)

Assigns a uniform `metal_share` to all derivative products. Two benchmark cases:

- **Flat = 1.0** (upper bound): 232 tariff applies to the full customs value. Equivalent to the pre-metal-content codebase.
- **Flat = 0.0** (lower bound): 232 tariff applies to none of the derivative value. The non-metal portion receives IEEPA instead.

```yaml
metal_content:
  method: 'flat'
  flat_share: 1.0   # or 0.0
```

### 2b. BEA Input-Output Method

Assigns metal content shares at the **GTAP sector level** (~45 sectors) using BEA Input-Output requirements tables. Each GTAP sector maps to a BEA industry via `resources/gtap_bea_crosswalk.csv`, and the metal share equals the row-331 (Primary Metals) requirement coefficient for that industry.

Two sub-options control which BEA table is used:

- **Domestic requirements** (default): The Leontief inverse excluding imports. Traces the full supply chain through domestic production only. Captures indirect metal content embedded in domestically-produced intermediates but excludes imported intermediates.
- **Total requirements**: The Leontief inverse including imports. Traces the full global supply chain. Produces higher shares because it captures metal content in imported intermediates that are themselves not subject to 232.

Both are built by `scripts/build_metal_content_shares.R`, which reads the BEA CSVs in `resources/bea/`, extracts row 331, joins with the GTAP-BEA crosswalk, and writes to `resources/metal_content_shares_{domestic,total}.csv`.

```yaml
metal_content:
  method: 'bea'
  bea_table: 'domestic'   # or 'total'
```

**Key BEA shares (domestic / total):**

| GTAP Sector | Description | Domestic | Total |
|---|---|---|---|
| I_S | Iron & steel | 1.0000 | 1.0000 |
| NFM | Non-ferrous metals | 1.0000 | 1.0000 |
| FMP | Fabricated metal products | 0.2410 | 0.3548 |
| EEQ | Electrical equipment | 0.1334 | 0.2579 |
| OME | Machinery | 0.1125 | 0.1835 |
| MVH | Motor vehicles & parts | 0.0992 | 0.1792 |
| OTN | Other transport equipment | 0.0333 | 0.0638 |
| OMF | Miscellaneous manufacturing | 0.0339 | 0.0662 |
| ELE | Computer & electronic products | 0.0153 | 0.0300 |

**Strengths:** Continuous shares that reflect the actual I-O structure of each industry. Captures inter-industry variation (fabricated metals have much higher metal content than electronics).

**Limitations:** Shares are at the industry level, so all HTS codes within a GTAP sector get the same share. Also reflects U.S. domestic production technology, not the production function of foreign exporters.

### 2c. CBO Bucket Method

Assigns metal content shares at the **HTS10 product level** using three discrete buckets from the Congressional Budget Office's [conventional tariff analysis model](https://github.com/US-CBO/conventional-tariff-analysis-model).

CBO classifies Section 232 derivative products into:

| Bucket | Default Share | Products | Source file |
|---|---|---|---|
| High metal content (Al/steel derivatives) | 75% | 168 | `resources/cbo/alst_deriv_h.csv` |
| Low metal content (Al/steel derivatives) | 25% | 735 | `resources/cbo/alst_deriv_l.csv` |
| Copper derivatives | 90% | 118 | `resources/cbo/copper.csv` |

Products not matched to any CBO list default to `metal_share = 1.0` (conservative). The CBO lists have some overlapping codes; we resolve these with priority order: copper > high > low.

```yaml
metal_content:
  method: 'cbo'
  # Optional overrides (defaults match CBO):
  cbo_high_share: 0.75
  cbo_low_share: 0.25
  cbo_copper_share: 0.90
```

**Coverage of 232 derivative products:** Of the 285 derivative products (non-primary) under metal 232 programs in the 11-17 scenario, CBO lists cover 278 (97.5%). The 7 unmatched products are mostly auto parts in chapter 87 and one furniture product. Breakdown of matched derivatives:

| Chapter | Products | CBO buckets assigned |
|---|---|---|
| 74 (copper) | 113 | 0.90 |
| 84 (machinery) | 53 | 35 low, 18 high |
| 94 (furniture) | 31 | 18 low, 13 high |
| 95 (toys/sports) | 22 | 20 low, 2 high |
| 85 (electrical) | 23 | 9 low, 9 high, 5 copper |
| 83 (base metal misc) | 24 | 9 low, 15 high |
| 87 (vehicles) | 7 | 2 low, 5 high |
| Other | 5 | mixed |

**Strengths:** Product-level assignment captures within-industry variation that BEA misses. Covers 97.5% of derivatives in our 232 lists. CBO's methodology is public and well-documented.

**Limitations:** Only three discrete values (0.25, 0.75, 0.90) --- no continuous variation. Products not on CBO lists default to 1.0. The all-product mean share of 0.969 appears high, but this is because 94.7% of products are not 232 derivatives and get the 1.0 default (which never affects their ETR since they face no 232 tariff).

---

## 3. Comparison of Results

All results below are from the `11-17` scenario, which includes steel, aluminum (base + derivatives), and copper 232 programs.

### 3a. Overall ETRs (2024 Census Weights)

| Method | Mean Metal Share | Overall ETR |
|---|---|---|
| Flat = 1.0 (upper bound) | 1.0000 | 14.68% |
| CBO (product buckets) | 0.9691 | 14.20% |
| BEA Total requirements | 0.1338 | 13.69% |
| BEA Domestic requirements | 0.1114 | 13.64% |
| Flat = 0.0 (lower bound) | 0.0663 | 13.39% |

The full range across methods is 1.29 pp (14.68% to 13.39%).

### 3b. Key Sector ETRs (pp, 2024 Census Weights)

Sectors where the metal content method matters most. Columns ordered from highest to lowest metal share assumption. Only sectors with cross-method spread > 2 pp shown.

**Fabricated Metal Products (fmp):**

| Country | Flat=1 | CBO | BEA Tot | BEA Dom | Flat=0 | Spread |
|---|---|---|---|---|---|---|
| China | 46.2 | 44.5 | 42.5 | 42.0 | 40.6 | 5.5 |
| Canada | 39.0 | 37.7 | 35.9 | 35.8 | 34.6 | 4.4 |
| Mexico | 38.0 | 36.6 | 34.7 | 34.4 | 33.5 | 4.5 |
| Japan | 34.0 | 32.6 | 31.5 | 31.6 | 29.9 | 4.0 |
| EU | 33.5 | 32.8 | 31.9 | 31.7 | 31.0 | 2.5 |

**Other Machinery (ome):**

| Country | Flat=1 | CBO | BEA Tot | BEA Dom | Flat=0 | Spread |
|---|---|---|---|---|---|---|
| Mexico | 16.5 | 12.3 | 11.1 | 10.7 | 9.9 | 6.6 |
| FTROW | 20.7 | 16.7 | 15.7 | 15.2 | 14.5 | 6.2 |
| China | 26.4 | 23.1 | 21.8 | 21.4 | 20.8 | 5.7 |
| Canada | 10.0 | 7.0 | 5.4 | 5.0 | 4.4 | 5.6 |
| Japan | 19.4 | 17.1 | 16.4 | 16.2 | 15.8 | 3.6 |

**Electrical Equipment (eeq):**

| Country | Flat=1 | CBO | BEA Tot | BEA Dom | Flat=0 | Spread |
|---|---|---|---|---|---|---|
| Canada | 12.2 | 11.5 | 10.2 | 9.8 | 9.4 | 2.8 |
| Mexico | 10.8 | 10.4 | 9.0 | 8.7 | 8.4 | 2.5 |
| China | 28.9 | 28.5 | 27.2 | 26.8 | 26.5 | 2.4 |
| ROW | 23.7 | 23.4 | 22.2 | 21.9 | 21.6 | 2.1 |

**Motor Vehicles (mvh):**

| Country | Flat=1 | CBO | BEA Tot | BEA Dom | Flat=0 | Spread |
|---|---|---|---|---|---|---|
| ROW | 28.9 | 27.6 | 27.4 | 27.7 | 24.4 | 4.5 |
| China | 34.4 | 32.8 | 31.8 | 31.9 | 30.5 | 3.9 |
| Canada | 18.0 | 16.7 | 14.4 | 14.4 | 14.1 | 3.9 |
| Mexico | 18.4 | 17.6 | 16.4 | 16.4 | 16.0 | 2.4 |

**Iron & Steel (i_s):** Identical across all methods (spread = 0). All ch 72/73 products are primary and forced to `metal_share = 1.0`.

### 3c. Interpretation

1. **CBO sits close to Flat=1**: CBO's bucket shares (0.25, 0.75, 0.90) are much higher than BEA's continuous industry shares (0.01--0.24), so CBO only modestly reduces ETRs relative to the upper bound. The overall ETR drops 0.48 pp from Flat=1 to CBO.

2. **BEA Domestic and Total are close to each other**: Total requirements are ~1.5--2x the domestic values for key sectors, but since the shares are small to begin with (e.g., 0.10 vs. 0.18 for machinery), the ETR difference is only 0.05 pp overall.

3. **Both BEA variants sit close to Flat=0**: Because BEA shares for derivative sectors are low (mostly 0.01--0.24), the metal-adjusted 232 tariff is small and most of the tariff burden comes from IEEPA on the non-metal portion. Overall BEA domestic is only 0.25 pp above Flat=0.

4. **Sectors most affected**: Fabricated metals (fmp) and machinery (ome) show the largest sensitivity, with 4--7 pp spreads across methods. These sectors have high 232 tariff rates and enough derivative products for the metal share to matter.

5. **Countries most affected**: Canada and Mexico show disproportionate sensitivity because their IEEPA rates are low (USMCA exceptions), so the gap between 232-on-full-value vs. 232-on-metal-content-only is larger.

---

## 4. Configuration Reference

All options go under `metal_content` in `other_params.yaml`:

```yaml
metal_content:
  method: 'cbo'                    # 'flat', 'bea', or 'cbo'
  primary_chapters: ['72', '73', '76']   # always forced to 1.0

  # Flat method only:
  flat_share: 1.0                  # uniform share for all derivatives

  # BEA method only:
  bea_table: 'domestic'            # 'domestic' or 'total'

  # CBO method only (defaults match CBO):
  cbo_high_share: 0.75
  cbo_low_share: 0.25
  cbo_copper_share: 0.90

  # Required for any method:
  metal_programs:                  # which 232 programs use metal content adjustment
    - steel
    - aluminum_base_articles
    - aluminum_derivative_9903_85_04
    - aluminum_derivative_9903_85_07
    - aluminum_derivative_9903_85_08
    - copper_derivatives
```

When the `metal_content` block is absent (old configs), behavior defaults to `flat` with `flat_share = 1.0`, preserving backward compatibility.

---

## 5. Source Data

| File | Description |
|---|---|
| `resources/bea/BEA - Domestic Requirements, Industry-by-Commodities - Summary - 2024.csv` | BEA domestic requirements table |
| `resources/bea/BEA - Total Requirements, Industry-by-Commodities - Summary - 2024.csv` | BEA total requirements table |
| `resources/gtap_bea_crosswalk.csv` | GTAP sector to BEA industry mapping |
| `resources/metal_content_shares_domestic.csv` | Pre-computed BEA domestic shares by GTAP sector |
| `resources/metal_content_shares_total.csv` | Pre-computed BEA total shares by GTAP sector |
| `resources/cbo/alst_deriv_h.csv` | CBO high metal content HTS list (168 codes) |
| `resources/cbo/alst_deriv_l.csv` | CBO low metal content HTS list (735 codes) |
| `resources/cbo/copper.csv` | CBO copper derivative HTS list (118 codes) |

**BEA shares build step:** Run `Rscript scripts/build_metal_content_shares.R` to regenerate the `metal_content_shares_{domestic,total}.csv` files from the raw BEA tables.

---

## 6. Caveats

1. **Domestic production vs. import composition.** BEA I-O coefficients reflect the average input mix of U.S. domestic producers, not foreign producers of the same goods. Foreign production may use different metal intensities.

2. **Industry-level vs. product-level variation.** BEA shares are at the GTAP sector level (~45 sectors), so all HTS codes within a sector get the same share. A steel desk and a wooden desk with steel legs are in the same industry but have very different metal shares. CBO partially addresses this with product-level classification.

3. **Value vs. cost concepts.** The BEA I-O coefficient captures the *cost* of metal inputs to the producing industry. CBP assesses tariffs on the *value* of metal content as declared by the importer. CBP's guidance says fabrication costs on the metal itself are *not* deductible, which would make the true dutiable share higher than the raw material input share. This suggests BEA shares may be a **lower bound** on the true dutiable metal share.

4. **CBO bucket granularity.** CBO uses only three values (0.25, 0.75, 0.90). The true metal content of individual products spans a continuous range.

5. **Primary metals override.** Chapters 72, 73, and 76 are forced to `metal_share = 1.0` regardless of method. This is correct for primary products but may slightly overstate the share for some articles-of-iron/steel in chapter 73 that were reclassified as derivatives.

---

## 7. References

- BEA Input-Output Accounts: https://www.bea.gov/data/industries/input-output-accounts-data
- CBO Conventional Tariff Analysis Model: https://github.com/US-CBO/conventional-tariff-analysis-model
- BIS Section 232 Steel and Aluminum (derivative product lists): https://www.bis.gov/about-bis/bis-leadership-and-offices/SIES/section-232-investigations/section-232-steel-aluminum
- CBP Section 232 FAQs (valuation guidance): https://www.cbp.gov/trade/programs-administration/entry-summary/232-tariffs-aluminum-and-steel-faqs
