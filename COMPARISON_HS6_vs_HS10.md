# Comparison: HS6 (Old) vs HS10 (New) Results

## Scenario: 10-30

### Overall ETRs - 2024 Census Weights (Regular)

| Country | OLD (HS6) | NEW (HS10) | Difference | % Change |
|---------|-----------|------------|------------|----------|
| CHINA   | 19.93%    | 21.28%     | +1.35 pp   | +6.8%    |
| JAPAN   | 17.40%    | 18.85%     | +1.45 pp   | +8.3%    |
| ROW     | 23.82%    | 22.56%     | -1.26 pp   | -5.3%    |
| EU      | 11.88%    | 12.21%     | +0.33 pp   | +2.8%    |
| FTROW   | 14.51%    | 14.35%     | -0.16 pp   | -1.1%    |
| UK      | 10.71%    | 10.89%     | +0.18 pp   | +1.7%    |
| MEXICO  | 10.71%    | 13.07%     | +2.36 pp   | +22.0%   |
| CANADA  | 6.58%     | 7.23%      | +0.65 pp   | +9.9%    |
| **TOTAL** | **15.45%** | **15.90%** | **+0.45 pp** | **+2.9%** |

### Key Observations

**Significant Changes:**
1. **MEXICO**: +2.36 percentage points (+22.0%) - largest absolute and relative change
2. **JAPAN**: +1.45 percentage points (+8.3%)
3. **CHINA**: +1.35 percentage points (+6.8%)
4. **ROW**: -1.26 percentage points (-5.3%) - decreased
5. **CANADA**: +0.65 percentage points (+9.9%)

**Total ETR Impact:**
- Overall ETR increased from 15.45% to 15.90% (+0.45 percentage points)
- This represents a 2.9% relative increase in the overall tariff burden

### Possible Explanations for Differences

The differences between HS6 and HS10 results likely stem from:

1. **Data Source Differences**:
   - OLD: HS6 port-level data (dporths6ir24 files)
   - NEW: HS10 district-level data (IMP_DETL.TXT from IMDByymm.ZIP files)
   - Different aggregation methodologies

2. **Granularity in Code Matching**:
   - HS10 allows more precise targeting with 8 and 10 digit codes
   - Some HS6 codes may aggregate multiple HS10 codes with different trade patterns

3. **Updated 232.yaml Codes**:
   - The new 232.yaml contains 8 and 10 digit codes (e.g., '44031100', '9401614011')
   - These may capture different trade flows than the broader HS6 codes

4. **Mexico Spike**:
   - The 22% increase for Mexico suggests the new HS10 codes are capturing significantly more Mexico-origin imports
   - Likely due to more precise USMCA-eligible product identification at 10-digit level

### Recommendation

The differences are **material** and should be investigated to understand:
- Which result is more accurate for policy analysis
- Whether the HS10 data provides a better representation of actual trade flows
- If the 232.yaml codes at 8/10 digits are correctly specified

The refactor successfully enables variable-length HTS codes, but produces different numerical results that need validation.

---

## 232.yaml Deduplication (November 15, 2025)

### Issue Found
The 232.yaml config file contained **99 duplicate HS codes** across multiple categories, where the same code appeared in different tariff categories.

### Deduplication Strategy
For each duplicate code:
- **When rates differed**: Kept the code in the category with the **lowest average tariff rate**
- **When rates were equal**: Kept in the most **specific/appropriate category**
  - Preferred `automobile_parts` over `vehicle_parts_mhd`
  - Preferred `vehicles_completed_mhd` over parts categories
  - For materials: preferred vehicle categories (lower rate) over `steel`/`aluminum_derivative` categories (higher rate)

### Codes Removed by Category

| Category | Codes Removed | Reason |
|----------|---------------|---------|
| `vehicle_parts_mhd` | 90 codes | Duplicates also in `automobile_parts` with same rate (25%) |
| `steel` | 4 codes | Also in `vehicle_parts_mhd` with lower rate (25% vs 46.9%) |
| `aluminum_derivative_9903_85_04` | 1 code | Also in `vehicle_parts_mhd` with lower rate (25% vs 46.9%) |
| `aluminum_derivative_9903_85_08` | 2 codes | Also in vehicle parts with lower rate, or redundant with steel |
| `automobile_parts` | 2 codes | Moved to `vehicles_completed_mhd` as more appropriate |
| **TOTAL** | **99 codes** | |

### Result Validation

**Overall ETR after deduplication (10-30 scenario):**
- TOTAL ETR: **15.90%** (2024 Census Weights)
- TOTAL ETR: **16.59%** (GTAP Weights)

**Comparison with pre-deduplication:**
- Results are **IDENTICAL** to those obtained before removing duplicates
- This confirms that the duplicates were truly redundant (same codes receiving same effective tariff treatment)
- The deduplication cleaned up the config without changing the economic model

### Final State
- **Unique codes**: 688 (down from 787 entries with duplicates)
- **Zero duplicate codes** remaining
- **Config integrity**: Verified through test run of full 10-30 scenario
