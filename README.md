# Tesbury — Site Selection & Sales Drivers

Recruiter‑focused snapshot: multi‑criteria site decision (WSM/TOPSIS) + store‑level sales regression.

## Highlights
- **Decision models:** WSM & TOPSIS across 3 scenarios (+ sensitivity on cost weight / importance factor).
- **Sales modeling:** Per‑store OLS and an overall model; validation via Actual vs Predicted.
- **Ready visuals:** 11 figures in `/images` with descriptive filenames.

## Repo Layout
```
data/       # Sales.csv (input)
scripts/    # part1_tesbury_site_selection.R, part2_sales_regression.R
images/     # wsm_*, topsis_*, sensitivity_*, actual_vs_predicted_*, store_coefficients_*
docs/       # one_pager.md (source) + one_pager.pdf (shareable)
```
## Reproduce (R)
```r
# Run analyses (adapt file paths if needed)
source('scripts/part1_tesbury_site_selection.R')
source('scripts/part2_sales_regression.R')
```
## Figures (quick view)

**Model fit**
- <img src="https://raw.githubusercontent.com/YugalSoni18/Tesbury_Site_Selection_and_Sales/main/images/actual_vs_predicted.png" width="600" alt="Actual vs Predicted">

**Store drivers**
- <img src="https://raw.githubusercontent.com/YugalSoni18/Tesbury_Site_Selection_and_Sales/main/images/store_coefficients.png" width="600" alt="Store Coefficients">

**WSM scenarios**
- <img src="https://raw.githubusercontent.com/YugalSoni18/Tesbury_Site_Selection_and_Sales/main/images/wsm_s1_cost_priority.png" width="600" alt="WSM — Scenario 1 (Cost priority)">
- <img src="https://raw.githubusercontent.com/YugalSoni18/Tesbury_Site_Selection_and_Sales/main/images/wsm_s2_space_security_x2.png" width="600" alt="WSM — Scenario 2 (Space & Security x2)">
- <img src="https://raw.githubusercontent.com/YugalSoni18/Tesbury_Site_Selection_and_Sales/main/images/wsm_s3_24x7_enforced.png" width="600" alt="WSM — Scenario 3 (24x7 enforced)">

**TOPSIS**
- <img src="https://raw.githubusercontent.com/YugalSoni18/Tesbury_Site_Selection_and_Sales/main/images/topsis_s3_24x7.png" width="600" alt="TOPSIS — Scenario 3 (24x7)">

**Sensitivity (utility margin + rank)**
- <img src="https://raw.githubusercontent.com/YugalSoni18/Tesbury_Site_Selection_and_Sales/main/images/sensitivity_s1_cost_weight_utility.png" width="600" alt="S1 — Cost weight (utility)">
- <img src="https://raw.githubusercontent.com/YugalSoni18/Tesbury_Site_Selection_and_Sales/main/images/sensitivity_s1_rank.png" width="600" alt="S1 — Rank vs cost weight">
- <img src="https://raw.githubusercontent.com/YugalSoni18/Tesbury_Site_Selection_and_Sales/main/images/sensitivity_s2_importance_factor.png" width="600" alt="S2 — Importance factor (utility)">
- <img src="https://raw.githubusercontent.com/YugalSoni18/Tesbury_Site_Selection_and_Sales/main/images/sensitivity_s2_rank.png" width="600" alt="S2 — Rank vs importance factor">
- <img src="https://raw.githubusercontent.com/YugalSoni18/Tesbury_Site_Selection_and_Sales/main/images/sensitivity_s3_cost_weight.png" width="600" alt="S3 — Cost weight (utility)">

License: MIT (see LICENSE).
