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
![Actual vs Predicted](images/actual_vs_predicted_sales.png)

**Store drivers**  
![Store Coefficients](images/store_coefficients_by_store.png)

**WSM scenarios**  
![WSM — Scenario 1 (Cost priority)](images/wsm_scenario1_cost_priority.png)  
![WSM — Scenario 2 (Space & Security ×2)](images/wsm_scenario2_space_security_x2.png)  
![WSM — Scenario 3 (24×7 enforced)](images/wsm_scenario3_24x7.png)

**TOPSIS**  
![TOPSIS — Scenario 3 (24×7)](images/topsis_scenario3_24x7.png)

**Sensitivity (utility margin + rank)**  
![S1 — Utility vs Cost weight](images/s1_utility_vs_weight_and_rank.png)  
![S2 — Utility vs Importance factor](images/s2_utility_vs_importance_and_rank.png)  
![S3 — Utility vs Cost weight](images/s3_utility_vs_weight_and_rank.png)  
![S1 — Margin](images/sensitivity_s1_margin.png)  
![S2 — Margin](images/sensitivity_s2_margin.png)  
![S3 — Margin](images/sensitivity_s3_margin.png)

License: MIT (see LICENSE).
