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

> All high-res images are under `/images`.

**Model fit**
![Actual vs Predicted](images/actual_vs_predicted.png?raw=true)

**Store drivers**
![Store Coefficients](images/store_coefficients.png)

**WSM scenarios**
![WSM — Scenario 1 (Cost priority)](images/wsm_s1_cost_priority.png)  
![WSM — Scenario 2 (Space & Security ×2)](images/wsm_s2_space_security_x2.png)  
![WSM — Scenario 3 (24×7 enforced)](images/wsm_s3_24x7_enforced.png)

**TOPSIS**
![TOPSIS — Scenario 3 (24×7)](images/topsis_s3_24x7.png)

**Sensitivity (utility margin + rank)**
![S1 — Cost weight (utility)](images/sensitivity_s1_cost_weight.png)  
![S1 — Rank vs cost weight](images/sensitivity_s1_rank.png)  
![S2 — Importance factor (utility)](images/sensitivity_s2_importance_factor.png)  
![S2 — Rank vs importance factor](images/sensitivity_s2_rank.png)  
![S3 — Cost weight (utility)](images/sensitivity_s3_cost_weight.png)

License: MIT (see LICENSE).
