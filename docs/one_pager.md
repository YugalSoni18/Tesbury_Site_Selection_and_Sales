# Tesbury: Site Selection & Sales Drivers — One‑Pager

**Goal:** Pick the best store location under multiple criteria (space, security, accessibility, customer reach, **cost**) and model weekly sales drivers across existing stores.

**Data:** `Sales.csv` — weekly sales with macro features (Fuel_Price, CPI, Temperature, Unemployment).

## What we did
- **Multi‑criteria decision modeling**: built **WSM** and **TOPSIS** scenarios to compare four alternatives (A1 Centre, A2 Suburbs, A3 Shared, A4 Extend).
- **Sensitivity analysis**: swept **Cost weight (C5)** and feature importance factors to reveal tipping points.
- **Store‑level regression**: OLS per store + overall model; validated with **Actual vs Predicted** and confidence bands.

## Key findings (from figures)
- **Site choice**  
  - *Scenario 1 (Cost 50%)* → **A4 Extend** best overall.  
  - *Scenario 2 (Space/Security ×2)* → **A2 Suburbs** rises to #1.  
  - *Scenario 3 (24/7 constraint)* → **A2 Suburbs** wins (**TOPSIS** closeness highest).
- **Tipping points**  
  - *S1*: Rankings stable across cost weights (A4 holds #1).  
  - *S2*: Results robust as space/security importance increases.  
  - *S3*: **Tipping ~0.24** on cost weight moves rank order (see S3 rank plot).
- **Sales drivers**  
  - **Fuel_Price** has the largest absolute effect magnitudes (store‑specific).  
  - **CPI** and **Temperature** show moderate effects; **Unemployment** varies by store.  
  - Overall model fits well (tight Actual vs Predicted cloud by store cluster).

## Outcome
- Recommended **A2 Suburbs** when 24/7 feasibility is enforced; otherwise **A4 Extend** under cost‑priority assumptions.
- Clear explanatory visuals + scripts enable scenario re‑runs in minutes.

**Stack:** R (tidyverse, ggplot2), MCDA (WSM/TOPSIS), OLS.

*Last updated: 2025-10-01*