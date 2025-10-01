:
# install.packages
library(dplyr)
library(ggplot2)
library(lubridate)
library(readr)
library(GGally)
library(broom)
library(purrr)
library(car)

# ---- PATH ----
desktop_path <- "/Users/201818462/Desktop"
out_dir <- file.path(desktop_path, "outputs_part2")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# Helper to save ggplots into out_dir
save_plot <- function(plot, filename, w = 7, h = 5) {
  ggsave(file.path(out_dir, filename), plot, width = w, height = h, dpi = 300)
}

# ---- 1) Load & Clean ----
# Read Sales.csv from Desktop (input), save outputs to outputs_part2
sales <- read_csv(file.path(desktop_path, "Sales.csv"), show_col_types = FALSE)

# Harmonise the unemployment column name
if ("Unemployment" %in% names(sales) && !("Unemployment_Index" %in% names(sales))) {
  names(sales)[names(sales) == "Unemployment"] <- "Unemployment_Index"
}

# Basic structure
print(str(sales))
print(summary(sales))

# Ensure types
sales <- sales %>%
  mutate(
    Date = as.Date(Date, format = "%d/%m/%Y"),
    LocationStore = as.factor(LocationStore)
  )

# Remove rows with any NA in key cols (safe default)
key_cols <- c("Weekly_Sales","Temperature","Fuel_Price","CPI","Unemployment_Index","Date","LocationStore")
sales <- sales %>% filter(if_all(all_of(key_cols), ~ !is.na(.)))

# ---- 2) EDA ----
# (A) Sales trend over time by store
p_trend <- ggplot(sales, aes(x = Date, y = Weekly_Sales, color = LocationStore)) +
  geom_line(alpha = 0.8) +
  labs(title = "Weekly Sales Trends by Store",
       x = "Date", y = "Weekly Sales (£)", color = "Store") +
  theme_minimal(base_size = 13)
print(p_trend); save_plot(p_trend, "p2_trend_by_store.png", w=12, h=5)

# (B) Distribution of sales by store (boxplot)
p_box <- ggplot(sales, aes(x = LocationStore, y = Weekly_Sales, fill = LocationStore)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.5) +
  labs(title = "Distribution of Weekly Sales by Store",
       x = "Store", y = "Weekly Sales (£)") +
  theme_minimal(base_size = 13)
print(p_box); save_plot(p_box, "p2_box_sales_by_store.png")

# (C) Correlation heatmap (numeric vars)
num_vars <- sales %>% select(Weekly_Sales, Temperature, Fuel_Price, CPI, Unemployment_Index)
p_corr <- ggcorr(num_vars, label = TRUE, hjust = 0.85, size = 3,
                 low = "lightblue", mid = "white", high = "darkred")
print(p_corr); save_plot(p_corr, "p2_corr_heatmap.png", w=8, h=6)

# (D) Quick store summary table
store_summary <- sales %>%
  group_by(LocationStore) %>%
  summarise(
    n_weeks   = n(),
    Avg_Sales = mean(Weekly_Sales),
    SD_Sales  = sd(Weekly_Sales),
    Min_Sales = min(Weekly_Sales),
    Max_Sales = max(Weekly_Sales),
    .groups = "drop"
  )
write_csv(store_summary, file.path(out_dir, "p2_store_summary.csv"))
print(store_summary)

# ---- 3) Overall Regression (with store dummies) ----
model_overall <- lm(
  Weekly_Sales ~ Temperature + Fuel_Price + CPI + Unemployment_Index + LocationStore,
  data = sales
)
overall_sum <- summary(model_overall)
print(overall_sum)

# Save overall summary to CSV (tidy)
overall_tidy   <- tidy(model_overall, conf.int = TRUE)
overall_glance <- glance(model_overall)
write_csv(overall_tidy,  file.path(out_dir, "p2_overall_coefficients.csv"))
write_csv(overall_glance, file.path(out_dir, "p2_overall_fit_stats.csv"))

# VIF (multicollinearity)
vif_values <- car::vif(model_overall)
print(vif_values)
write_csv(tibble(term = names(vif_values), VIF = as.numeric(vif_values)),
          file.path(out_dir, "p2_overall_vif.csv"))

# Diagnostics (RStudio plots window)
par(mfrow=c(2,2)); plot(model_overall); par(mfrow=c(1,1))

# Coefficient plot (overall)
p_coef <- overall_tidy %>%
  filter(term != "(Intercept)") %>%
  ggplot(aes(x = reorder(term, estimate), y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_pointrange(color = "#1F78B4") +
  coord_flip() +
  labs(title = "Overall Regression Coefficients (95% CI)",
       x = "Predictor", y = "Effect on Weekly Sales") +
  theme_minimal(base_size = 13)
print(p_coef); save_plot(p_coef, "p2_overall_coef_plot.png", w=9, h=6)

# Actual vs Predicted (overall)
sales$Predicted_Overall <- predict(model_overall)
p_pred <- ggplot(sales, aes(x = Weekly_Sales, y = Predicted_Overall, color = LocationStore)) +
  geom_point(alpha = 0.6) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  labs(title = "Actual vs Predicted Weekly Sales (Overall Model)",
       x = "Actual (£)", y = "Predicted (£)") +
  theme_minimal(base_size = 13)
print(p_pred); save_plot(p_pred, "p2_overall_actual_vs_pred.png", w=7, h=6)

# ---- 4) Store-Specific Regressions ----
models_by_store <- sales %>%
  group_split(LocationStore) %>%
  set_names(levels(sales$LocationStore)) %>%
  map(~ lm(Weekly_Sales ~ Temperature + Fuel_Price + CPI + Unemployment_Index, data = .x))

# Save tidy results per store
store_results <- imap_dfr(models_by_store, ~ tidy(.x, conf.int = TRUE) %>% mutate(Store = .y))
store_fit     <- imap_dfr(models_by_store, ~ glance(.x) %>% mutate(Store = .y))

write_csv(store_results, file.path(out_dir, "p2_storewise_coefficients.csv"))
write_csv(store_fit,     file.path(out_dir, "p2_storewise_fit_stats.csv"))
print(store_fit)

# Coefficient plots per store (facet)
p_coef_store <- store_results %>%
  filter(term != "(Intercept)") %>%
  ggplot(aes(x = reorder(term, estimate), y = estimate, ymin = conf.low, ymax = conf.high, color = term)) +
  geom_pointrange(show.legend = FALSE) +
  coord_flip() +
  facet_wrap(~ Store, scales = "free_x") +
  labs(title = "Store-specific Regression Coefficients (95% CI)",
       x = "Predictor", y = "Effect on Weekly Sales") +
  theme_minimal(base_size = 12)
print(p_coef_store); save_plot(p_coef_store, "p2_storewise_coef_plot.png", w=12, h=6)

# Actual vs Predicted per store (panels)
pred_by_store <- sales %>%
  group_by(LocationStore) %>%
  group_modify(~ {
    m <- lm(Weekly_Sales ~ Temperature + Fuel_Price + CPI + Unemployment_Index, data = .x)
    .x %>% mutate(Predicted = predict(m))
  }) %>% ungroup()

p_pred_store <- ggplot(pred_by_store, aes(x = Weekly_Sales, y = Predicted, color = LocationStore)) +
  geom_point(alpha = 0.6) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  facet_wrap(~ LocationStore) +
  labs(title = "Actual vs Predicted Weekly Sales — Store-specific Models",
       x = "Actual (£)", y = "Predicted (£)") +
  theme_minimal(base_size = 12)
print(p_pred_store); save_plot(p_pred_store, "p2_storewise_actual_vs_pred.png", w=12, h=6)

# ---- 5) Business-Focused Summaries ----
# Overall: sign (+/-) & p-value flag
overall_effects <- overall_tidy %>%
  filter(term != "(Intercept)") %>%
  transmute(
    term,
    estimate,
    p_value = p.value,
    direction = if_else(estimate > 0, "Positive", "Negative"),
    significant = if_else(p.value < 0.05, "Yes", "No")
  )
write_csv(overall_effects, file.path(out_dir, "p2_overall_effects_summary.csv"))
print(overall_effects)

# Per-store: sign & significance for each predictor
store_effects <- store_results %>%
  filter(term != "(Intercept)") %>%
  transmute(
    Store,
    term,
    estimate,
    p_value = p.value,
    direction = if_else(estimate > 0, "Positive", "Negative"),
    significant = if_else(p.value < 0.05, "Yes", "No")
  ) %>% arrange(Store, desc(significant), term)
write_csv(store_effects, file.path(out_dir, "p2_storewise_effects_summary.csv"))
print(store_effects, n = 100)

cat("\nAll outputs saved to:\n", normalizePath(out_dir), "\n")