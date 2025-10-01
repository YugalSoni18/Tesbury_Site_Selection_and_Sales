BADS Assignment R Script
Part 1:


dir.create("outputs_part1", showWarnings = FALSE, recursive = TRUE)

# ---------------------- 1) Packages -----------------------
need <- c("tidyverse","janitor","scales","RColorBrewer","grid")
to_install <- need[!need %in% installed.packages()[,"Package"]]
if (length(to_install)) install.packages(to_install, dependencies = TRUE)

suppressPackageStartupMessages({
  library(tidyverse)
  library(janitor)
  library(scales)
  library(RColorBrewer)
  library(grid)     # for arrow() in annotations
})

# ---------------------- 2) Data (from brief) --------------
alts <- tribble(
  ~Alternative,    ~C1_transport,             ~C2_hours,              ~C3_space,   ~C4_security_0to10, ~C5_cost_gbp,
  "A1_Centre",     "good road & rail",        "24/7",                 "Poor",      6,                   90000,
  "A2_Suburbs",    "good road, no rail",      "24/7",                 "Excellent", 8,                   60000,
  "A3_Shared",     "poor road, good rail",    "7-23, except Sunday",  "Good",      6,                   30000,
  "A4_Extend",     "excellent road & rail",   "24/7",                 "Good",      2,                   20000
)
cat("\n== Raw alternatives & criteria ==\n")
print(alts)

# ---------------------- 3) Value functions ----------------
vf_transport <- c(
  "poor road, good rail"   = 0.50,
  "good road, no rail"     = 0.60,
  "good road & rail"       = 0.80,
  "excellent road & rail"  = 1.00
)
vf_hours <- c("24/7"=1.00, "7-23, except Sunday"=0.40)
vf_space <- c("Poor"=0.00, "Good"=0.70, "Excellent"=1.00)

normalize_01 <- function(x) (x - min(x)) / (max(x) - min(x) + 1e-9)

alts_val <- alts %>%
  clean_names() %>%
  mutate(
    v1_transport = vf_transport[c1_transport] %>% as.numeric(),
    v2_hours     = vf_hours[c2_hours] %>% as.numeric(),
    v3_space     = vf_space[c3_space] %>% as.numeric(),
    v4_security  = c4_security_0to10 / 10,
    v5_cost      = 1 - normalize_01(c5_cost_gbp)      # lower £ => higher value
  )

cat("\n== Value-scaled (0–1) ==\n")
print(alts_val %>% select(alternative, starts_with("v")))

# ---------------------- 4) Scenario weights ---------------
w_S1 <- c(C1=0.125, C2=0.125, C3=0.125, C4=0.125, C5=0.50)           # S1 Cost priority
w_S2 <- c(C1=1/7,  C2=1/7,  C3=2/7,  C4=2/7,  C5=1/7)                # S2 Space&Security ×2
w_S3_equal <- c(C1=0.25, C3=0.25, C4=0.25, C5=0.25)                  # S3 24/7 required (C2 excluded)

# ---------------------- 5) WSM function -------------------
score_with_weights <- function(df, w) {
  stopifnot(abs(sum(w) - 1) < 1e-8)
  df %>%
    mutate(
      score = w["C1"]*v1_transport +
        (if ("C2" %in% names(w)) w["C2"] else 0)*v2_hours +
        w["C3"]*v3_space +
        w["C4"]*v4_security +
        w["C5"]*v5_cost
    ) %>%
    arrange(desc(score)) %>%
    mutate(rank = row_number())
}

# ---------------------- 6) WSM results (S1 & S2 only) ----
res_S1 <- alts_val %>% score_with_weights(w_S1)
res_S2 <- alts_val %>% score_with_weights(w_S2)

cat("\n== WSM S1 ==\n")
print(res_S1 %>% select(alternative, score, rank))
cat("\n== WSM S2 ==\n")
print(res_S2 %>% select(alternative, score, rank))

# Save WSM tables for S1 & S2
write_csv(res_S1 %>% select(alternative, starts_with("v"), score, rank), "outputs_part1/scenario1_cost_priority.csv")
write_csv(res_S2 %>% select(alternative, starts_with("v"), score, rank), "outputs_part1/scenario2_space_security_x2.csv")

# ---------------------- 7) WSM bar charts (S1 & S2 only) --
pastel_palette <- brewer.pal(4, "Pastel1")
names(pastel_palette) <- c("A1_Centre","A2_Suburbs","A3_Shared","A4_Extend")

plot_scores <- function(tbl, title){
  ggplot(tbl, aes(x = fct_reorder(alternative, score), y = score, fill = alternative)) +
    geom_col() + coord_flip() +
    scale_y_continuous(labels = label_percent(accuracy = 1)) +
    scale_fill_manual(values = pastel_palette, drop = FALSE) +
    labs(x=NULL, y="Overall value (0–1)", title=title, fill="Alternative") +
    theme_minimal(base_size = 12)
}
g_S1 <- plot_scores(res_S1, "WSM — Scenario 1: Cost priority (w_C5 = 0.50)")
g_S2 <- plot_scores(res_S2, "WSM — Scenario 2: Space & Security ×2")
print(g_S1)
print(g_S2)
ggsave("outputs_part1/WSM_S1_scores.png", g_S1, width=7, height=4.5, dpi=300)
ggsave("outputs_part1/WSM_S2_scores.png", g_S2, width=7, height=4.5, dpi=300)

# ---------------------- 13) TOPSIS (Scenario 3 only) ------
topsis <- function(values_mat, weights, alt_names){
  stopifnot(abs(sum(weights) - 1) < 1e-8)
  denom <- sqrt(colSums(values_mat^2))
  R <- sweep(values_mat, 2, denom, "/")
  V <- sweep(R, 2, weights, "*")
  ideal_best  <- apply(V, 2, max)
  ideal_worst <- apply(V, 2, min)
  d_plus  <- sqrt(rowSums((V - matrix(ideal_best,  nrow(V), ncol(V), byrow=TRUE))^2))
  d_minus <- sqrt(rowSums((V - matrix(ideal_worst, nrow(V), ncol(V), byrow=TRUE))^2))
  cc <- d_minus / (d_plus + d_minus)
  tibble(Alternative = alt_names, topsis_score = cc) %>% arrange(desc(topsis_score)) %>% mutate(topsis_rank=row_number())
}
plot_topsis <- function(tbl, title){
  ggplot(tbl, aes(x=fct_reorder(Alternative, topsis_score), y=topsis_score, fill=Alternative)) +
    geom_col() + coord_flip() +
    scale_y_continuous(labels = label_number(accuracy=0.01)) +
    scale_fill_manual(values = pastel_palette, drop=FALSE) +
    labs(x=NULL, y="TOPSIS closeness", title=title, fill="Alternative") +
    theme_minimal(base_size = 12)
}

# S3 (24/7 constraint, C2 excluded), using TOPSIS
S3_df_full <- alts_val %>% filter(c2_hours == "24/7")
mat_S3 <- S3_df_full %>% select(v1_transport, v3_space, v4_security, v5_cost) %>% as.matrix()
top_S3 <- topsis(mat_S3, weights = w_S3_equal, alt_names = S3_df_full$alternative)
print(top_S3); g3 <- plot_topsis(top_S3, "TOPSIS — Scenario 3: 24/7 (infeasible removed)")
print(g3)
write_csv(top_S3, "outputs_part1/topsis_S3_scores.csv")
ggsave("outputs_part1/TOPSIS_S3_scores.png", g3, width=7, height=4.5, dpi=300)


# ---------------------- 8) Sensitivity engine (core) ------
build_sensitivity <- function(df, x_seq, make_w) {
  scores <- purrr::map_dfr(x_seq, function(xv) {
    w <- make_w(xv)
    score_with_weights(df, w) %>% dplyr::select(alternative, score) %>% dplyr::mutate(x = xv)
  })
  winners <- scores %>%
    dplyr::arrange(x, dplyr::desc(score)) %>%
    dplyr::group_by(x) %>%
    dplyr::summarise(
      top       = dplyr::first(alternative),
      top_score = dplyr::first(score),
      second    = dplyr::nth(alternative, 2),
      second_sc = dplyr::nth(score, 2),
      margin    = top_score - second_sc,
      .groups = "drop"
    )
  ranges <- winners %>%
    dplyr::arrange(x) %>%
    dplyr::mutate(grp = cumsum(top != dplyr::lag(top, default = dplyr::first(top)))) %>%
    dplyr::group_by(top, grp) %>%
    dplyr::summarise(xmin = min(x), xmax = max(x), xmid = (xmin + xmax)/2, .groups = "drop") %>%
    dplyr::arrange(xmin)
  
  list(scores = scores, winners = winners, ranges = ranges)
}

# ---------------------- 9) Plotters (warning-proof) -------
plot_sensitivity_lines <- function(scores, winners, ranges, x_label, file_tag, title_tag, palette){
  pal_sub <- palette[intersect(names(palette), unique(scores$alternative))]
  tipping_x <- ranges$xmin[-1]
  y_max <- max(scores$score, na.rm = TRUE)
  
  winner_layer <- dplyr::left_join(scores, winners, by = "x") %>%
    dplyr::filter(alternative == top)
  
  p <- ggplot(scores, aes(x = x, y = score, colour = alternative)) +
    geom_line(linewidth = 1) +
    geom_line(data = winner_layer, linewidth = 2) +
    { if (length(tipping_x)) geom_vline(xintercept = tipping_x, linetype = "dashed") } +
    scale_color_manual(values = pal_sub, name = "Alternative") +
    labs(x = x_label, y = "Overall value (0–1)", title = paste(title_tag, "— utility vs", x_label)) +
    theme_minimal(base_size = 12) +
    coord_cartesian(clip = "on") +   # labels placed inside range
    geom_label(
      data = ranges,
      mapping = aes(x = xmid, y = y_max * 0.985, label = top),
      inherit.aes = FALSE,
      size = 3, label.size = 0, alpha = 0.7
    ) +
    { if (length(tipping_x))
      annotate("segment",
               x = tipping_x, xend = tipping_x,
               y = y_max * 0.95, yend = y_max * 0.88,
               arrow = arrow(length = unit(0.18, "cm")),
               linewidth = 0.3)
    } +
    { if (length(tipping_x))
      annotate("label",
               x = tipping_x, y = y_max * 0.97,
               label = paste0("Tipping @ ", format(round(tipping_x, 2), nsmall = 2)),
               size = 3, label.size = 0.2, alpha = 0.85)
    }
  
print(p)
ggsave(paste0("outputs_part1/", file_tag, "_lines.png"), p, width = 7, height = 4.5, dpi = 300)
}

plot_sensitivity_ranks <- function(scores, ranges, x_label, file_tag, palette){
  rank_df <- scores %>%
    dplyr::group_by(x) %>%
    dplyr::arrange(dplyr::desc(score), .by_group = TRUE) %>%
    dplyr::mutate(rank = dplyr::row_number()) %>%
    dplyr::ungroup()
  
  pal_sub <- palette[intersect(names(palette), unique(rank_df$alternative))]
  tipping_x <- ranges$xmin[-1]
  n_alt <- length(unique(rank_df$alternative))
  
  p <- ggplot(rank_df, aes(x = x, y = rank, colour = alternative)) +
    geom_line(linewidth = 1) +
    scale_y_reverse(breaks = 1:n_alt, limits = c(n_alt, 1)) +  # 1 at top
    { if (length(tipping_x)) geom_vline(xintercept = tipping_x, linetype = "dashed") } +
    scale_color_manual(values = pal_sub, name = "Alternative") +
    labs(x = x_label, y = "Rank (1 = best)", title = paste(file_tag, "— rank across", x_label)) +
    theme_minimal(base_size = 12) +
    coord_cartesian(clip = "off") +
    { if (length(tipping_x))
      annotate("label",
               x = tipping_x, y = 1,
               label = paste0("Tipping @ ", format(round(tipping_x, 2), nsmall = 2)),
               vjust = 1.4, size = 3, label.size = 0.2, alpha = 0.85)
    }
  
  print(p)
  ggsave(paste0("outputs_part1/", file_tag, "_ranks.png"), p, width = 7, height = 4.5, dpi = 300)
}

plot_sensitivity_margin <- function(winners, ranges, x_label, file_tag){
  tipping_x <- ranges$xmin[-1]
  y_rng <- range(winners$margin, na.rm = TRUE)
  span  <- diff(y_rng)
  if (!is.finite(span) || span <= 0) {   # safeguard if margins are constant
    y_rng <- c(0, 0.1); span <- 0.1
  }
  y_lab <- y_rng[1] + 0.92 * span
  y_seg_start <- y_rng[1] + 0.88 * span
  y_seg_end   <- y_rng[1] + 0.08 * span
  
  p <- ggplot(winners, aes(x = x, y = margin)) +
    geom_line(linewidth = 1) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    { if (length(tipping_x)) geom_vline(xintercept = tipping_x, linetype = "dotted") } +
    labs(x = x_label, y = "Top – second (utility)", title = paste(file_tag, "— margin of victory")) +
    theme_minimal(base_size = 12) +
    coord_cartesian(clip = "off") +
    { if (length(tipping_x))
      annotate("segment",
               x = tipping_x, xend = tipping_x,
               y = y_seg_start, yend = y_seg_end,
               arrow = arrow(length = unit(0.18, "cm")),
               linewidth = 0.3)
    } +
    { if (length(tipping_x))
      annotate("label",
               x = tipping_x, y = y_lab,
               label = paste0("Tipping @ ", format(round(tipping_x, 2), nsmall = 2)),
               size = 3, label.size = 0.2, alpha = 0.85)
    }
  
  print(p)
  ggsave(paste0("outputs_part1/", file_tag, "_margin.png"), p, width = 7, height = 4.5, dpi = 300)
}

# ---------------------- 10) Sensitivity S1 ----------------
w_seq_S1 <- seq(0.40, 0.70, by = 0.02)
make_w_S1 <- function(wc){
  w_other <- (1 - wc)/4
  c(C1=w_other, C2=w_other, C3=w_other, C4=w_other, C5=wc)
}
sens_S1 <- build_sensitivity(df = alts_val, x_seq = w_seq_S1, make_w = make_w_S1)

write_csv(sens_S1$winners %>% transmute(w_cost = x, top_choice = top), "outputs_part1/sensitivity_S1_top_choice.csv")
write_csv(sens_S1$scores %>% pivot_wider(names_from=alternative, values_from=score), "outputs_part1/sensitivity_S1_all_scores.csv")

plot_sensitivity_lines(sens_S1$scores, sens_S1$winners, sens_S1$ranges,
                       x_label = "Weight on Cost (C5)",
                       file_tag = "Sensitivity_S1", title_tag = "S1",
                       palette = pastel_palette)
plot_sensitivity_ranks(sens_S1$scores, sens_S1$ranges,
                       x_label = "Weight on Cost (C5)",
                       file_tag = "Sensitivity_S1", palette = pastel_palette)
plot_sensitivity_margin(sens_S1$winners, sens_S1$ranges,
                        x_label = "Weight on Cost (C5)",
                        file_tag = "Sensitivity_S1")
cat("\n[S1] Winner stability:\n")
print(sens_S1$winners %>% count(top, name="times") %>% arrange(desc(times)))
cat("[S1] Tipping at Cost weights: ",
    if(length(sens_S1$ranges$xmin[-1])) paste(sprintf('%.2f', sens_S1$ranges$xmin[-1]), collapse=", ") else "None", "\n")

# ---------------------- 11) Sensitivity S2 ----------------
f_seq <- seq(1.20, 3.00, by = 0.05)
make_w_S2 <- function(f){
  b <- 1 / (2*f + 3)
  c(C1=b, C2=b, C3=f*b, C4=f*b, C5=b)
}
sens_S2 <- build_sensitivity(df = alts_val, x_seq = f_seq, make_w = make_w_S2)

write_csv(sens_S2$winners %>% transmute(f = x, top_choice = top), "outputs_part1/sensitivity_S2_top_choice.csv")
write_csv(sens_S2$scores %>% pivot_wider(names_from=alternative, values_from=score), "outputs_part1/sensitivity_S2_all_scores.csv")

plot_sensitivity_lines(sens_S2$scores, sens_S2$winners, sens_S2$ranges,
                       x_label = "Importance factor for Space & Security (f)",
                       file_tag = "Sensitivity_S2", title_tag = "S2",
                       palette = pastel_palette)
plot_sensitivity_ranks(sens_S2$scores, sens_S2$ranges,
                       x_label = "Importance factor (f)",
                       file_tag = "Sensitivity_S2", palette = pastel_palette)
plot_sensitivity_margin(sens_S2$winners, sens_S2$ranges,
                        x_label = "Importance factor (f)",
                        file_tag = "Sensitivity_S2")
cat("\n[S2] Winner stability:\n")
print(sens_S2$winners %>% count(top, name="times") %>% arrange(desc(times)))
cat("[S2] Tipping at factor f: ",
    if(length(sens_S2$ranges$xmin[-1])) paste(sprintf('%.2f', sens_S2$ranges$xmin[-1]), collapse=", ") else "None", "\n")

# ---------------------- 12) Sensitivity S3 ----------------
S3_df <- alts_val %>% filter(c2_hours == "24/7")
w_seq_S3 <- seq(0.10, 0.60, by = 0.02)
make_w_S3 <- function(wc){
  w_other <- (1 - wc)/3
  c(C1=w_other, C2=0, C3=w_other, C4=w_other, C5=wc)
}
sens_S3 <- build_sensitivity(df = S3_df, x_seq = w_seq_S3, make_w = make_w_S3)

write_csv(sens_S3$winners %>% transmute(w_cost = x, top_choice = top), "outputs_part1/sensitivity_S3_top_choice.csv")
write_csv(sens_S3$scores %>% pivot_wider(names_from=alternative, values_from=score), "outputs_part1/sensitivity_S3_all_scores.csv")

plot_sensitivity_lines(sens_S3$scores, sens_S3$winners, sens_S3$ranges,
                       x_label = "Weight on Cost (C5) — 24/7 enforced",
                       file_tag = "Sensitivity_S3", title_tag = "S3",
                       palette = pastel_palette)
plot_sensitivity_ranks(sens_S3$scores, sens_S3$ranges,
                       x_label = "Weight on Cost (C5)",
                       file_tag = "Sensitivity_S3", palette = pastel_palette)
plot_sensitivity_margin(sens_S3$winners, sens_S3$ranges,
                        x_label = "Weight on Cost (C5)",
                        file_tag = "Sensitivity_S3")
cat("\n[S3] Winner stability:\n")
print(sens_S3$winners %>% count(top, name="times") %>% arrange(desc(times)))
cat("[S3] Tipping at Cost weights: ",
    if(length(sens_S3$ranges$xmin[-1])) paste(sprintf('%.2f', sens_S3$ranges$xmin[-1]), collapse=", ") else "None", "\n")

# ---------------------- 14) Wrap-up -----------------------
cat("\n== Outputs saved to: ", file.path(getwd(), "outputs_part1"), " ==\n", sep="")
print(list.files("outputs_part1", full.names = TRUE))