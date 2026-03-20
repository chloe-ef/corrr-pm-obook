# 04_surprise_decomp.R — Who was closer? Signed errors, PEAD decomposition
# Input:  build/event_panel.rds
# Output: analysis/surprise_results.rds, figures

library(data.table)
library(ggplot2)
library(fixest)

data_dir     <- "~/Documents/git/corrr/390_paper/data"
analysis_dir <- "~/Documents/data/corrr/390_paper/analysis"

panel <- readRDS(file.path(data_dir, "event_panel.rds"))
p <- panel[!is.na(actual_eps) & !is.na(consensus_mean) & !is.na(eps_target)]
cat("Surprise decomposition sample:", nrow(p), "events\n")

results <- list()

# ═══════════════════════════════════════════════════════════════════════════
# 1. WHO WAS CLOSER?
# ═══════════════════════════════════════════════════════════════════════════

cat("\n=== WHO WAS CLOSER? (MM line vs Analyst Consensus) ===\n")

pf <- p[!is.na(pm_closer)]
cat("PM closer:", sum(pf$pm_closer), "of", nrow(pf),
    "(", round(mean(pf$pm_closer) * 100, 1), "%)\n")

# Binomial test: is it significantly different from 50%?
bt <- binom.test(sum(pf$pm_closer), nrow(pf), p = 0.5)
cat("Binomial test p-value:", format.pval(bt$p.value, digits = 3), "\n")

results$closer_overall <- list(
  pm_closer_pct = mean(pf$pm_closer),
  n = nrow(pf),
  binom_p = bt$p.value
)

# Stratify by GAAP/non-GAAP
cat("\nBy EPS type:\n")
by_type <- pf[, .(
  n = .N,
  pm_closer_pct = mean(pm_closer),
  mae_pm = mean(abs_delta_pm),
  mae_an = mean(abs_delta_an)
), by = eps_type]
print(by_type)
results$closer_by_type <- by_type

# Stratify by analyst coverage
pf[, coverage_group := fcase(
  num_analysts <= 5,  "low (1-5)",
  num_analysts <= 15, "medium (6-15)",
  default = "high (16+)"
)]
cat("\nBy analyst coverage:\n")
by_cov <- pf[!is.na(coverage_group), .(
  n = .N,
  pm_closer_pct = mean(pm_closer),
  mae_pm = mean(abs_delta_pm),
  mae_an = mean(abs_delta_an)
), by = coverage_group]
print(by_cov)
results$closer_by_coverage <- by_cov

# Stratify by PM volume
pf[, vol_group := fcase(
  total_pm_volume <= quantile(total_pm_volume, 0.33, na.rm = TRUE), "low",
  total_pm_volume <= quantile(total_pm_volume, 0.67, na.rm = TRUE), "medium",
  default = "high"
)]
cat("\nBy PM volume:\n")
by_vol <- pf[, .(
  n = .N,
  pm_closer_pct = mean(pm_closer),
  mae_pm = mean(abs_delta_pm),
  mae_an = mean(abs_delta_an)
), by = vol_group]
print(by_vol)
results$closer_by_volume <- by_vol

# ═══════════════════════════════════════════════════════════════════════════
# 1b. SELL-SIDE BIAS REGRESSION (Rev 2)
# actual_eps = alpha + beta * consensus_mean
# Quantifies systematic analyst undershoot.
# Key citations: Richardson, Teoh & Wysocki (2004) "Walk-down to Beatable
# Analyst Forecasts"; Matsumoto (2002); Ke & Yu (2006);
# Cotter, Tuna & Wysocki (2006).
# ═══════════════════════════════════════════════════════════════════════════

cat("\n=== SELL-SIDE BIAS REGRESSION ===\n")

bias_data <- p[!is.na(actual_eps) & !is.na(consensus_mean)]
cat("Bias regression sample:", nrow(bias_data), "\n")

if (nrow(bias_data) >= 20) {
  # Pooled OLS: actual_eps = alpha + beta * consensus_mean
  bias_fit <- lm(actual_eps ~ consensus_mean, data = bias_data)
  bias_sum <- summary(bias_fit)

  cat("\nPooled OLS: actual_eps ~ consensus_mean\n")
  print(bias_sum$coefficients)
  cat("R-squared:", round(bias_sum$r.squared, 4), "\n")
  cat("Adj R-squared:", round(bias_sum$adj.r.squared, 4), "\n")

  # Interpretation
  alpha_hat <- coef(bias_fit)[1]
  beta_hat  <- coef(bias_fit)[2]
  cat(sprintf("\nalpha = %.4f (expected > 0 if systematic undershoot)\n", alpha_hat))
  cat(sprintf("beta  = %.4f (expected ≈ 1)\n", beta_hat))

  results$bias_regression <- list(
    alpha     = alpha_hat,
    beta      = beta_hat,
    r_squared = bias_sum$r.squared,
    adj_r2    = bias_sum$adj.r.squared,
    coef_table = bias_sum$coefficients,
    n         = nrow(bias_data)
  )

  # Per-ticker alpha for tickers with 2+ observations
  ticker_counts <- bias_data[, .N, by = ticker]
  multi_tickers <- ticker_counts[N >= 2, ticker]

  if (length(multi_tickers) >= 5) {
    cat("\nPer-ticker alpha (tickers with 2+ obs):\n")
    ticker_alphas <- bias_data[ticker %in% multi_tickers, {
      if (.N >= 2) {
        fit <- lm(actual_eps ~ consensus_mean)
        list(alpha = coef(fit)[1], n = .N)
      } else {
        list(alpha = NA_real_, n = .N)
      }
    }, by = ticker]
    ticker_alphas <- ticker_alphas[!is.na(alpha)]

    cat("  Tickers with per-ticker regression:", nrow(ticker_alphas), "\n")
    cat("  Mean alpha:", round(mean(ticker_alphas$alpha), 4), "\n")
    cat("  Median alpha:", round(median(ticker_alphas$alpha), 4), "\n")
    cat("  % with alpha > 0:", round(mean(ticker_alphas$alpha > 0) * 100, 1), "%\n")

    results$bias_per_ticker <- ticker_alphas
  }
}

# ═══════════════════════════════════════════════════════════════════════════
# 2. SIGNED ERRORS — SYSTEMATIC BIAS?
# ═══════════════════════════════════════════════════════════════════════════

cat("\n=== SIGNED ERRORS ===\n")

# delta_pm > 0 means MM line was too high (optimistic) — actual was lower
# delta_an > 0 means analyst consensus was too high (optimistic)
cat("MM line mean signed error:", round(mean(p$delta_pm, na.rm = TRUE), 4), "\n")
cat("Analyst mean signed error:", round(mean(p$delta_an, na.rm = TRUE), 4), "\n")

# t-test: is bias significantly different from 0?
tt_pm <- t.test(p$delta_pm)
tt_an <- t.test(p$delta_an)
cat("MM bias t-stat:", round(tt_pm$statistic, 3), "p =",
    format.pval(tt_pm$p.value, digits = 3), "\n")
cat("Analyst bias t-stat:", round(tt_an$statistic, 3), "p =",
    format.pval(tt_an$p.value, digits = 3), "\n")

results$signed_errors <- list(
  pm_mean = mean(p$delta_pm, na.rm = TRUE),
  an_mean = mean(p$delta_an, na.rm = TRUE),
  pm_t = as.numeric(tt_pm$statistic),
  pm_p = tt_pm$p.value,
  an_t = as.numeric(tt_an$statistic),
  an_p = tt_an$p.value
)

# Figure: PM-Implied EPS vs Analyst Consensus — Forecast Error Comparison
# Loads implied EPS from 06_implied_eps.py output and plots signed errors
# for the crowd's implied point estimate vs the analyst consensus.
implied <- fread(file.path(data_dir, "implied_eps_results.csv"))
implied <- implied[!is.na(implied_eps_t) & !is.na(actual_eps) & !is.na(consensus_mean)]

err_long <- rbindlist(list(
  data.table(source = "PM Implied EPS (Method A)", error = implied$implied_eps_t - implied$actual_eps),
  data.table(source = "Analyst Consensus", error = implied$consensus_mean - implied$actual_eps)
))
err_long <- err_long[!is.na(error)]

# Winsorize for plotting
q_lo <- quantile(err_long$error, 0.01, na.rm = TRUE)
q_hi <- quantile(err_long$error, 0.99, na.rm = TRUE)
err_plot <- err_long[error >= q_lo & error <= q_hi]

pm_mae <- round(mean(abs(implied$implied_eps_t - implied$actual_eps)), 3)
an_mae <- round(mean(abs(implied$consensus_mean - implied$actual_eps)), 3)

g_err <- ggplot(err_plot, aes(x = error, fill = source)) +
  geom_density(alpha = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(x = "Forecast Error (predicted - actual EPS)",
       y = "Density", fill = "Source",
       title = "Signed Forecast Errors: PM-Implied EPS vs Analyst Consensus",
       subtitle = sprintf("PM Implied MAE = $%.3f, Analyst MAE = $%.3f (n = %d)",
                          pm_mae, an_mae, nrow(implied))) +
  theme_minimal(base_size = 12)
ggsave(file.path(analysis_dir, "fig_signed_errors.pdf"), g_err, width = 7, height = 5)
cat("Saved fig_signed_errors.pdf\n")

# ═══════════════════════════════════════════════════════════════════════════
# 3. PEAD DECOMPOSITION — DO RETURNS RESPOND TO delta_pm OR delta_an?
# ═══════════════════════════════════════════════════════════════════════════

cat("\n=== PEAD DECOMPOSITION ===\n")

reg_data <- p[!is.na(excess_return_1d) & !is.na(delta_pm) & !is.na(delta_an)]
cat("Regression sample:", nrow(reg_data), "\n")

if (nrow(reg_data) >= 20) {
  # Standardize delta_pm and delta_an for coefficient comparability
  reg_data[, z_delta_pm := (delta_pm - mean(delta_pm)) / sd(delta_pm)]
  reg_data[, z_delta_an := (delta_an - mean(delta_an)) / sd(delta_an)]

  # Regressions
  m1 <- feols(excess_return_1d ~ z_delta_an, data = reg_data)
  m2 <- feols(excess_return_1d ~ z_delta_pm, data = reg_data)
  m3 <- feols(excess_return_1d ~ z_delta_an + z_delta_pm, data = reg_data)

  cat("\nHorse race: excess_return_1d ~ delta_an + delta_pm\n")
  etable(m1, m2, m3,
         headers = c("Analyst Only", "MM Only", "Horse Race"),
         title = "PEAD Decomposition (1-day)")

  results$pead_1d <- list(
    m1_coefs = coeftable(m1),
    m2_coefs = coeftable(m2),
    m3_coefs = coeftable(m3)
  )

  # 5-day horizon
  reg5 <- p[!is.na(excess_return_5d) & !is.na(delta_pm) & !is.na(delta_an)]
  if (nrow(reg5) >= 20) {
    reg5[, z_delta_pm := (delta_pm - mean(delta_pm)) / sd(delta_pm)]
    reg5[, z_delta_an := (delta_an - mean(delta_an)) / sd(delta_an)]

    m4 <- feols(excess_return_5d ~ z_delta_an, data = reg5)
    m5 <- feols(excess_return_5d ~ z_delta_pm, data = reg5)
    m6 <- feols(excess_return_5d ~ z_delta_an + z_delta_pm, data = reg5)

    cat("\n5-day horizon:\n")
    etable(m4, m5, m6,
           headers = c("Analyst Only", "MM Only", "Horse Race"),
           title = "PEAD Decomposition (5-day)")

    results$pead_5d <- list(
      m4_coefs = coeftable(m4),
      m5_coefs = coeftable(m5),
      m6_coefs = coeftable(m6)
    )
  }
}

# ═══════════════════════════════════════════════════════════════════════════
# 4. CROWD REFINEMENT — DOES FLOW PREDICT SURPRISE RESIDUAL?
# ═══════════════════════════════════════════════════════════════════════════

cat("\n=== CROWD REFINEMENT ===\n")

# Does flow_imbalance predict the sign/magnitude of delta_an?
# If crowd buys "beat" aggressively when analysts are pessimistic,
# and actual EPS comes in above consensus, the crowd refined the forecast.

ref_data <- p[!is.na(flow_imbalance) & !is.na(delta_an)]
cat("Refinement sample:", nrow(ref_data), "\n")

if (nrow(ref_data) >= 20) {
  # Logistic: does flow_imbalance predict P(actual > consensus)?
  ref_data[, beat_consensus := as.numeric(actual_eps > consensus_mean)]

  glm_fit <- glm(beat_consensus ~ flow_imbalance, data = ref_data, family = "binomial")
  cat("\nLogistic: P(actual > consensus) ~ flow_imbalance\n")
  print(summary(glm_fit)$coefficients)

  results$refinement_logistic <- summary(glm_fit)$coefficients

  # Linear: flow_imbalance predicts surprise magnitude?
  m_ref <- feols(delta_an ~ flow_imbalance, data = ref_data)
  cat("\nLinear: delta_an ~ flow_imbalance\n")
  print(coeftable(m_ref))

  results$refinement_linear <- coeftable(m_ref)

  # Tercile sort
  ref_data[, flow_tercile := fcase(
    flow_imbalance <= quantile(flow_imbalance, 1/3), 1L,
    flow_imbalance <= quantile(flow_imbalance, 2/3), 2L,
    default = 3L
  )]

  cat("\nBy flow tercile:\n")
  tercile_tab <- ref_data[, .(
    n = .N,
    mean_flow = mean(flow_imbalance),
    pct_beat_consensus = mean(beat_consensus),
    mean_surprise = mean(-delta_an),  # actual - consensus (positive = beat)
    median_surprise = median(-delta_an),  # TODO: verify median is positive when beat rate > 50%
    mean_xs_1d = mean(excess_return_1d, na.rm = TRUE)
  ), by = flow_tercile][order(flow_tercile)]
  print(tercile_tab)

  # Diagnostic: check if negative mean surprise with high beat rate is outlier-driven
  cat("\nDiagnostic: surprise distribution in bullish tercile (T3):\n")
  t3 <- ref_data[flow_tercile == 3L]
  cat("  Beat consensus:", sum(t3$beat_consensus), "of", nrow(t3), "\n")
  cat("  Mean surprise (actual - consensus):", round(mean(-t3$delta_an), 3), "\n")
  cat("  Median surprise:", round(median(-t3$delta_an), 3), "\n")
  cat("  Min surprise:", round(min(-t3$delta_an), 3), "\n")
  cat("  Max surprise:", round(max(-t3$delta_an), 3), "\n")

  results$refinement_terciles <- tercile_tab
}

# ═══════════════════════════════════════════════════════════════════════════
# FIGURE: Who was closer — distribution of |delta_pm| - |delta_an|
# ═══════════════════════════════════════════════════════════════════════════

pf[, error_diff := abs_delta_pm - abs_delta_an]  # negative = PM closer

g_closer <- ggplot(pf, aes(x = error_diff)) +
  geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7, color = "white") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  labs(x = "|MM Error| - |Analyst Error| (negative = PM closer)",
       y = "Count",
       title = "Who Was Closer to Actual EPS?",
       subtitle = sprintf("PM closer: %.1f%% of %d events",
                          mean(pf$pm_closer) * 100, nrow(pf))) +
  theme_minimal(base_size = 12)
ggsave(file.path(analysis_dir, "fig_who_closer.pdf"), g_closer, width = 7, height = 5)
cat("Saved fig_who_closer.pdf\n")

# ═══════════════════════════════════════════════════════════════════════════
# FIGURE: EPS Attribution Waterfall (4-component)
# Decomposes median gap between analyst consensus and actual EPS into:
#   (1) historical bias (trailing 8-quarter median surprise per firm)
#   (2) PM incremental (implied EPS above bias-corrected baseline)
#   (3) residual (actual - implied)
# ═══════════════════════════════════════════════════════════════════════════

cat("\n=== EPS ATTRIBUTION WATERFALL ===\n")

# Load implied EPS results (already loaded as `implied` above)
# Merge trailing median surprise from ibes_history
history <- readRDS(file.path(data_dir, "ibes_history.rds"))
setDT(history)
history <- history[!is.na(surprise)]

# Non-GAAP events with implied EPS
wf <- implied[accounting_basis == "Non-GAAP" &
              !is.na(implied_eps_t) & !is.na(actual_eps) & !is.na(consensus_mean)]

# Compute trailing 8-quarter median surprise per event
wf[, earnings_date := as.Date(earnings_date)]
history[, fpedats := as.Date(fpedats)]

wf[, typical_beat := {
  tb <- sapply(seq_len(.N), function(i) {
    tk <- ticker[i]; basis <- accounting_basis[i]; ed <- earnings_date[i]
    sub <- history[oftic == tk & accounting_basis == basis & fpedats < ed]
    sub <- tail(sub[order(fpedats)], 8)
    if (nrow(sub) >= 4) median(sub$surprise) else NA_real_
  })
  tb
}]

wf_clean <- wf[!is.na(typical_beat)]
cat("Waterfall sample:", nrow(wf_clean), "Non-GAAP events\n")

# Median decomposition
med_cons     <- median(wf_clean$consensus_mean)
med_walkdown <- median(wf_clean$typical_beat)
med_implied  <- median(wf_clean$implied_eps_t)
med_actual   <- median(wf_clean$actual_eps)
med_corrected <- med_cons + med_walkdown
med_pm_incr   <- med_implied - med_corrected
med_residual  <- med_actual - med_implied
med_total     <- med_actual - med_cons

cat(sprintf("  Consensus: $%.2f\n", med_cons))
cat(sprintf("  Historical bias: +$%.2f (%.0f%%)\n", med_walkdown, med_walkdown / med_total * 100))
cat(sprintf("  PM incremental:  +$%.2f (%.0f%%)\n", med_pm_incr, med_pm_incr / med_total * 100))
cat(sprintf("  Residual:        +$%.2f (%.0f%%)\n", med_residual, med_residual / med_total * 100))
cat(sprintf("  Actual:          $%.2f\n", med_actual))

# Build waterfall data frame for ggplot
wf_df <- data.table(
  label   = factor(c("Analyst\nConsensus", "Historical\nBias", "PM\nIncremental",
                      "Remaining\nGap", "Actual\nEPS"),
                    levels = c("Analyst\nConsensus", "Historical\nBias", "PM\nIncremental",
                               "Remaining\nGap", "Actual\nEPS")),
  ymin    = c(0,        med_cons,      med_corrected, med_implied, 0),
  ymax    = c(med_cons, med_corrected, med_implied,   med_actual,  med_actual),
  fill_grp = c("base", "walkdown", "pm", "residual", "base")
)

g_wf <- ggplot(wf_df, aes(x = label, ymin = ymin, ymax = ymax, fill = fill_grp)) +
  geom_rect(aes(xmin = as.numeric(label) - 0.3, xmax = as.numeric(label) + 0.3),
            color = "white", linewidth = 0.3) +
  # Connector lines
  geom_segment(data = data.table(
    x = c(1.3, 2.3, 3.3, 4.3),
    xend = c(1.7, 2.7, 3.7, 4.7),
    y = c(med_cons, med_corrected, med_implied, med_actual)
  ), aes(x = x, xend = xend, y = y, yend = y),
  inherit.aes = FALSE, color = "grey60", linetype = "dashed", linewidth = 0.4) +
  # Value labels
  annotate("text", x = 1, y = med_cons + 0.015,
           label = sprintf("$%.2f", med_cons), size = 3.5, fontface = "bold") +
  annotate("text", x = 2, y = med_corrected + 0.015,
           label = sprintf("+$%.2f", med_walkdown), size = 3.5, fontface = "bold") +
  annotate("text", x = 3, y = med_implied + 0.015,
           label = sprintf("+$%.2f", med_pm_incr), size = 3.5, fontface = "bold") +
  annotate("text", x = 4, y = med_actual + 0.015,
           label = sprintf("+$%.2f", med_residual), size = 3.5, fontface = "bold") +
  annotate("text", x = 5, y = med_actual + 0.015,
           label = sprintf("$%.2f", med_actual), size = 3.5, fontface = "bold") +
  scale_fill_manual(values = c("base" = "#2C5F8A", "walkdown" = "#D4711A",
                                "pm" = "#2EAA4F", "residual" = "#8AAFCC"),
                    guide = "none") +
  scale_y_continuous(labels = scales::dollar_format(), expand = expansion(mult = c(0, 0.08))) +
  labs(x = NULL, y = "EPS ($)",
       title = "EPS Attribution: Decomposing the Analyst-to-Actual Gap",
       subtitle = sprintf("Median Non-GAAP event (n = %d). Historical bias: $%.2f (%.0f%%); PM incremental: $%.2f (%.0f%%).",
                          nrow(wf_clean), med_walkdown, med_walkdown/med_total*100,
                          med_pm_incr, med_pm_incr/med_total*100)) +
  theme_minimal(base_size = 12)

ggsave(file.path(analysis_dir, "fig_eps_waterfall.pdf"), g_wf, width = 7, height = 5)
cat("Saved fig_eps_waterfall.pdf\n")

# Directional accuracy: does PM-implied adjustment move in the correct direction?
wf_clean[, pm_adj_dir := sign(implied_eps_t - consensus_mean)]
wf_clean[, actual_dir := sign(actual_eps - consensus_mean)]
wf_clean[, dir_correct := pm_adj_dir == actual_dir]
pct_dir_correct <- mean(wf_clean$dir_correct) * 100
cat(sprintf("  PM adjustment correct direction: %.1f%% of %d events\n",
            pct_dir_correct, nrow(wf_clean)))

results$waterfall <- list(
  consensus = med_cons, walkdown = med_walkdown, pm_incr = med_pm_incr,
  residual = med_residual, actual = med_actual, n = nrow(wf_clean),
  pct_dir_correct = pct_dir_correct
)

saveRDS(results, file.path(analysis_dir, "surprise_results.rds"))
cat("\nSaved surprise_results.rds\n")
