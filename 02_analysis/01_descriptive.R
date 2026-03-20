# 01_descriptive.R — Descriptive statistics and distributions
# Input:  build/event_panel.rds
# Output: analysis/descriptive_results.rds, figures

library(data.table)
library(ggplot2)

data_root    <- Sys.getenv("DATA_DIR", file.path(getwd(), "data"))
build_dir    <- file.path(data_root, "build")
analysis_dir <- file.path(data_root, "analysis")

panel <- readRDS(file.path(build_dir, "event_panel.rds"))

# Restrict to events with IBES match
p <- panel[!is.na(actual_eps) & !is.na(consensus_mean)]
cat("Analysis sample:", nrow(p), "events\n")

results <- list()

# ═══════════════════════════════════════════════════════════════════════════
# 1. MM LINE VS ANALYST CONSENSUS
# ═══════════════════════════════════════════════════════════════════════════

cat("\n=== MM LINE VS ANALYST CONSENSUS ===\n")

# Correlation between eps_target and consensus
r <- cor(p$eps_target, p$consensus_mean, use = "complete.obs")
cat("Correlation(eps_target, consensus_mean):", round(r, 3), "\n")

# Regression: eps_target ~ consensus_mean
fit_line <- lm(eps_target ~ consensus_mean, data = p)
cat("\nRegression: eps_target ~ consensus_mean\n")
print(summary(fit_line)$coefficients)

# Distribution of target_consensus_gap_pct
gap <- p[!is.na(target_consensus_gap_pct)]
cat("\nTarget-consensus gap (%):\n")
print(summary(gap$target_consensus_gap_pct * 100))
cat("Line above consensus:", sum(p$line_above_consensus, na.rm = TRUE), "of", nrow(p),
    "(", round(mean(p$line_above_consensus, na.rm = TRUE) * 100, 1), "%)\n")
cat("Line below consensus:", sum(!p$line_above_consensus, na.rm = TRUE), "\n")
cat("Line = consensus (within 1%):", sum(abs(p$target_consensus_gap_pct) < 0.01, na.rm = TRUE), "\n")

results$line_vs_consensus <- list(
  correlation = r,
  reg_coef = coef(fit_line),
  reg_r2 = summary(fit_line)$r.squared,
  gap_summary = summary(gap$target_consensus_gap_pct),
  pct_above = mean(p$line_above_consensus, na.rm = TRUE)
)

# Figure: scatter eps_target vs consensus_mean
g1 <- ggplot(p, aes(x = consensus_mean, y = eps_target)) +
  geom_point(alpha = 0.5, size = 1.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  geom_smooth(method = "lm", se = TRUE, color = "steelblue") +
  labs(x = "Analyst Consensus EPS", y = "PM Market Maker EPS Target",
       title = "Market Maker EPS Line vs Analyst Consensus",
       subtitle = sprintf("r = %.3f, n = %d", r, nrow(p))) +
  theme_minimal(base_size = 12)
ggsave(file.path(analysis_dir, "fig_mm_vs_consensus_scatter.pdf"),
       g1, width = 7, height = 6)
cat("Saved fig_mm_vs_consensus_scatter.pdf\n")

# ═══════════════════════════════════════════════════════════════════════════
# 2. BEAT PROBABILITY DISTRIBUTION
# ═══════════════════════════════════════════════════════════════════════════

cat("\n=== BEAT PROBABILITY ===\n")
bp <- p[!is.na(beat_prob_last)]
cat("Events with beat_prob_last:", nrow(bp), "\n")
print(summary(bp$beat_prob_last))

# How does beat_prob relate to target_vs_consensus?
if (nrow(bp[!is.na(target_vs_consensus)]) > 10) {
  r_bp <- cor(bp$beat_prob_last, bp$target_vs_consensus, use = "complete.obs")
  cat("Corr(beat_prob, target_vs_consensus):", round(r_bp, 3), "\n")
  cat("(Negative = when line is below consensus [easy beat], crowd expects beat)\n")
  results$beat_prob_vs_gap_corr <- r_bp
}

# Figure: histogram of beat_prob_last
g2 <- ggplot(bp, aes(x = beat_prob_last)) +
  geom_histogram(bins = 20, fill = "steelblue", alpha = 0.7, color = "white") +
  geom_vline(xintercept = 0.5, linetype = "dashed", color = "red") +
  labs(x = "Last Pre-Earnings Beat Probability", y = "Count",
       title = "Distribution of PM Beat Probability",
       subtitle = sprintf("n = %d, median = %.2f", nrow(bp), median(bp$beat_prob_last))) +
  theme_minimal(base_size = 12)
ggsave(file.path(analysis_dir, "fig_beat_prob_hist.pdf"), g2, width = 7, height = 5)
cat("Saved fig_beat_prob_hist.pdf\n")

# ═══════════════════════════════════════════════════════════════════════════
# 3. FLOW DECOMPOSITION
# ═══════════════════════════════════════════════════════════════════════════

cat("\n=== FLOW DECOMPOSITION ===\n")
fl <- p[!is.na(flow_imbalance)]
cat("Events with flow_imbalance:", nrow(fl), "\n")
print(summary(fl$flow_imbalance))

cat("\nFlow imbalance > 0 (net beat flow):", sum(fl$flow_imbalance > 0),
    "(", round(mean(fl$flow_imbalance > 0) * 100, 1), "%)\n")

# Correlation: flow_imbalance vs beat_prob_last
if (nrow(fl[!is.na(beat_prob_last)]) > 10) {
  r_flow_bp <- cor(fl$flow_imbalance, fl$beat_prob_last, use = "complete.obs")
  cat("Corr(flow_imbalance, beat_prob_last):", round(r_flow_bp, 3), "\n")
  results$flow_vs_beat_prob_corr <- r_flow_bp
}

# Flow breakdown by cell
cat("\nVolume by cell (mean $):\n")
cat(sprintf("  Buy-Yes:  %s\n", format(round(mean(fl$buy_yes_vol)), big.mark = ",")))
cat(sprintf("  Sell-Yes: %s\n", format(round(mean(fl$sell_yes_vol)), big.mark = ",")))
cat(sprintf("  Buy-No:   %s\n", format(round(mean(fl$buy_no_vol)), big.mark = ",")))
cat(sprintf("  Sell-No:  %s\n", format(round(mean(fl$sell_no_vol)), big.mark = ",")))

# Figure: histogram of flow_imbalance
g3 <- ggplot(fl, aes(x = flow_imbalance)) +
  geom_histogram(bins = 25, fill = "darkgreen", alpha = 0.7, color = "white") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  labs(x = "Flow Imbalance (net beat flow / total volume)",
       y = "Count",
       title = "Distribution of PM Flow Imbalance",
       subtitle = sprintf("n = %d, median = %.3f", nrow(fl),
                          median(fl$flow_imbalance))) +
  theme_minimal(base_size = 12)
ggsave(file.path(analysis_dir, "fig_flow_imbalance_hist.pdf"), g3, width = 7, height = 5)
cat("Saved fig_flow_imbalance_hist.pdf\n")

# ═══════════════════════════════════════════════════════════════════════════
# 4. SUMMARY STATISTICS TABLE
# ═══════════════════════════════════════════════════════════════════════════

cat("\n=== SUMMARY STATISTICS ===\n")

# By GAAP/non-GAAP
cat("\nBy EPS type:\n")
by_type <- p[, .(
  n          = .N,
  n_tickers  = uniqueN(ticker),
  med_analysts = median(as.numeric(num_analysts), na.rm = TRUE),
  med_pm_vol = median(total_pm_volume, na.rm = TRUE),
  med_n_trades = median(as.numeric(n_trades), na.rm = TRUE),
  med_beat_prob = median(beat_prob_last, na.rm = TRUE),
  pct_actual_beat = mean(actual_beat, na.rm = TRUE)
), by = eps_type]
print(by_type)

# Overall
cat("\nOverall:\n")
cat("Events:", nrow(p), "\n")
cat("Unique tickers:", uniqueN(p$ticker), "\n")
cat("Date range:", as.character(range(p$earnings_date, na.rm = TRUE)), "\n")
cat("Median analyst coverage:", median(p$num_analysts, na.rm = TRUE), "\n")
cat("Median PM volume:", format(round(median(p$total_pm_volume, na.rm = TRUE)),
                                big.mark = ","), "\n")
cat("Actual beat rate:", round(mean(p$actual_beat, na.rm = TRUE) * 100, 1), "%\n")

results$by_type <- by_type
results$n_events <- nrow(p)
results$n_tickers <- uniqueN(p$ticker)

# ═══════════════════════════════════════════════════════════════════════════
# 5. TIMING GAPS — CONSENSUS STALENESS VS PM ACTIVITY (Rev 1)
# ═══════════════════════════════════════════════════════════════════════════
#
# IBES Summary updates monthly (statpers = Thursday before 3rd Friday).
# Analysts face a practical quiet period 1-3 weeks before earnings per Reg FD.
# The PM trades continuously — this is a structural timing advantage.

cat("\n=== TIMING GAPS: CONSENSUS STALENESS VS PM ACTIVITY ===\n")

gap_vars <- c("days_consensus_to_earnings", "days_pm_open_to_earnings",
              "days_consensus_to_pm_close")
gap_labels <- c("Consensus → Earnings", "PM Open → Earnings",
                "Consensus → PM Close")

# Summary stats table
timing_stats <- lapply(seq_along(gap_vars), function(j) {
  v <- gap_vars[j]
  if (!v %in% names(p)) return(NULL)
  vals <- p[[v]]
  vals <- vals[!is.na(vals)]
  if (length(vals) == 0) return(NULL)
  data.table(
    variable = gap_labels[j],
    n        = length(vals),
    mean     = round(mean(vals), 1),
    median   = round(median(vals), 1),
    min      = min(vals),
    max      = max(vals),
    sd       = round(sd(vals), 1)
  )
})
timing_table <- rbindlist(timing_stats[!vapply(timing_stats, is.null, logical(1))])

if (nrow(timing_table) > 0) {
  cat("\nTimeline: Consensus Staleness vs Prediction Market Activity\n")
  print(timing_table)
  results$timing_gaps <- timing_table

  # Figure: histogram of timing gaps
  gap_data <- lapply(seq_along(gap_vars), function(j) {
    v <- gap_vars[j]
    if (!v %in% names(p)) return(NULL)
    data.table(
      variable = gap_labels[j],
      days = p[[v]]
    )
  })
  gap_long <- rbindlist(gap_data[!vapply(gap_data, is.null, logical(1))])
  gap_long <- gap_long[!is.na(days)]

  g_timing <- ggplot(gap_long, aes(x = days, fill = variable)) +
    geom_histogram(bins = 25, alpha = 0.7, color = "white", position = "identity") +
    facet_wrap(~variable, ncol = 1, scales = "free_y") +
    labs(x = "Days", y = "Count",
         title = "Timeline: Consensus Staleness vs PM Activity",
         subtitle = paste0("Consensus last updated median ",
                           timing_table[variable == "Consensus → Earnings", median],
                           " days before earnings; PM trades continuously")) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "none")
  ggsave(file.path(analysis_dir, "fig_timing_gaps.pdf"), g_timing, width = 7, height = 8)
  cat("Saved fig_timing_gaps.pdf\n")
} else {
  cat("No timing gap variables found in panel\n")
}

saveRDS(results, file.path(analysis_dir, "descriptive_results.rds"))
cat("\nSaved descriptive_results.rds\n")
