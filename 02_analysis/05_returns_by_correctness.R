# 05_returns_by_correctness.R — Post-earnings returns conditional on PM crowd accuracy
# Input:  build/event_panel.rds
# Output: analysis/correctness_results.rds, figures
#
# Key question: How large are stock returns when PM crowd expectations are
# correct vs incorrect? Both long (correct beat) and short (correct miss)
# sides are economically meaningful.

library(data.table)
library(ggplot2)
library(fixest)

data_dir     <- "~/Documents/git/corrr/390_paper/data"
analysis_dir <- "~/Documents/data/corrr/390_paper/analysis"

panel <- readRDS(file.path(data_dir, "event_panel.rds"))
p <- panel[!is.na(actual_eps) & !is.na(consensus_mean) & !is.na(beat_prob_last) &
           !is.na(excess_return_1d) & !is.na(excess_return_5d) &
           !is.na(excess_return_10d) & !is.na(flow_imbalance) & !is.na(num_analysts)]
cat("Sample:", nrow(p), "events\n")

results <- list()

# Helpers
t_stat <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) < 3) return(NA_real_)
  mean(x) / (sd(x) / sqrt(length(x)))
}

winsorize <- function(x, lo = 0.02, hi = 0.98) {
  q <- quantile(x, probs = c(lo, hi), na.rm = TRUE)
  pmin(pmax(x, q[1]), q[2])
}

# ═══════════════════════════════════════════════════════════════════════════
# 1. DIRECTION × CORRECTNESS: THE 4-CELL TABLE
# ═══════════════════════════════════════════════════════════════════════════

cat("\n=== DIRECTION × CORRECTNESS ===\n")

p[, prediction_outcome := fcase(
  crowd_predicted_beat == TRUE  & actual_beat == TRUE,  "Predicted beat, correct",
  crowd_predicted_beat == TRUE  & actual_beat == FALSE, "Predicted beat, WRONG",
  crowd_predicted_beat == FALSE & actual_beat == FALSE, "Predicted miss, correct",
  crowd_predicted_beat == FALSE & actual_beat == TRUE,  "Predicted miss, WRONG"
)]

four_cell <- p[!is.na(excess_return_1d) & !is.na(prediction_outcome), .(
  n           = .N,
  mean_1d_pct = mean(excess_return_1d) * 100,
  med_1d_pct  = median(excess_return_1d) * 100,
  t_1d        = t_stat(excess_return_1d),
  mean_5d_pct = mean(excess_return_5d, na.rm = TRUE) * 100,
  med_5d_pct  = median(excess_return_5d, na.rm = TRUE) * 100,
  t_5d        = t_stat(excess_return_5d),
  mean_10d_pct = if ("excess_return_10d" %in% names(.SD))
                   mean(excess_return_10d, na.rm = TRUE) * 100
                 else NA_real_,
  t_10d       = if ("excess_return_10d" %in% names(.SD))
                  t_stat(excess_return_10d)
                else NA_real_,
  hit_rate    = mean(excess_return_1d > 0)
), by = prediction_outcome][order(-mean_1d_pct)]

cat("4-cell returns (%):\n")
print(four_cell)
results$four_cell <- four_cell

# Figure: bar chart of 4-cell returns
four_cell[, outcome_label := factor(prediction_outcome,
  levels = c("Predicted beat, correct", "Predicted beat, WRONG",
             "Predicted miss, WRONG", "Predicted miss, correct"))]

g1 <- ggplot(four_cell, aes(x = outcome_label, y = mean_1d_pct,
                             fill = grepl("correct", prediction_outcome))) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  geom_errorbar(aes(ymin = mean_1d_pct - 1.96 * abs(mean_1d_pct / t_1d),
                    ymax = mean_1d_pct + 1.96 * abs(mean_1d_pct / t_1d)),
                width = 0.3) +
  geom_text(aes(label = sprintf("n=%d\nt=%.2f", n, t_1d)),
            vjust = ifelse(four_cell$mean_1d_pct > 0, -0.8, 1.5), size = 3) +
  scale_fill_manual(values = c("TRUE" = "steelblue", "FALSE" = "firebrick3")) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "", y = "Mean 1-Day Excess Return (%)",
       title = "Post-Earnings Returns by PM Crowd Prediction Accuracy",
       subtitle = sprintf("Correct predictions: long beat (%+.1f%%) and short miss (%+.1f%%)",
                          four_cell[prediction_outcome == "Predicted beat, correct", mean_1d_pct],
                          four_cell[prediction_outcome == "Predicted miss, correct", mean_1d_pct])) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 15, hjust = 1))
ggsave(file.path(analysis_dir, "fig_four_cell_returns.pdf"), g1, width = 8, height = 6)
cat("Saved fig_four_cell_returns.pdf\n")

# ═══════════════════════════════════════════════════════════════════════════
# 2. CONVICTION STRENGTH × CORRECTNESS
# ═══════════════════════════════════════════════════════════════════════════

cat("\n=== CONVICTION × CORRECTNESS ===\n")

p[, conviction := abs(beat_prob_last - 0.5)]
p[, conviction_group := fcase(
  conviction < 0.15, "Low (<65%)",
  conviction < 0.35, "Medium (65-85%)",
  default = "High (>85%)"
)]

conv_ret <- p[!is.na(excess_return_1d), .(
  n           = .N,
  pct_correct = mean(crowd_correct, na.rm = TRUE) * 100,
  mean_1d_pct = mean(excess_return_1d) * 100,
  mean_5d_pct = mean(excess_return_5d, na.rm = TRUE) * 100,
  t_1d        = t_stat(excess_return_1d)
), by = .(conviction_group, crowd_correct)][order(conviction_group, -crowd_correct)]

cat("By conviction × correctness:\n")
print(conv_ret)
results$conviction <- conv_ret

# ═══════════════════════════════════════════════════════════════════════════
# 3. LONG/SHORT STRATEGY SIMULATION
# ═══════════════════════════════════════════════════════════════════════════

cat("\n=== LONG/SHORT STRATEGY ===\n")

# Long: crowd predicts beat with high conviction (beat_prob > 0.7)
# Short: crowd predicts miss with high conviction (beat_prob < 0.3)
p[, strategy_side := fcase(
  beat_prob_last > 0.7, "long",
  beat_prob_last < 0.3, "short",
  default = "no_trade"
)]

# For shorts, return is -1 × excess_return (profit from decline)
p[, strategy_return_1d := fcase(
  strategy_side == "long",  excess_return_1d,
  strategy_side == "short", -excess_return_1d,
  default = NA_real_
)]
p[, strategy_return_5d := fcase(
  strategy_side == "long",  excess_return_5d,
  strategy_side == "short", -excess_return_5d,
  default = NA_real_
)]

strat <- p[strategy_side != "no_trade" & !is.na(strategy_return_1d)]
cat("Strategy universe:", nrow(strat), "events\n")
cat("  Long:", sum(strat$strategy_side == "long"), "\n")
cat("  Short:", sum(strat$strategy_side == "short"), "\n")

# Day 10 strategy returns (Rev 3)
if ("excess_return_10d" %in% names(p)) {
  p[, strategy_return_10d := fcase(
    strategy_side == "long",  excess_return_10d,
    strategy_side == "short", -excess_return_10d,
    default = NA_real_
  )]
} else {
  p[, strategy_return_10d := NA_real_]
}

strat_summary <- strat[, .(
  n           = .N,
  pct_correct = mean(crowd_correct, na.rm = TRUE) * 100,
  mean_1d_pct = mean(strategy_return_1d) * 100,
  med_1d_pct  = median(strategy_return_1d) * 100,
  t_1d        = t_stat(strategy_return_1d),
  mean_5d_pct = mean(strategy_return_5d, na.rm = TRUE) * 100,
  t_5d        = t_stat(strategy_return_5d),
  mean_10d_pct = if ("strategy_return_10d" %in% names(.SD))
                   mean(strategy_return_10d, na.rm = TRUE) * 100
                 else NA_real_,
  t_10d       = if ("strategy_return_10d" %in% names(.SD))
                  t_stat(strategy_return_10d)
                else NA_real_,
  hit_rate    = mean(strategy_return_1d > 0) * 100
), by = strategy_side]

cat("\nStrategy returns (%):\n")
print(strat_summary)

# Combined L/S
combined <- strat[, .(
  n           = .N,
  mean_1d_pct = mean(strategy_return_1d) * 100,
  med_1d_pct  = median(strategy_return_1d) * 100,
  t_1d        = t_stat(strategy_return_1d),
  mean_5d_pct = mean(strategy_return_5d, na.rm = TRUE) * 100,
  t_5d        = t_stat(strategy_return_5d),
  hit_rate    = mean(strategy_return_1d > 0) * 100
)]
cat("\nCombined L/S:\n")
print(combined)

results$strategy <- list(by_side = strat_summary, combined = combined)

# ═══════════════════════════════════════════════════════════════════════════
# 3b. SHORT-ONLY STRATEGY (Rev 3)
# Professor noticed significant negative drift for miss predictions
# (-4.33% by day 5, t = -2.69). Alpha may be concentrated on short side.
# ═══════════════════════════════════════════════════════════════════════════

cat("\n=== SHORT-ONLY STRATEGY ===\n")

# Test multiple thresholds
short_thresholds <- c(0.20, 0.25, 0.30, 0.35)

short_results_list <- lapply(short_thresholds, function(thresh) {
  short_universe <- p[beat_prob_last < thresh & !is.na(excess_return_1d)]
  if (nrow(short_universe) < 3) {
    return(data.table(threshold = thresh, n = nrow(short_universe),
                      mean_1d_pct = NA_real_, t_1d = NA_real_,
                      mean_5d_pct = NA_real_, t_5d = NA_real_,
                      mean_10d_pct = NA_real_, t_10d = NA_real_,
                      hit_rate_1d = NA_real_))
  }
  # Short profits from decline: -1 * excess_return
  data.table(
    threshold    = thresh,
    n            = nrow(short_universe),
    mean_1d_pct  = mean(-short_universe$excess_return_1d) * 100,
    t_1d         = t_stat(-short_universe$excess_return_1d),
    mean_5d_pct  = mean(-short_universe$excess_return_5d, na.rm = TRUE) * 100,
    t_5d         = t_stat(-short_universe$excess_return_5d),
    mean_10d_pct = if ("excess_return_10d" %in% names(short_universe))
                     mean(-short_universe$excess_return_10d, na.rm = TRUE) * 100
                   else NA_real_,
    t_10d        = if ("excess_return_10d" %in% names(short_universe))
                     t_stat(-short_universe$excess_return_10d)
                   else NA_real_,
    hit_rate_1d  = mean(short_universe$excess_return_1d < 0) * 100
  )
})

short_table <- rbindlist(short_results_list)
cat("Short-Only Strategy Returns by Threshold:\n")
print(short_table)
results$short_only <- short_table

# Detailed short-only at primary threshold (0.30)
short_30 <- p[beat_prob_last < 0.30 & !is.na(excess_return_1d)]
if (nrow(short_30) >= 3) {
  cat(sprintf("\nShort-only (prob < 0.30): n=%d, mean 1d=%.2f%% (t=%.2f), 5d=%.2f%% (t=%.2f), hit=%.1f%%\n",
              nrow(short_30),
              mean(-short_30$excess_return_1d) * 100,
              t_stat(-short_30$excess_return_1d),
              mean(-short_30$excess_return_5d, na.rm = TRUE) * 100,
              t_stat(-short_30$excess_return_5d),
              mean(short_30$excess_return_1d < 0) * 100))

  # Compare to L/S strategy
  cat("\nComparison to L/S strategy:\n")
  cat(sprintf("  L/S combined:  mean 1d=%.2f%%, hit=%.1f%%\n",
              combined$mean_1d_pct, combined$hit_rate))
  cat(sprintf("  Short-only:    mean 1d=%.2f%%, hit=%.1f%%\n",
              mean(-short_30$excess_return_1d) * 100,
              mean(short_30$excess_return_1d < 0) * 100))
}

# ═══════════════════════════════════════════════════════════════════════════
# 4. FLOW-ENHANCED STRATEGY (LONG HIGH-FLOW BEATS, SHORT LOW-FLOW MISSES)
# ═══════════════════════════════════════════════════════════════════════════

cat("\n=== FLOW-ENHANCED STRATEGY ===\n")

# Long: beat_prob > 0.7 AND flow_imbalance > median
# Short: beat_prob < 0.3 AND flow_imbalance < median
flow_med <- median(p$flow_imbalance, na.rm = TRUE)

p[, flow_strategy := fcase(
  beat_prob_last > 0.7 & flow_imbalance > flow_med, "long (beat + bullish flow)",
  beat_prob_last < 0.3 & flow_imbalance < flow_med, "short (miss + bearish flow)",
  default = "no_trade"
)]

p[, flow_strat_ret_1d := fcase(
  flow_strategy == "long (beat + bullish flow)",   excess_return_1d,
  flow_strategy == "short (miss + bearish flow)", -excess_return_1d,
  default = NA_real_
)]
p[, flow_strat_ret_5d := fcase(
  flow_strategy == "long (beat + bullish flow)",   excess_return_5d,
  flow_strategy == "short (miss + bearish flow)", -excess_return_5d,
  default = NA_real_
)]

flow_strat <- p[flow_strategy != "no_trade" & !is.na(flow_strat_ret_1d)]

flow_strat_summary <- flow_strat[, .(
  n           = .N,
  pct_correct = mean(crowd_correct, na.rm = TRUE) * 100,
  mean_1d_pct = mean(flow_strat_ret_1d) * 100,
  med_1d_pct  = median(flow_strat_ret_1d) * 100,
  t_1d        = t_stat(flow_strat_ret_1d),
  mean_5d_pct = mean(flow_strat_ret_5d, na.rm = TRUE) * 100,
  t_5d        = t_stat(flow_strat_ret_5d),
  hit_rate    = mean(flow_strat_ret_1d > 0) * 100
), by = flow_strategy]

cat("Flow-enhanced strategy:\n")
print(flow_strat_summary)

flow_combined <- flow_strat[, .(
  n           = .N,
  mean_1d_pct = mean(flow_strat_ret_1d) * 100,
  t_1d        = t_stat(flow_strat_ret_1d),
  mean_5d_pct = mean(flow_strat_ret_5d, na.rm = TRUE) * 100,
  t_5d        = t_stat(flow_strat_ret_5d),
  hit_rate    = mean(flow_strat_ret_1d > 0) * 100
)]
cat("\nFlow-enhanced combined L/S:\n")
print(flow_combined)

results$flow_strategy <- list(by_side = flow_strat_summary, combined = flow_combined)

# ═══════════════════════════════════════════════════════════════════════════
# 5. CUMULATIVE DRIFT: DO RETURNS PERSIST?
# ═══════════════════════════════════════════════════════════════════════════

cat("\n=== RETURN PERSISTENCE ===\n")

# Compare 1d vs 5d for each cell
drift <- p[!is.na(excess_return_1d) & !is.na(excess_return_5d) &
           !is.na(prediction_outcome), .(
  n = .N,
  day1_pct = mean(excess_return_1d) * 100,
  day5_pct = mean(excess_return_5d) * 100,
  day10_pct = if ("excess_return_10d" %in% names(.SD))
                mean(excess_return_10d, na.rm = TRUE) * 100
              else NA_real_,
  drift_2_to_5 = (mean(excess_return_5d) - mean(excess_return_1d)) * 100,
  drift_6_to_10 = if ("excess_return_10d" %in% names(.SD))
                    (mean(excess_return_10d, na.rm = TRUE) - mean(excess_return_5d)) * 100
                  else NA_real_
), by = prediction_outcome][order(-day1_pct)]

cat("Return drift (day 1 vs day 5, %):\n")
print(drift)
results$drift <- drift

# ═══════════════════════════════════════════════════════════════════════════
# 6. FIGURES
# ═══════════════════════════════════════════════════════════════════════════

# Figure: 1d and 5d returns side by side
drift_long <- melt(drift, id.vars = c("prediction_outcome", "n"),
                   measure.vars = c("day1_pct", "day5_pct"),
                   variable.name = "horizon", value.name = "return_pct")
drift_long[, horizon := fifelse(horizon == "day1_pct", "1-Day", "5-Day")]
drift_long[, outcome_label := factor(prediction_outcome,
  levels = c("Predicted beat, correct", "Predicted beat, WRONG",
             "Predicted miss, WRONG", "Predicted miss, correct"))]

g2 <- ggplot(drift_long, aes(x = outcome_label, y = return_pct, fill = horizon)) +
  geom_col(position = "dodge", alpha = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_fill_manual(values = c("1-Day" = "steelblue", "5-Day" = "darkblue")) +
  labs(x = "", y = "Mean Excess Return (%)", fill = "Horizon",
       title = "Post-Earnings Return Drift by PM Prediction Outcome",
       subtitle = sprintf("Correct miss calls: %+.1f%% day 1, continuing to %+.1f%% by day 5",
                          drift[prediction_outcome == "Predicted miss, correct", day1_pct],
                          drift[prediction_outcome == "Predicted miss, correct", day5_pct])) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 15, hjust = 1))
ggsave(file.path(analysis_dir, "fig_return_drift.pdf"), g2, width = 9, height = 6)
cat("Saved fig_return_drift.pdf\n")

# Figure: Return distributions for long vs short side
strat_for_plot <- p[strategy_side != "no_trade" & !is.na(excess_return_1d)]
strat_for_plot[, side_label := fifelse(strategy_side == "long",
  "Long (crowd expects beat)", "Short (crowd expects miss)")]

g3 <- ggplot(strat_for_plot, aes(x = excess_return_1d * 100, fill = side_label)) +
  geom_histogram(bins = 30, alpha = 0.7, color = "white", position = "identity") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_wrap(~side_label, ncol = 1, scales = "free_y") +
  scale_fill_manual(values = c("Long (crowd expects beat)" = "steelblue",
                                "Short (crowd expects miss)" = "firebrick3")) +
  labs(x = "1-Day Excess Return (%)", y = "Count",
       title = "Return Distributions: Long Beats vs Short Misses",
       subtitle = sprintf("Long: mean +%.1f%% (n=%d) | Short: mean +%.1f%% profit (n=%d)",
                          strat_summary[strategy_side == "long", mean_1d_pct],
                          strat_summary[strategy_side == "long", n],
                          strat_summary[strategy_side == "short", mean_1d_pct],
                          strat_summary[strategy_side == "short", n])) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")
ggsave(file.path(analysis_dir, "fig_long_short_distributions.pdf"), g3, width = 8, height = 7)
cat("Saved fig_long_short_distributions.pdf\n")

# Figure: Conviction vs returns scatter
p_conv <- p[!is.na(excess_return_1d)]
p_conv[, signed_conviction := fifelse(crowd_predicted_beat, conviction, -conviction)]

g4 <- ggplot(p_conv, aes(x = beat_prob_last, y = excess_return_1d * 100,
                           color = crowd_correct)) +
  geom_point(alpha = 0.5, size = 2) +
  geom_smooth(method = "lm", se = TRUE, color = "black", linewidth = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0.5, linetype = "dotted", color = "gray50") +
  scale_color_manual(values = c("TRUE" = "steelblue", "FALSE" = "firebrick3"),
                     labels = c("TRUE" = "Crowd correct", "FALSE" = "Crowd wrong")) +
  labs(x = "PM Beat Probability", y = "1-Day Excess Return (%)",
       title = "Beat Probability vs Post-Earnings Returns",
       subtitle = "Higher conviction in correct direction → larger returns",
       color = "") +
  theme_minimal(base_size = 12)
ggsave(file.path(analysis_dir, "fig_conviction_vs_returns.pdf"), g4, width = 8, height = 6)
cat("Saved fig_conviction_vs_returns.pdf\n")

# ═══════════════════════════════════════════════════════════════════════════
# SUMMARY
# ═══════════════════════════════════════════════════════════════════════════

cat("\n=== KEY FINDINGS ===\n")
cat("1. Correct beat prediction: +",
    round(four_cell[prediction_outcome == "Predicted beat, correct", mean_1d_pct], 2),
    "% day 1 (n=", four_cell[prediction_outcome == "Predicted beat, correct", n], ")\n")
cat("2. Correct miss prediction: ",
    round(four_cell[prediction_outcome == "Predicted miss, correct", mean_1d_pct], 2),
    "% day 1, ",
    round(four_cell[prediction_outcome == "Predicted miss, correct", mean_5d_pct], 2),
    "% day 5 (n=", four_cell[prediction_outcome == "Predicted miss, correct", n], ")\n")
cat("3. L/S strategy (prob > 0.7 / < 0.3): ",
    round(combined$mean_1d_pct, 2), "% day 1, ",
    round(combined$hit_rate, 1), "% hit rate\n")
cat("4. Flow-enhanced L/S: ",
    round(flow_combined$mean_1d_pct, 2), "% day 1, ",
    round(flow_combined$hit_rate, 1), "% hit rate\n")

saveRDS(results, file.path(analysis_dir, "correctness_results.rds"))
cat("\nSaved correctness_results.rds\n")
