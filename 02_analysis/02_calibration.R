# 02_calibration.R — Calibration curves & Brier scores for PM crowd vs analysts
# Input:  build/event_panel.rds
# Output: analysis/calibration_results.rds, analysis/fig_calibration.pdf

library(data.table)
library(ggplot2)

data_root    <- Sys.getenv("DATA_DIR", file.path(getwd(), "data"))
build_dir    <- file.path(data_root, "build")
analysis_dir <- file.path(data_root, "analysis")

panel <- readRDS(file.path(build_dir, "event_panel.rds"))
p <- panel[!is.na(actual_eps) & !is.na(consensus_mean) & !is.na(beat_prob_last)]
cat("Calibration sample:", nrow(p), "events\n")

results <- list()

# ═══════════════════════════════════════════════════════════════════════════
# 1. PM CROWD CALIBRATION CURVE
# ═══════════════════════════════════════════════════════════════════════════

cat("\n=== PM CROWD CALIBRATION ===\n")

# Bin by beat_prob_last into 5 bins
p[, prob_bin := cut(beat_prob_last,
                    breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1.0),
                    labels = c("0-20%", "20-40%", "40-60%", "60-80%", "80-100%"),
                    include.lowest = TRUE)]

calib_pm <- p[!is.na(prob_bin), .(
  n           = .N,
  mean_prob   = mean(beat_prob_last),
  actual_freq = mean(as.numeric(actual_beat))
), by = prob_bin][order(prob_bin)]

cat("PM crowd calibration:\n")
print(calib_pm)

results$calib_pm <- calib_pm

# ═══════════════════════════════════════════════════════════════════════════
# 2. ANALYST CALIBRATION
# ═══════════════════════════════════════════════════════════════════════════

cat("\n=== ANALYST CALIBRATION ===\n")

# When analysts imply beat (consensus > target), what % actually beat?
p_analysts_beat <- p[consensus_predicted_beat == TRUE]
p_analysts_miss <- p[consensus_predicted_beat == FALSE]

cat("Analysts predict beat:", nrow(p_analysts_beat),
    "— actual beat rate:", round(mean(p_analysts_beat$actual_beat, na.rm = TRUE) * 100, 1), "%\n")
cat("Analysts predict miss:", nrow(p_analysts_miss),
    "— actual beat rate:", round(mean(p_analysts_miss$actual_beat, na.rm = TRUE) * 100, 1), "%\n")

results$analyst_calib <- list(
  predict_beat_n = nrow(p_analysts_beat),
  predict_beat_actual = mean(p_analysts_beat$actual_beat, na.rm = TRUE),
  predict_miss_n = nrow(p_analysts_miss),
  predict_miss_actual = mean(p_analysts_miss$actual_beat, na.rm = TRUE)
)

# ═══════════════════════════════════════════════════════════════════════════
# 3. FLOW-BASED CALIBRATION
# ═══════════════════════════════════════════════════════════════════════════

cat("\n=== FLOW-BASED CALIBRATION ===\n")

p_flow <- p[!is.na(flow_imbalance)]
p_flow[, flow_bin := cut(flow_imbalance,
                         breaks = quantile(flow_imbalance, probs = seq(0, 1, 0.2)),
                         labels = paste0("Q", 1:5),
                         include.lowest = TRUE)]

calib_flow <- p_flow[!is.na(flow_bin), .(
  n           = .N,
  mean_flow   = mean(flow_imbalance),
  actual_freq = mean(as.numeric(actual_beat))
), by = flow_bin][order(flow_bin)]

cat("Flow-based calibration:\n")
print(calib_flow)

results$calib_flow <- calib_flow

# ═══════════════════════════════════════════════════════════════════════════
# 4. BRIER SCORES
# ═══════════════════════════════════════════════════════════════════════════

cat("\n=== BRIER SCORES ===\n")

# PM crowd Brier score
actual_binary <- as.numeric(p$actual_beat)
brier_pm <- mean((p$beat_prob_last - actual_binary)^2, na.rm = TRUE)
cat("PM crowd Brier score:", round(brier_pm, 4), "\n")

# Analyst-implied probability: model from (consensus - target) / dispersion
# Simple sigmoid: P(beat) = pnorm((consensus - target) / stdev)
p[, analyst_implied_prob := fifelse(
  !is.na(consensus_stdev) & consensus_stdev > 0.001,
  pnorm((consensus_mean - eps_target) / consensus_stdev),
  fifelse(consensus_mean > eps_target, 0.9, 0.1)  # fallback
)]

brier_an <- mean((p$analyst_implied_prob - actual_binary)^2, na.rm = TRUE)
cat("Analyst-implied Brier score:", round(brier_an, 4), "\n")

# Naive baseline: always predict the sample mean
brier_naive <- mean((mean(actual_binary) - actual_binary)^2)
cat("Naive baseline Brier:", round(brier_naive, 4), "\n")

# Brier skill score (relative to naive)
bss_pm <- 1 - brier_pm / brier_naive
bss_an <- 1 - brier_an / brier_naive
cat("PM Brier Skill Score:", round(bss_pm, 4), "\n")
cat("Analyst Brier Skill Score:", round(bss_an, 4), "\n")

results$brier <- list(
  pm = brier_pm, analyst = brier_an, naive = brier_naive,
  bss_pm = bss_pm, bss_an = bss_an
)

# ═══════════════════════════════════════════════════════════════════════════
# 5. ΔPM VS ΔAN COMPARISON
# ═══════════════════════════════════════════════════════════════════════════

cat("\n=== FORECAST ERROR COMPARISON ===\n")

pf <- p[!is.na(delta_pm) & !is.na(delta_an)]
cat("Events with both errors:", nrow(pf), "\n")

mae_pm <- mean(pf$abs_delta_pm)
mae_an <- mean(pf$abs_delta_an)
cat("MAE (MM line):", round(mae_pm, 4), "\n")
cat("MAE (analysts):", round(mae_an, 4), "\n")
cat("PM closer:", sum(pf$pm_closer), "of", nrow(pf),
    "(", round(mean(pf$pm_closer) * 100, 1), "%)\n")

# Paired t-test: abs_delta_pm vs abs_delta_an
if (nrow(pf) >= 5) {
  tt <- t.test(pf$abs_delta_pm, pf$abs_delta_an, paired = TRUE)
  cat("Paired t-test (PM - AN):", round(tt$statistic, 3),
      "p =", format.pval(tt$p.value, digits = 3), "\n")
  results$paired_test <- list(
    t_stat = as.numeric(tt$statistic),
    p_value = tt$p.value,
    mean_diff = tt$estimate
  )
}

# By GAAP/non-GAAP
cat("\nBy EPS type:\n")
by_type <- pf[, .(
  n = .N,
  mae_pm = mean(abs_delta_pm),
  mae_an = mean(abs_delta_an),
  pct_pm_closer = mean(pm_closer)
), by = eps_type]
print(by_type)

results$mae <- list(pm = mae_pm, an = mae_an, by_type = by_type)

# ═══════════════════════════════════════════════════════════════════════════
# 6. CALIBRATION FIGURE
# ═══════════════════════════════════════════════════════════════════════════

# Combine PM and analyst calibration for plot
calib_plot <- copy(calib_pm)
calib_plot[, source := "PM Crowd"]
setnames(calib_plot, "mean_prob", "predicted")
setnames(calib_plot, "actual_freq", "actual")

# For analyst, create bins from analyst_implied_prob
p[, analyst_bin := cut(analyst_implied_prob,
                       breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1.0),
                       labels = c("0-20%", "20-40%", "40-60%", "60-80%", "80-100%"),
                       include.lowest = TRUE)]

calib_an <- p[!is.na(analyst_bin), .(
  n         = .N,
  predicted = mean(analyst_implied_prob),
  actual    = mean(as.numeric(actual_beat))
), by = .(prob_bin = analyst_bin)]
calib_an[, source := "Analysts"]

calib_both <- rbindlist(list(calib_plot[, .(prob_bin, n, predicted, actual, source)],
                             calib_an), use.names = TRUE)

g_calib <- ggplot(calib_both, aes(x = predicted, y = actual, color = source)) +
  geom_point(aes(size = n)) +
  geom_line() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
  scale_size_continuous(range = c(2, 8)) +
  labs(x = "Predicted Beat Probability", y = "Actual Beat Frequency",
       title = "Calibration: PM Crowd vs Analyst Consensus",
       subtitle = sprintf("PM Brier = %.3f, Analyst Brier = %.3f (n = %d)",
                          brier_pm, brier_an, nrow(p)),
       color = "Source", size = "Events") +
  coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
  theme_minimal(base_size = 12)
ggsave(file.path(analysis_dir, "fig_calibration.pdf"), g_calib, width = 7, height = 6)
cat("\nSaved fig_calibration.pdf\n")

saveRDS(results, file.path(analysis_dir, "calibration_results.rds"))
cat("Saved calibration_results.rds\n")
