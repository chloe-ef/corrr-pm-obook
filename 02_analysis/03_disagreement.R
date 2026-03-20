# 03_disagreement.R — Crowd vs analyst disagreement analysis
# Input:  build/event_panel.rds
# Output: analysis/disagreement_results.rds, figures

library(data.table)
library(ggplot2)
library(fixest)

build_dir    <- "~/Documents/data/corrr/390_paper/build"
analysis_dir <- "~/Documents/data/corrr/390_paper/analysis"

panel <- readRDS(file.path(build_dir, "event_panel.rds"))
p <- panel[!is.na(actual_eps) & !is.na(consensus_mean) & !is.na(beat_prob_last)]
cat("Disagreement sample:", nrow(p), "events\n")

results <- list()

# Helper functions
t_stat <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) < 3) return(NA_real_)
  mean(x) / (sd(x) / sqrt(length(x)))
}

ic_spearman <- function(signal, ret) {
  ok <- !is.na(signal) & !is.na(ret)
  if (sum(ok) < 5) return(NA_real_)
  cor(rank(signal[ok]), ret[ok], method = "spearman")
}

# ═══════════════════════════════════════════════════════════════════════════
# 1. DISAGREEMENT PREVALENCE — 2×2 TABLE
# ═══════════════════════════════════════════════════════════════════════════

cat("\n=== DISAGREEMENT PREVALENCE ===\n")

p[, crowd_beat := crowd_predicted_beat]
p[, analyst_beat := consensus_predicted_beat]

crosstab <- p[!is.na(crowd_beat) & !is.na(analyst_beat),
              .N, by = .(crowd_beat, analyst_beat)]
cat("2x2 table (crowd_predicted_beat × consensus_predicted_beat):\n")
print(dcast(crosstab, crowd_beat ~ analyst_beat, value.var = "N", fill = 0))

agree <- p[crowd_beat == analyst_beat]
disagree <- p[crowd_beat != analyst_beat]
cat("\nAgreement:", nrow(agree), "(", round(nrow(agree) / nrow(p) * 100, 1), "%)\n")
cat("Disagreement:", nrow(disagree), "(", round(nrow(disagree) / nrow(p) * 100, 1), "%)\n")

results$crosstab <- crosstab
results$n_agree <- nrow(agree)
results$n_disagree <- nrow(disagree)

# ═══════════════════════════════════════════════════════════════════════════
# 2. WHO'S RIGHT WHEN THEY DISAGREE?
# ═══════════════════════════════════════════════════════════════════════════

cat("\n=== CONDITIONAL ACCURACY WHEN DISAGREEING ===\n")

if (nrow(disagree) > 0) {
  # Crowd says beat, analysts say miss
  crowd_beat_analysts_miss <- disagree[crowd_beat == TRUE & analyst_beat == FALSE]
  if (nrow(crowd_beat_analysts_miss) > 0) {
    crowd_right_1 <- mean(crowd_beat_analysts_miss$actual_beat, na.rm = TRUE)
    cat("Crowd=beat, Analysts=miss (n=", nrow(crowd_beat_analysts_miss), "):\n")
    cat("  Actual beat:", round(crowd_right_1 * 100, 1), "% → crowd correct\n")
  }

  # Crowd says miss, analysts say beat
  crowd_miss_analysts_beat <- disagree[crowd_beat == FALSE & analyst_beat == TRUE]
  if (nrow(crowd_miss_analysts_beat) > 0) {
    crowd_right_2 <- mean(!crowd_miss_analysts_beat$actual_beat, na.rm = TRUE)
    cat("Crowd=miss, Analysts=beat (n=", nrow(crowd_miss_analysts_beat), "):\n")
    cat("  Actual miss:", round(crowd_right_2 * 100, 1), "% → crowd correct\n")
  }

  # Overall: who's right more often when they disagree?
  disagree[, crowd_right := crowd_correct]
  disagree[, analyst_right := analysts_correct]
  cat("\nOverall when disagreeing:\n")
  cat("  Crowd correct:", round(mean(disagree$crowd_right, na.rm = TRUE) * 100, 1), "%\n")
  cat("  Analysts correct:", round(mean(disagree$analyst_right, na.rm = TRUE) * 100, 1), "%\n")

  results$disagreement_accuracy <- list(
    crowd_correct_rate = mean(disagree$crowd_right, na.rm = TRUE),
    analyst_correct_rate = mean(disagree$analyst_right, na.rm = TRUE),
    n_disagree = nrow(disagree)
  )
}

# ═══════════════════════════════════════════════════════════════════════════
# 3. FLOW IMBALANCE AS SIGNAL IN DISAGREEMENT
# ═══════════════════════════════════════════════════════════════════════════

cat("\n=== FLOW IMBALANCE IN DISAGREEMENT ===\n")

# When flow_imbalance is strongly positive (crowd aggressively buying beat)
# but analysts are cautious, who turns out right?
p[, strong_crowd_beat := flow_imbalance > quantile(flow_imbalance, 0.75, na.rm = TRUE)]
p[, analyst_cautious := !consensus_predicted_beat]

strong_disagree <- p[strong_crowd_beat == TRUE & analyst_cautious == TRUE]
cat("Strong crowd beat + analyst cautious:", nrow(strong_disagree), "events\n")
if (nrow(strong_disagree) >= 5) {
  cat("  Actual beat rate:", round(mean(strong_disagree$actual_beat, na.rm = TRUE) * 100, 1), "%\n")
  results$strong_disagree_beat_rate <- mean(strong_disagree$actual_beat, na.rm = TRUE)
}

# Reverse: strong crowd miss + analysts bullish
p[, strong_crowd_miss := flow_imbalance < quantile(flow_imbalance, 0.25, na.rm = TRUE)]
strong_disagree_rev <- p[strong_crowd_miss == TRUE & consensus_predicted_beat == TRUE]
cat("Strong crowd miss + analyst bullish:", nrow(strong_disagree_rev), "events\n")
if (nrow(strong_disagree_rev) >= 5) {
  cat("  Actual beat rate:", round(mean(strong_disagree_rev$actual_beat, na.rm = TRUE) * 100, 1), "%\n")
}

# ═══════════════════════════════════════════════════════════════════════════
# 4. RETURN PREDICTION
# ═══════════════════════════════════════════════════════════════════════════

cat("\n=== RETURN PREDICTION ===\n")

# Construct analyst dispersion (stdev / abs(mean))
p[, analyst_dispersion := fifelse(
  abs(consensus_mean) > 0.001 & !is.na(consensus_stdev),
  consensus_stdev / abs(consensus_mean),
  NA_real_
)]

# Agreement/disagreement × actual outcome
p[, agree_group := fcase(
  crowd_beat == TRUE & analyst_beat == TRUE, "Both predict beat",
  crowd_beat == FALSE & analyst_beat == FALSE, "Both predict miss",
  crowd_beat == TRUE & analyst_beat == FALSE, "Crowd beat, analysts miss",
  crowd_beat == FALSE & analyst_beat == TRUE, "Crowd miss, analysts beat",
  default = NA_character_
)]

cat("\nExcess return (1d) by agreement group:\n")
ret_by_group <- p[!is.na(agree_group) & !is.na(excess_return_1d), .(
  n = .N,
  mean_xs = mean(excess_return_1d),
  t = t_stat(excess_return_1d)
), by = agree_group]
print(ret_by_group)

results$return_by_group <- ret_by_group

# Regression: excess_return_1d ~ beat_prob + delta_an + flow_imbalance + controls
reg_data <- p[!is.na(excess_return_1d) & !is.na(flow_imbalance) &
              !is.na(delta_an) & !is.na(analyst_dispersion)]

if (nrow(reg_data) >= 20) {
  cat("\nReturn regression (n =", nrow(reg_data), "):\n")

  m1 <- feols(excess_return_1d ~ beat_prob_last + delta_an, data = reg_data)
  m2 <- feols(excess_return_1d ~ beat_prob_last + delta_an + flow_imbalance,
              data = reg_data)
  m3 <- feols(excess_return_1d ~ beat_prob_last + delta_an + flow_imbalance +
              analyst_dispersion + pre_stock_vol, data = reg_data)

  etable(m1, m2, m3,
         headers = c("Base", "+ Flow", "+ Controls"),
         title = "Excess Return (1d) Prediction")

  results$return_regressions <- list(
    m1_coefs = coeftable(m1),
    m2_coefs = coeftable(m2),
    m3_coefs = coeftable(m3)
  )
}

# ═══════════════════════════════════════════════════════════════════════════
# 5. CONTINUOUS SIGNAL — INFORMATION COEFFICIENTS
# ═══════════════════════════════════════════════════════════════════════════

cat("\n=== INFORMATION COEFFICIENTS ===\n")

# Spearman IC: flow_imbalance vs excess returns
ic_flow_1d <- ic_spearman(p$flow_imbalance, p$excess_return_1d)
ic_flow_5d <- ic_spearman(p$flow_imbalance, p$excess_return_5d)
cat("IC(flow_imbalance, excess_return_1d):", round(ic_flow_1d, 4), "\n")
cat("IC(flow_imbalance, excess_return_5d):", round(ic_flow_5d, 4), "\n")

# IC: beat_prob - analyst_implied_prob
p[, analyst_implied_prob := fifelse(
  !is.na(consensus_stdev) & consensus_stdev > 0.001,
  pnorm((consensus_mean - eps_target) / consensus_stdev),
  fifelse(consensus_mean > eps_target, 0.9, 0.1)
)]
p[, prob_spread := beat_prob_last - analyst_implied_prob]

ic_spread_1d <- ic_spearman(p$prob_spread, p$excess_return_1d)
ic_spread_5d <- ic_spearman(p$prob_spread, p$excess_return_5d)
cat("IC(prob_spread, excess_return_1d):", round(ic_spread_1d, 4), "\n")
cat("IC(prob_spread, excess_return_5d):", round(ic_spread_5d, 4), "\n")

results$ic <- list(
  flow_1d = ic_flow_1d, flow_5d = ic_flow_5d,
  spread_1d = ic_spread_1d, spread_5d = ic_spread_5d
)

# ═══════════════════════════════════════════════════════════════════════════
# FIGURE: Disagreement accuracy
# ═══════════════════════════════════════════════════════════════════════════

if (nrow(ret_by_group) > 0) {
  g_disagree <- ggplot(ret_by_group, aes(x = agree_group, y = mean_xs * 100)) +
    geom_col(fill = "steelblue", alpha = 0.7) +
    geom_text(aes(label = sprintf("n=%d\nt=%.1f", n, t)),
              vjust = -0.3, size = 3) +
    labs(x = "", y = "Mean Excess Return (%)",
         title = "Post-Earnings Excess Returns by Agreement Group") +
    theme_minimal(base_size = 12) +
    theme(axis.text.x = element_text(angle = 20, hjust = 1))
  ggsave(file.path(analysis_dir, "fig_disagreement_returns.pdf"),
         g_disagree, width = 8, height = 6)
  cat("\nSaved fig_disagreement_returns.pdf\n")
}

saveRDS(results, file.path(analysis_dir, "disagreement_results.rds"))
cat("Saved disagreement_results.rds\n")
