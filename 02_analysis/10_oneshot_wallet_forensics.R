# 10_oneshot_wallet_forensics.R — Low-Activity Wallet Forensic Analysis
# Input:  import/dome_trades_combined.rds, build/dome_eps_events.rds,
#         build/event_panel.rds, data/oneshot_wallet_profiles.rds (frozen)
# Output: analysis/oneshot_wallet_forensics.rds,
#         analysis/fig_oneshot_wallets.pdf
#
# Examines the opposite tail from 07_wallet_analysis.R: wallets with 1-3
# earnings markets that bet big, got it right, and potentially disappeared.
# This pattern is a classic forensic signal for informed trading.
#
# Wallet profiles pulled from Dome API are frozen in data/ by
# 01_build/05_pull_oneshot_wallets.R — this script reads from frozen data.

library(data.table)
library(ggplot2)

data_dir     <- "~/Documents/git/corrr/390_paper/data"
import_dir   <- "~/Documents/data/corrr/390_paper/import"  # dome_trades_combined.rds (too large to freeze)
analysis_dir <- "~/Documents/data/corrr/390_paper/analysis"

cat("=== ONE-SHOT WALLET FORENSIC ANALYSIS ===\n\n")

# ═══════════════════════════════════════════════════════════════════════════
# 1. DATA CONSTRUCTION (replicates 07 lines 24-62)
# ═══════════════════════════════════════════════════════════════════════════

trades <- readRDS(file.path(import_dir, "dome_trades_combined.rds"))
events <- readRDS(file.path(data_dir, "dome_eps_events.rds"))
panel  <- readRDS(file.path(data_dir, "event_panel.rds"))

# Filter trades to earnings markets
earnings_slugs <- events$market_slug
earn_trades <- trades[market_slug %in% earnings_slugs]
cat("Earnings trades:", format(nrow(earn_trades), big.mark = ","), "\n")
cat("Unique wallets:", format(uniqueN(earn_trades$maker_address), big.mark = ","), "\n")

# Merge outcome
outcomes <- panel[!is.na(actual_beat), .(market_slug, actual_beat, beat_prob_last,
                                          earnings_date)]
earn_trades <- merge(earn_trades, outcomes, by = "market_slug", all.x = FALSE)
cat("Trades with outcome:", format(nrow(earn_trades), big.mark = ","), "\n")

# Classify trade direction and correctness
earn_trades[, bullish_trade := (side == "BUY" & bid_type == "yes") |
                               (side == "SELL" & bid_type == "no")]
earn_trades[, bearish_trade := (side == "SELL" & bid_type == "yes") |
                               (side == "BUY" & bid_type == "no")]
earn_trades[, correct_direction := (bullish_trade & actual_beat == TRUE) |
                                   (bearish_trade & actual_beat == FALSE)]

# ═══════════════════════════════════════════════════════════════════════════
# 2. WALLET-MARKET AGGREGATION (replicates 07 lines 69-81)
# ═══════════════════════════════════════════════════════════════════════════

wallet_market <- earn_trades[, .(
  bullish_vol  = sum(dollar_volume[bullish_trade], na.rm = TRUE),
  bearish_vol  = sum(dollar_volume[bearish_trade], na.rm = TRUE),
  n_trades     = .N,
  actual_beat  = actual_beat[1],
  earnings_date = earnings_date[1],
  last_trade_time = max(block_timestamp)
), by = .(maker_address, market_slug)]

wallet_market[, net_position := bullish_vol - bearish_vol]
wallet_market[, wallet_called_beat := net_position > 0]
wallet_market[, wallet_correct := wallet_called_beat == actual_beat]
wallet_market[, market_volume := bullish_vol + bearish_vol]

# ═══════════════════════════════════════════════════════════════════════════
# 3. AGGREGATE TO WALLET LEVEL
# ═══════════════════════════════════════════════════════════════════════════

wallets <- wallet_market[, .(
  n_markets      = .N,
  n_correct      = sum(wallet_correct),
  hit_rate       = mean(wallet_correct),
  total_volume   = sum(market_volume),
  n_trades_total = sum(n_trades)
), by = maker_address]

sample_beat_rate <- mean(outcomes$actual_beat)

cat("\n=== WALLET UNIVERSE ===\n")
cat("Total wallets:", format(nrow(wallets), big.mark = ","), "\n")
cat(sprintf("Sample beat rate: %.1f%%\n", sample_beat_rate * 100))

# ═══════════════════════════════════════════════════════════════════════════
# 4. DEFINE ONE-SHOT COHORT (n_markets <= 3)
# ═══════════════════════════════════════════════════════════════════════════

oneshot <- wallets[n_markets <= 3]
regular <- wallets[n_markets > 3]

cat("\n=== ONE-SHOT COHORT (1-3 markets) ===\n")
cat(sprintf("One-shot wallets: %s of %s (%.1f%%)\n",
            format(nrow(oneshot), big.mark = ","),
            format(nrow(wallets), big.mark = ","),
            nrow(oneshot) / nrow(wallets) * 100))
cat(sprintf("Regular wallets (4+): %s\n", format(nrow(regular), big.mark = ",")))

# Breakdown by n_markets
cat("\nBreakdown by market count:\n")
for (nm in 1:3) {
  sub <- oneshot[n_markets == nm]
  cat(sprintf("  %d market(s): %s wallets, mean hit rate %.1f%%\n",
              nm, format(nrow(sub), big.mark = ","), mean(sub$hit_rate) * 100))
}

# Volume stats
median_vol_all <- median(wallets$total_volume)
cat(sprintf("\nOverall median wallet volume: $%.0f\n", median_vol_all))
cat(sprintf("One-shot median volume: $%.0f\n", median(oneshot$total_volume)))
cat(sprintf("Regular median volume: $%.0f\n", median(regular$total_volume)))

# ═══════════════════════════════════════════════════════════════════════════
# 5. IDENTIFY PERFECT ONE-SHOT WALLETS
# ═══════════════════════════════════════════════════════════════════════════

perfect <- oneshot[hit_rate == 1.0]
big_bet_threshold <- max(500, median_vol_all)
perfect_big <- perfect[total_volume >= big_bet_threshold]

cat("\n=== PERFECT ONE-SHOT WALLETS (100% correct) ===\n")
cat(sprintf("Perfect wallets: %s of %s one-shot (%.1f%%)\n",
            format(nrow(perfect), big.mark = ","),
            format(nrow(oneshot), big.mark = ","),
            nrow(perfect) / nrow(oneshot) * 100))
cat(sprintf("Big-bet threshold: $%.0f (max of $500 and overall median)\n",
            big_bet_threshold))
cat(sprintf("Perfect + big bet: %s wallets\n",
            format(nrow(perfect_big), big.mark = ",")))

if (nrow(perfect_big) > 0) {
  cat(sprintf("  Mean volume: $%s\n",
              format(round(mean(perfect_big$total_volume)), big.mark = ",")))
  cat(sprintf("  Median volume: $%s\n",
              format(round(median(perfect_big$total_volume)), big.mark = ",")))
  cat(sprintf("  Max volume: $%s\n",
              format(round(max(perfect_big$total_volume)), big.mark = ",")))
  cat(sprintf("  Mean trades: %.1f\n", mean(perfect_big$n_trades_total)))
}

# ═══════════════════════════════════════════════════════════════════════════
# 5b. CORRECTNESS DISTRIBUTION
# ═══════════════════════════════════════════════════════════════════════════

cat("\n=== CORRECTNESS DISTRIBUTION (one-shot cohort) ===\n")
cat(sprintf("  %-12s  %8s  %8s  %10s\n", "Hit Rate", "Count", "Pct", "Median Vol"))
cat(paste(rep("-", 45), collapse = ""), "\n")
for (hr in sort(unique(oneshot$hit_rate))) {
  sub <- oneshot[hit_rate == hr]
  cat(sprintf("  %-12s  %8s  %7.1f%%  $%9s\n",
              sprintf("%.0f%%", hr * 100),
              format(nrow(sub), big.mark = ","),
              nrow(sub) / nrow(oneshot) * 100,
              format(round(median(sub$total_volume)), big.mark = ",")))
}

# ═══════════════════════════════════════════════════════════════════════════
# 5c. BINOMIAL TESTS
# ═══════════════════════════════════════════════════════════════════════════

cat("\n=== BINOMIAL TESTS ===\n")

# For each n_markets bucket, test whether the fraction of 100% wallets
# exceeds what chance would predict given the base beat rate.
for (nm in 1:3) {
  sub <- oneshot[n_markets == nm]
  n_perfect <- sum(sub$hit_rate == 1.0)
  n_total <- nrow(sub)
  # Probability of going nm/nm correct by chance = beat_rate^nm
  # (simplified: each market has ~beat_rate chance of the bet being correct
  # if the wallet just bet "beat" every time)
  p_chance <- sample_beat_rate^nm
  bt <- binom.test(n_perfect, n_total, p = p_chance, alternative = "greater")
  cat(sprintf("\n%d-market wallets: %d / %d perfect (%.1f%%)\n",
              nm, n_perfect, n_total, n_perfect / n_total * 100))
  cat(sprintf("  Chance baseline (always bet beat): %.1f%%\n", p_chance * 100))
  cat(sprintf("  Observed: %.1f%%\n", n_perfect / n_total * 100))
  cat(sprintf("  Binom p-value (one-sided): %s\n", format.pval(bt$p.value, digits = 4)))
}

# Pooled test: across all one-shot wallets, is overall accuracy above baseline?
oneshot_pooled_correct <- sum(oneshot$n_correct)
oneshot_pooled_total <- sum(oneshot$n_markets)
bt_pooled <- binom.test(oneshot_pooled_correct, oneshot_pooled_total,
                        p = sample_beat_rate, alternative = "greater")
cat(sprintf("\nPooled one-shot accuracy: %d / %d = %.1f%%\n",
            oneshot_pooled_correct, oneshot_pooled_total,
            oneshot_pooled_correct / oneshot_pooled_total * 100))
cat(sprintf("  vs baseline %.1f%%, p-value: %s\n",
            sample_beat_rate * 100, format.pval(bt_pooled$p.value, digits = 4)))

# ═══════════════════════════════════════════════════════════════════════════
# 6. TIMING ANALYSIS (perfect + big-bet wallets)
# ═══════════════════════════════════════════════════════════════════════════

cat("\n=== TIMING ANALYSIS ===\n")

if (nrow(perfect_big) > 0) {
  # Get the wallet-market detail rows for perfect big-bet wallets
  flagged_wm <- wallet_market[maker_address %in% perfect_big$maker_address]

  # Parse timestamps
  flagged_wm[, last_trade_ts := as.POSIXct(last_trade_time, origin = "1970-01-01",
                                            tz = "UTC")]
  flagged_wm[, earnings_ts := as.POSIXct(earnings_date, tz = "UTC")]

  # Hours between last trade and earnings
  flagged_wm[, hours_before_earnings := as.numeric(
    difftime(earnings_ts, last_trade_ts, units = "hours")
  )]

  # Flag trades within 24h of earnings
  flagged_wm[, within_24h := hours_before_earnings >= 0 &
                              hours_before_earnings <= 24]

  cat(sprintf("Flagged wallet-market positions: %d\n", nrow(flagged_wm)))
  cat(sprintf("Positions where last trade was within 24h of earnings: %d (%.1f%%)\n",
              sum(flagged_wm$within_24h, na.rm = TRUE),
              mean(flagged_wm$within_24h, na.rm = TRUE) * 100))

  cat("\nHours-before-earnings distribution (flagged wallets):\n")
  valid_hours <- flagged_wm[hours_before_earnings >= 0]$hours_before_earnings
  if (length(valid_hours) > 0) {
    cat(sprintf("  Median: %.1f hours\n", median(valid_hours)))
    cat(sprintf("  Mean:   %.1f hours\n", mean(valid_hours)))
    cat(sprintf("  Min:    %.1f hours\n", min(valid_hours)))
    cat(sprintf("  Max:    %.1f hours\n", max(valid_hours)))
    cat(sprintf("  Within 24h: %d of %d\n",
                sum(valid_hours <= 24), length(valid_hours)))
    cat(sprintf("  Within 48h: %d of %d\n",
                sum(valid_hours <= 48), length(valid_hours)))
  }
} else {
  cat("No perfect + big-bet wallets found; skipping timing analysis.\n")
}

# ═══════════════════════════════════════════════════════════════════════════
# 7. PRIOR ACTIVITY CHECK (from frozen Dome API pull)
# ═══════════════════════════════════════════════════════════════════════════

cat("\n=== PRIOR ACTIVITY CHECK (Dome API data) ===\n")

dome_path <- file.path(data_dir, "oneshot_wallet_profiles.rds")
if (file.exists(dome_path) && nrow(perfect_big) > 0) {
  dome_data <- readRDS(dome_path)
  wallet_activity <- dome_data$wallet_activity

  flagged_addrs <- perfect_big$maker_address
  cat(sprintf("Flagged wallets: %d\n", length(flagged_addrs)))
  cat(sprintf("Dome data loaded (pulled %s)\n", format(dome_data$pull_date, "%Y-%m-%d")))
  cat(sprintf("Wallets with full trade history: %d\n", nrow(wallet_activity)))

  # Summary from orders data
  cat(sprintf("\nAll-market activity (from Dome orders endpoint):\n"))
  cat(sprintf("  Median total markets: %.0f\n",
              median(wallet_activity$n_total_markets)))
  cat(sprintf("  Median total orders: %.0f\n",
              median(wallet_activity$n_total_orders)))
  cat(sprintf("  Median all-market volume: $%s\n",
              format(round(median(wallet_activity$total_volume)), big.mark = ",")))

  # Activity classification
  cat("\nActivity classification (from full Dome trade history):\n")
  type_tab <- wallet_activity[, .N, by = wallet_type]
  for (i in seq_len(nrow(type_tab))) {
    cat(sprintf("  %-25s %d (%.1f%%)\n",
                type_tab$wallet_type[i], type_tab$N[i],
                type_tab$N[i] / nrow(wallet_activity) * 100))
  }

  div <- wallet_activity[wallet_type == "diversified"]
  if (nrow(div) > 0) {
    cat(sprintf("\nAmong diversified wallets:\n"))
    cat(sprintf("  Mean total markets: %.1f\n", mean(div$n_total_markets)))
    cat(sprintf("  Mean non-earnings markets: %.1f\n", mean(div$n_other_markets)))
  }

  fresh <- wallet_activity[wallet_type == "fresh_earnings_only"]
  if (nrow(fresh) > 0) {
    cat(sprintf("\nFresh earnings-only wallets: %d\n", nrow(fresh)))
    cat(sprintf("  These wallets have NO other Polymarket activity.\n"))
  }
} else if (!file.exists(dome_path)) {
  cat("WARNING: Frozen Dome data not found at ", dome_path, "\n")
  cat("Run 01_build/05_pull_oneshot_wallets.R first to pull wallet profiles.\n")
} else {
  cat("No perfect + big-bet wallets found; skipping activity check.\n")
}

# ═══════════════════════════════════════════════════════════════════════════
# 8. FIGURE: SCATTER OF ONE-SHOT WALLETS
# ═══════════════════════════════════════════════════════════════════════════

cat("\n=== GENERATING FIGURE ===\n")

oneshot[, correctness := factor(
  fifelse(hit_rate == 1.0, "100% correct",
          fifelse(hit_rate == 0.0, "0% correct", "Partial")),
  levels = c("100% correct", "Partial", "0% correct")
)]

g <- ggplot(oneshot, aes(x = n_markets, y = total_volume, color = correctness)) +
  geom_jitter(alpha = 0.4, width = 0.2, size = 1.2) +
  scale_y_log10(labels = scales::dollar_format()) +
  scale_color_manual(values = c("100% correct" = "#2ca02c",
                                "Partial" = "#ff7f0e",
                                "0% correct" = "#d62728")) +
  geom_hline(yintercept = big_bet_threshold, linetype = "dashed",
             color = "gray40", linewidth = 0.5) +
  annotate("text", x = 3.4, y = big_bet_threshold,
           label = sprintf("Big-bet threshold ($%s)",
                           format(round(big_bet_threshold), big.mark = ",")),
           hjust = 1, vjust = -0.5, size = 3, color = "gray40") +
  labs(x = "Number of Earnings Markets",
       y = "Total Dollar Volume (log scale)",
       color = "Correctness",
       title = "One-Shot Wallets: Volume vs Market Count",
       subtitle = sprintf("n = %s wallets with 1-3 earnings markets",
                          format(nrow(oneshot), big.mark = ","))) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

ggsave(file.path(analysis_dir, "fig_oneshot_wallets.pdf"), g, width = 8, height = 6)
cat("Saved fig_oneshot_wallets.pdf\n")

# ═══════════════════════════════════════════════════════════════════════════
# 9. SAVE RESULTS
# ═══════════════════════════════════════════════════════════════════════════

results <- list(
  oneshot_wallets = oneshot,
  perfect_wallets = perfect,
  perfect_big_wallets = perfect_big,
  wallet_universe = wallets,
  sample_beat_rate = sample_beat_rate,
  big_bet_threshold = big_bet_threshold,
  binom_test_pooled = bt_pooled,
  timing = if (nrow(perfect_big) > 0) flagged_wm else NULL,
  wallet_activity = if (exists("wallet_activity")) wallet_activity else NULL
)

saveRDS(results, file.path(analysis_dir, "oneshot_wallet_forensics.rds"))
cat("\nSaved oneshot_wallet_forensics.rds\n")
cat("\n=== DONE ===\n")
