# 07_wallet_analysis.R — Wallet-Level Concentration & Smart Money Analysis
# Input:  import/dome_trades_combined.rds, build/dome_eps_events.rds,
#         build/event_panel.rds, build/hourly_market_probabilities.rds
# Output: analysis/wallet_results.rds, analysis/fig_wallet_hitrates.pdf
#
# Uses on-chain maker_address from Dome trades to identify "smart money"
# wallets and test whether their flow predicts outcomes beyond price.
# This analysis is unique to blockchain-settled prediction markets and
# cannot be replicated with CLOB API price data (cf. Gomez Cram et al. 2025).

library(data.table)
library(ggplot2)

data_root    <- Sys.getenv("DATA_DIR", file.path(getwd(), "data"))
import_dir   <- file.path(data_root, "import")
build_dir    <- file.path(data_root, "build")
analysis_dir <- file.path(data_root, "analysis")

cat("=== WALLET-LEVEL SMART MONEY ANALYSIS ===\n\n")

# ═══════════════════════════════════════════════════════════════════════════
# 1. DATA CONSTRUCTION
# ═══════════════════════════════════════════════════════════════════════════

trades <- readRDS(file.path(import_dir, "dome_trades_combined.rds"))
events <- readRDS(file.path(build_dir, "dome_eps_events.rds"))
panel  <- readRDS(file.path(build_dir, "event_panel.rds"))

# Filter trades to beat/miss earnings markets
earnings_slugs <- events$market_slug
earn_trades <- trades[market_slug %in% earnings_slugs]
cat("Earnings trades:", format(nrow(earn_trades), big.mark = ","), "\n")
cat("Unique wallets:", format(uniqueN(earn_trades$maker_address), big.mark = ","), "\n")

# Merge outcome (actual_beat) from event panel
outcomes <- panel[!is.na(actual_beat), .(market_slug, actual_beat, beat_prob_last)]
earn_trades <- merge(earn_trades, outcomes, by = "market_slug", all.x = FALSE)
cat("Trades with outcome:", format(nrow(earn_trades), big.mark = ","), "\n")
cat("Markets with outcome:", uniqueN(earn_trades$market_slug), "\n")

# Classify each trade direction
earn_trades[, bullish_trade := (side == "BUY" & bid_type == "yes") |
                               (side == "SELL" & bid_type == "no")]
earn_trades[, bearish_trade := (side == "SELL" & bid_type == "yes") |
                               (side == "BUY" & bid_type == "no")]
earn_trades[, correct_direction := (bullish_trade & actual_beat == TRUE) |
                                   (bearish_trade & actual_beat == FALSE)]

# Classify contra-flow trades: trading AGAINST the current market price
# Contra = buying "no" (or selling "yes") when price > 0.50 (market says beat)
#        = buying "yes" (or selling "no") when price < 0.50 (market says miss)
# We use the implied_prob at time of trade as the "current price"
earn_trades[, implied_prob := fifelse(bid_type == "yes", price, 1 - price)]
earn_trades[, contra_trade := (bearish_trade & beat_prob_last > 0.50) |
                              (bullish_trade & beat_prob_last < 0.50)]

cat("\nTrade direction breakdown:\n")
cat("  Bullish:", format(sum(earn_trades$bullish_trade), big.mark = ","), "\n")
cat("  Bearish:", format(sum(earn_trades$bearish_trade), big.mark = ","), "\n")
cat("  Correct:", format(sum(earn_trades$correct_direction), big.mark = ","),
    sprintf("(%.1f%%)\n", mean(earn_trades$correct_direction) * 100))
cat("  Contra-flow:", format(sum(earn_trades$contra_trade), big.mark = ","),
    sprintf("(%.1f%%)\n", mean(earn_trades$contra_trade) * 100))

# ═══════════════════════════════════════════════════════════════════════════
# 2. WALLET PROFILING
# ═══════════════════════════════════════════════════════════════════════════

# Net dollar position per (wallet, market)
wallet_market <- earn_trades[, .(
  bullish_vol  = sum(dollar_volume[bullish_trade], na.rm = TRUE),
  bearish_vol  = sum(dollar_volume[bearish_trade], na.rm = TRUE),
  contra_vol   = sum(dollar_volume[contra_trade], na.rm = TRUE),
  n_trades     = .N,
  actual_beat  = actual_beat[1]
), by = .(maker_address, market_slug)]

wallet_market[, net_position := bullish_vol - bearish_vol]
wallet_market[, wallet_called_beat := net_position > 0]
wallet_market[, wallet_correct := wallet_called_beat == actual_beat]
wallet_market[, market_volume := bullish_vol + bearish_vol]

# Aggregate per wallet
wallets <- wallet_market[, .(
  n_markets          = .N,
  n_correct          = sum(wallet_correct),
  hit_rate           = mean(wallet_correct),
  total_volume       = sum(market_volume),
  avg_vol_per_market = mean(market_volume),
  total_contra_vol   = sum(contra_vol),
  n_trades_total     = sum(n_trades)
), by = maker_address]
wallets[, contra_share := fifelse(total_volume > 0, total_contra_vol / total_volume, 0)]

cat("\n=== ALL WALLETS ===\n")
cat("Total wallets:", format(nrow(wallets), big.mark = ","), "\n")
cat("Wallets with 15+ markets:", sum(wallets$n_markets >= 15), "\n")

# Sample baseline
sample_beat_rate <- mean(outcomes$actual_beat)
cat(sprintf("Sample beat rate: %.1f%%\n", sample_beat_rate * 100))

# ═══════════════════════════════════════════════════════════════════════════
# 2b. BROAD WALLET DISTRIBUTION (>= 5 markets, for histogram)
# ═══════════════════════════════════════════════════════════════════════════

w_broad <- wallets[n_markets >= 5]
cat(sprintf("\nBroad sample (>= 5 markets): %d wallets\n", nrow(w_broad)))
cat("Hit rate distribution:\n")
print(summary(w_broad$hit_rate))

# ═══════════════════════════════════════════════════════════════════════════
# 3. DEFINE "SMART MONEY" (STRICT FILTERS)
# ═══════════════════════════════════════════════════════════════════════════

cat("\n=== SMART MONEY DEFINITION (STRICT) ===\n")

# Filter 1: Breadth >= 15 distinct markets
w <- wallets[n_markets >= 15]
cat(sprintf("After breadth filter (>= 15 markets): %d wallets\n", nrow(w)))

# Filter 2: Top 25% of total dollar volume (within the 15+ market group)
vol_p75 <- quantile(w$total_volume, 0.75)
cat(sprintf("Top-25%% volume threshold: $%s\n", format(round(vol_p75), big.mark = ",")))

w_qualified <- w[total_volume >= vol_p75]
cat(sprintf("After volume filter (top 25%%): %d wallets\n", nrow(w_qualified)))

cat("\nQualified wallet stats:\n")
cat(sprintf("  Hit rate: mean=%.1f%%, median=%.1f%%, min=%.1f%%, max=%.1f%%\n",
            mean(w_qualified$hit_rate) * 100, median(w_qualified$hit_rate) * 100,
            min(w_qualified$hit_rate) * 100, max(w_qualified$hit_rate) * 100))
cat(sprintf("  Breadth: mean=%.0f, median=%.0f markets\n",
            mean(w_qualified$n_markets), median(w_qualified$n_markets)))
cat(sprintf("  Volume: mean=$%s, median=$%s\n",
            format(round(mean(w_qualified$total_volume)), big.mark = ","),
            format(round(median(w_qualified$total_volume)), big.mark = ",")))

# Filter 3: Top-decile hit rate within the qualified group
hit_p90 <- quantile(w_qualified$hit_rate, 0.90)
cat(sprintf("\nTop-decile hit rate (within qualified): %.1f%%\n", hit_p90 * 100))

smart_wallets <- w_qualified[hit_rate >= hit_p90]
rest_wallets  <- w_qualified[hit_rate < hit_p90]

cat(sprintf("Smart wallets: %d\n", nrow(smart_wallets)))
cat(sprintf("Non-smart qualified wallets: %d\n", nrow(rest_wallets)))

cat("\n--- SMART vs REST (within qualified group) ---\n")
cat(sprintf("  %-20s  %10s  %10s\n", "", "Smart", "Rest"))
cat(paste(rep("-", 45), collapse = ""), "\n")
cat(sprintf("  %-20s  %9.1f%%  %9.1f%%\n", "Mean hit rate",
            mean(smart_wallets$hit_rate) * 100, mean(rest_wallets$hit_rate) * 100))
cat(sprintf("  %-20s  %9.1f%%  %9.1f%%\n", "Median hit rate",
            median(smart_wallets$hit_rate) * 100, median(rest_wallets$hit_rate) * 100))
cat(sprintf("  %-20s  %10.0f  %10.0f\n", "Mean markets",
            mean(smart_wallets$n_markets), mean(rest_wallets$n_markets)))
cat(sprintf("  %-20s  $%9s  $%9s\n", "Mean volume",
            format(round(mean(smart_wallets$total_volume)), big.mark = ","),
            format(round(mean(rest_wallets$total_volume)), big.mark = ",")))
cat(sprintf("  %-20s  %9.1f%%  %9.1f%%\n", "Contra-flow share",
            mean(smart_wallets$contra_share) * 100, mean(rest_wallets$contra_share) * 100))

# Binomial test: is the smart group's hit rate significantly above the baseline?
smart_pooled_correct <- sum(smart_wallets$n_correct)
smart_pooled_total   <- sum(smart_wallets$n_markets)
bt <- binom.test(smart_pooled_correct, smart_pooled_total, p = sample_beat_rate)
cat(sprintf("\nBinomial test (smart pooled accuracy vs %.0f%% baseline):\n",
            sample_beat_rate * 100))
cat(sprintf("  Observed: %d / %d = %.1f%%\n", smart_pooled_correct, smart_pooled_total,
            smart_pooled_correct / smart_pooled_total * 100))
cat(sprintf("  p-value: %s\n", format.pval(bt$p.value, digits = 4)))

# HHI across all qualified wallets
wallet_vol_shares <- w_qualified[, .(maker_address, vol_share = total_volume / sum(total_volume))]
hhi <- sum(wallet_vol_shares$vol_share^2) * 10000
cat(sprintf("\nHHI (qualified wallets): %.0f (10000=monopoly, <1500=competitive)\n", hhi))

# Volume concentration
smart_addrs <- smart_wallets$maker_address
total_smart_vol <- sum(smart_wallets$total_volume)
total_all_vol   <- sum(w_qualified$total_volume)
cat(sprintf("Smart money volume share: $%s of $%s (%.1f%%)\n",
            format(round(total_smart_vol), big.mark = ","),
            format(round(total_all_vol), big.mark = ","),
            total_smart_vol / total_all_vol * 100))

# ═══════════════════════════════════════════════════════════════════════════
# 4. SMART CONTRA-FLOW: TRADING AGAINST THE PRICE
# ═══════════════════════════════════════════════════════════════════════════

cat("\n=== SMART CONTRA-FLOW (CONSENSUS DEFIER TEST) ===\n")

# Per market: compute smart contra-flow imbalance
# This captures when smart wallets trade AGAINST the prevailing price
smart_contra_by_market <- earn_trades[maker_address %in% smart_addrs, .(
  smart_contra_bullish = sum(dollar_volume[contra_trade & bullish_trade], na.rm = TRUE),
  smart_contra_bearish = sum(dollar_volume[contra_trade & bearish_trade], na.rm = TRUE),
  smart_bullish_all    = sum(dollar_volume[bullish_trade], na.rm = TRUE),
  smart_bearish_all    = sum(dollar_volume[bearish_trade], na.rm = TRUE)
), by = market_slug]

smart_contra_by_market[, smart_imbalance := fifelse(
  (smart_bullish_all + smart_bearish_all) > 0,
  (smart_bullish_all - smart_bearish_all) / (smart_bullish_all + smart_bearish_all), 0)]
smart_contra_by_market[, smart_contra_net := smart_contra_bullish - smart_contra_bearish]
smart_contra_by_market[, smart_contra_total := smart_contra_bullish + smart_contra_bearish]
smart_contra_by_market[, smart_contra_imbalance := fifelse(
  smart_contra_total > 0, smart_contra_net / smart_contra_total, 0)]

# Public flow
public_flow <- wallet_market[!(maker_address %in% smart_addrs), .(
  public_bullish = sum(bullish_vol),
  public_bearish = sum(bearish_vol)
), by = market_slug]
public_flow[, public_imbalance := fifelse(
  (public_bullish + public_bearish) > 0,
  (public_bullish - public_bearish) / (public_bullish + public_bearish), 0)]

# Merge for regression
reg_data <- merge(panel[!is.na(actual_beat),
                        .(market_slug, actual_beat, beat_prob_last, flow_imbalance)],
                  smart_contra_by_market[, .(market_slug, smart_imbalance,
                                            smart_contra_imbalance)],
                  by = "market_slug", all.x = TRUE)
reg_data <- merge(reg_data,
                  public_flow[, .(market_slug, public_imbalance)],
                  by = "market_slug", all.x = TRUE)

reg_data[is.na(smart_imbalance), smart_imbalance := 0]
reg_data[is.na(smart_contra_imbalance), smart_contra_imbalance := 0]
reg_data[is.na(public_imbalance), public_imbalance := 0]

cat("Regression sample:", nrow(reg_data), "events\n")
cat("Events with smart money activity:", sum(reg_data$smart_imbalance != 0), "\n")
cat("Events with smart contra-flow:", sum(reg_data$smart_contra_imbalance != 0), "\n")

# ═══════════════════════════════════════════════════════════════════════════
# 5. LOGISTIC REGRESSIONS
# ═══════════════════════════════════════════════════════════════════════════

cat("\n=== LOGISTIC REGRESSIONS ===\n")

cat("\n--- Model 1: Price only ---\n")
m1 <- glm(actual_beat ~ beat_prob_last, data = reg_data, family = "binomial")
cat(sprintf("  AIC: %.1f\n", AIC(m1)))
print(summary(m1)$coefficients)

cat("\n--- Model 2: Price + Full Flow (all wallets) ---\n")
m2 <- glm(actual_beat ~ beat_prob_last + flow_imbalance, data = reg_data, family = "binomial")
cat(sprintf("  AIC: %.1f (vs M1: %+.1f)\n", AIC(m2), AIC(m2) - AIC(m1)))
print(summary(m2)$coefficients)

cat("\n--- Model 3: Price + Smart Flow ---\n")
m3 <- glm(actual_beat ~ beat_prob_last + smart_imbalance, data = reg_data, family = "binomial")
cat(sprintf("  AIC: %.1f (vs M1: %+.1f)\n", AIC(m3), AIC(m3) - AIC(m1)))
print(summary(m3)$coefficients)

cat("\n--- Model 4: Price + Smart Flow + Public Flow ---\n")
m4 <- glm(actual_beat ~ beat_prob_last + smart_imbalance + public_imbalance,
           data = reg_data, family = "binomial")
cat(sprintf("  AIC: %.1f (vs M1: %+.1f)\n", AIC(m4), AIC(m4) - AIC(m1)))
print(summary(m4)$coefficients)

cat("\n--- Model 5: Price + Smart Contra-Flow ---\n")
m5 <- glm(actual_beat ~ beat_prob_last + smart_contra_imbalance,
           data = reg_data, family = "binomial")
cat(sprintf("  AIC: %.1f (vs M1: %+.1f)\n", AIC(m5), AIC(m5) - AIC(m1)))
print(summary(m5)$coefficients)

# ═══════════════════════════════════════════════════════════════════════════
# 6. DIRECTIONAL ACCURACY COMPARISON
# ═══════════════════════════════════════════════════════════════════════════

cat("\n=== DIRECTIONAL ACCURACY ===\n")

# Smart money per-market calls
smart_calls <- wallet_market[maker_address %in% smart_addrs,
                             .(smart_net = sum(bullish_vol) - sum(bearish_vol),
                               actual_beat = actual_beat[1]),
                             by = market_slug]
smart_calls[, smart_called_beat := smart_net > 0]
smart_calls[, smart_correct := smart_called_beat == actual_beat]

# Public per-market calls
public_calls <- wallet_market[!(maker_address %in% smart_addrs),
                              .(public_net = sum(bullish_vol) - sum(bearish_vol),
                                actual_beat = actual_beat[1]),
                              by = market_slug]
public_calls[, public_called_beat := public_net > 0]
public_calls[, public_correct := public_called_beat == actual_beat]

# Price-based calls
price_calls <- reg_data[, .(market_slug, actual_beat,
                            price_called_beat = beat_prob_last > 0.5)]
price_calls[, price_correct := price_called_beat == actual_beat]

cat(sprintf("  Smart money accuracy:  %.1f%% (%d / %d markets)\n",
            mean(smart_calls$smart_correct) * 100,
            sum(smart_calls$smart_correct), nrow(smart_calls)))
cat(sprintf("  General public accuracy: %.1f%% (%d / %d markets)\n",
            mean(public_calls$public_correct) * 100,
            sum(public_calls$public_correct), nrow(public_calls)))
cat(sprintf("  Price-based accuracy:  %.1f%% (%d / %d markets)\n",
            mean(price_calls$price_correct) * 100,
            sum(price_calls$price_correct), nrow(price_calls)))

# Disagreement: when smart money disagrees with the price
disagree <- merge(smart_calls[, .(market_slug, smart_called_beat, smart_correct)],
                  price_calls, by = "market_slug")
disagree[, smart_disagrees := smart_called_beat != price_called_beat]

n_disagree <- sum(disagree$smart_disagrees)
cat(sprintf("\nSmart money disagrees with price: %d of %d events (%.1f%%)\n",
            n_disagree, nrow(disagree), mean(disagree$smart_disagrees) * 100))

if (n_disagree >= 3) {
  de <- disagree[smart_disagrees == TRUE]
  cat(sprintf("  When they disagree:\n"))
  cat(sprintf("    Smart money correct: %.1f%% (%d / %d)\n",
              mean(de$smart_correct) * 100, sum(de$smart_correct), nrow(de)))
  cat(sprintf("    Price correct:       %.1f%% (%d / %d)\n",
              mean(de$price_correct) * 100, sum(de$price_correct), nrow(de)))
}

# ═══════════════════════════════════════════════════════════════════════════
# 7. FIGURES
# ═══════════════════════════════════════════════════════════════════════════

cat("\n=== GENERATING FIGURES ===\n")

# Use the broad sample (>= 5 markets) for the histogram
g1 <- ggplot(w_broad, aes(x = hit_rate * 100)) +
  geom_histogram(bins = 25, fill = "steelblue", alpha = 0.7, color = "white") +
  geom_vline(xintercept = sample_beat_rate * 100, linetype = "dashed",
             color = "red", linewidth = 1) +
  geom_vline(xintercept = 50, linetype = "dotted", color = "gray50") +
  annotate("text", x = sample_beat_rate * 100 + 2, y = Inf,
           label = sprintf("Naive baseline\n(always bet beat)\n%.0f%%",
                           sample_beat_rate * 100),
           hjust = 0, vjust = 1.5, size = 3, color = "red") +
  annotate("text", x = 50 + 1, y = Inf,
           label = "Random\n50%", hjust = 0, vjust = 1.5, size = 3, color = "gray50") +
  labs(x = "Per-Wallet Hit Rate (%)",
       y = "Number of Wallets",
       title = "Distribution of Wallet-Level Directional Accuracy",
       subtitle = sprintf("n = %d wallets (>= 5 markets each), sample beat rate = %.0f%%",
                          nrow(w_broad), sample_beat_rate * 100)) +
  theme_minimal(base_size = 12)

ggsave(file.path(analysis_dir, "fig_wallet_hitrates.pdf"), g1, width = 8, height = 6)
cat("Saved fig_wallet_hitrates.pdf\n")

# ═══════════════════════════════════════════════════════════════════════════
# SAVE RESULTS
# ═══════════════════════════════════════════════════════════════════════════

# Total earnings volume (all trades, not just qualified wallets)
total_earnings_vol <- sum(earn_trades$dollar_volume, na.rm = TRUE)
smart_total_vol_share <- total_smart_vol / total_earnings_vol
cat(sprintf("Smart share of ALL earnings volume: $%s of $%s (%.1f%%)\n",
            format(round(total_smart_vol), big.mark = ","),
            format(round(total_earnings_vol), big.mark = ","),
            smart_total_vol_share * 100))

results <- list(
  wallet_stats_broad = w_broad,
  wallet_stats_strict = w_qualified,
  smart_wallets = smart_wallets,
  smart_calls = smart_calls,
  public_calls = public_calls,
  regression_models = list(m1 = m1, m2 = m2, m3 = m3, m4 = m4, m5 = m5),
  hhi = hhi,
  smart_volume_share_qualified = total_smart_vol / total_all_vol,
  smart_volume_share_total = smart_total_vol_share,
  smart_pooled_hit_rate = smart_pooled_correct / smart_pooled_total,
  smart_contra_share = mean(smart_wallets$contra_share),
  rest_contra_share = mean(rest_wallets$contra_share),
  sample_beat_rate = sample_beat_rate,
  binom_test = bt
)

saveRDS(results, file.path(analysis_dir, "wallet_results.rds"))
cat("\nSaved wallet_results.rds\n")
