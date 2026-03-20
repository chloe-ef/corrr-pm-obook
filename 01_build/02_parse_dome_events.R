# 02_parse_dome_events.R — Parse EPS targets from Dome slugs + compute crowd flow
# Input:  build/market_classification.rds, import/dome_trades_combined.rds,
#         build/hourly_market_probabilities.rds
# Output: build/dome_eps_events.rds

library(data.table)

data_root  <- Sys.getenv("DATA_DIR", file.path(getwd(), "data"))
import_dir <- file.path(data_root, "import")
build_dir  <- file.path(data_root, "build")

markets <- readRDS(file.path(build_dir, "market_classification.rds"))
trades  <- readRDS(file.path(import_dir, "dome_trades_combined.rds"))
hourly  <- readRDS(file.path(build_dir, "hourly_market_probabilities.rds"))

# ─── A. Filter to beat/miss EPS markets ────────────────────────────────────
# Slug structure: {ticker}-quarterly-earnings-{gaap|nongaap}-eps-{date}-{value}
beat_miss <- markets[grepl("quarterly-earnings-(gaap|nongaap)-eps", market_slug)]
cat(nrow(beat_miss), "beat/miss EPS markets\n")

# ─── B. Parse eps_type from slug ──────────────────────────────────────────
beat_miss[, eps_type := fifelse(
  grepl("quarterly-earnings-gaap-eps", market_slug), "gaap", "nongaap"
)]
cat("\nEPS type:\n")
print(beat_miss[, .N, by = eps_type])

# ─── C. Parse eps_target from slug ────────────────────────────────────────
# EPS value is the last segment after the date portion.
# Date formats: MM-DD-YYYY or YYYY-MM-DD
# Value format: (neg)?Digits(ptDigits)?
#   Examples: 2pt67 → 2.67, neg5pt24 → -5.24, 0 → 0, 95pt61 → 95.61

# Strip everything up to and including the date to isolate the EPS part
beat_miss[, eps_part := sub(
  ".*-eps-[0-9]+-[0-9]+-[0-9]+-", "", market_slug
)]
# Handle YYYY-MM-DD date format (a few early markets)
beat_miss[grepl("-eps-[0-9]{4}-[0-9]{2}-[0-9]{2}-", market_slug),
          eps_part := sub(".*-eps-[0-9]{4}-[0-9]{2}-[0-9]{2}-", "", market_slug)]

# Convert: neg → -, pt → .
parse_eps_value <- function(s) {
  v <- sub("^neg", "-", s)
  v <- sub("pt", ".", v)
  as.numeric(v)
}

beat_miss[, eps_target := parse_eps_value(eps_part)]

parsed <- sum(!is.na(beat_miss$eps_target))
cat("\nEPS target parsed:", parsed, "of", nrow(beat_miss), "\n")
if (parsed < nrow(beat_miss)) {
  cat("Unparsed:\n")
  print(beat_miss[is.na(eps_target), .(market_slug, eps_part)])
}

# ─── D. Parse earnings_date from slug ─────────────────────────────────────
# Two date formats in slug: MM-DD-YYYY (majority) and YYYY-MM-DD (rare)
extract_earnings_date <- function(slug) {
  # Try MM-DD-YYYY
  m <- regmatches(slug, regexpr("[0-9]{2}-[0-9]{2}-[0-9]{4}", slug))
  if (length(m) == 1 && nchar(m) > 0) {
    return(as.Date(m, format = "%m-%d-%Y"))
  }
  # Try YYYY-MM-DD
  m2 <- regmatches(slug, regexpr("[0-9]{4}-[0-9]{2}-[0-9]{2}", slug))
  if (length(m2) == 1 && nchar(m2) > 0) {
    return(as.Date(m2, format = "%Y-%m-%d"))
  }
  return(as.Date(NA))
}

beat_miss[, earnings_date := vapply(market_slug, function(s)
  as.numeric(extract_earnings_date(s)), numeric(1))]
beat_miss[, earnings_date := as.Date(earnings_date, origin = "1970-01-01")]

# Fallback to last_trade if slug parsing failed
beat_miss[is.na(earnings_date), earnings_date := as.Date(last_trade)]

cat("Earnings date range:", as.character(range(beat_miss$earnings_date, na.rm = TRUE)), "\n")

# ─── E. Compute crowd flow metrics from raw trades ────────────────────────
flow_trades <- trades[market_slug %in% beat_miss$market_slug]
cat("\n", format(nrow(flow_trades), big.mark = ","), "trades in beat/miss markets\n")

# Classify into 4 cells: BUY/SELL × yes/no
flow <- flow_trades[, .(
  buy_yes_vol  = sum(dollar_volume[side == "BUY"  & bid_type == "yes"], na.rm = TRUE),
  sell_yes_vol = sum(dollar_volume[side == "SELL" & bid_type == "yes"], na.rm = TRUE),
  buy_no_vol   = sum(dollar_volume[side == "BUY"  & bid_type == "no"],  na.rm = TRUE),
  sell_no_vol  = sum(dollar_volume[side == "SELL" & bid_type == "no"],  na.rm = TRUE),
  n_trades     = .N,
  beat_conviction = {
    # Volume-weighted avg price on buy-yes trades (crowd confidence in beat)
    by_idx <- side == "BUY" & bid_type == "yes"
    if (any(by_idx)) {
      sum(price[by_idx] * dollar_volume[by_idx], na.rm = TRUE) /
        sum(dollar_volume[by_idx], na.rm = TRUE)
    } else {
      NA_real_
    }
  }
), by = market_slug]

flow[, total_pm_volume := buy_yes_vol + sell_yes_vol + buy_no_vol + sell_no_vol]
flow[, net_beat_flow   := (buy_yes_vol + sell_no_vol) - (buy_no_vol + sell_yes_vol)]
flow[, flow_imbalance  := fifelse(total_pm_volume > 0,
                                  net_beat_flow / total_pm_volume, 0)]

cat("Flow metrics computed for", nrow(flow), "markets\n")
cat("Flow imbalance range:", round(range(flow$flow_imbalance), 3), "\n")

# ─── F. Beat probability from hourly probabilities ────────────────────────
setorder(hourly, market_slug, trade_date, hour_bin)

# Last probability per market (from traded hours only)
# Use prob_vwap (dollar-volume-weighted avg implied probability) as primary metric.
# More robust than last-trade price: weights large-dollar trades more heavily,
# dampening noise from small fills. True bid-ask midpoint unavailable (Dome data
# contains on-chain fills only; Polymarket resting orders live off-chain).
# Academic precedent: Roll (1984), Bliss & Panigirtzoglou (2004).
last_prob <- hourly[traded == TRUE,
                    .(beat_prob_last = prob_vwap[.N],
                      last_prob_date = trade_date[.N]),
                    by = market_slug]

# Probability ~5 trading days before last trade (also VWAP-based)
prob_5d <- hourly[traded == TRUE, {
  last_d <- trade_date[.N]
  target_d <- last_d - 5
  diffs <- abs(as.numeric(trade_date - target_d))
  idx <- which.min(diffs)
  list(beat_prob_5d_ago = prob_vwap[idx])
}, by = market_slug]

# ─── G. Merge everything ─────────────────────────────────────────────────
# Pass first_trade through for timing gap analysis (Rev 1)
# Add accounting_basis for strict GAAP/non-GAAP sigma separation (Patch 2)
events <- beat_miss[, .(ticker, earnings_date, market_slug, eps_type, eps_target,
                        title, n_trades, total_vol, first_trade,
                        accounting_basis = fifelse(eps_type == "gaap", "GAAP", "Non-GAAP"))]

events <- merge(events, flow[, .(market_slug, buy_yes_vol, sell_yes_vol,
                                  buy_no_vol, sell_no_vol, net_beat_flow,
                                  flow_imbalance, beat_conviction,
                                  total_pm_volume,
                                  n_trades_flow = n_trades)],
                by = "market_slug", all.x = TRUE)

events <- merge(events, last_prob, by = "market_slug", all.x = TRUE)
events <- merge(events, prob_5d, by = "market_slug", all.x = TRUE)

# Use flow n_trades if available (more accurate than classification count)
events[!is.na(n_trades_flow), n_trades := n_trades_flow]
events[, n_trades_flow := NULL]

# ─── H. Liquidity filter (Patch 6) ───────────────────────────────────────
# Drop markets with negligible PM activity (stale prices unreliable).
# Academic precedent: Goyal & Saretto (2009), OptionMetrics Ivy DB methodology.
n_before_filter <- nrow(events)
events <- events[total_pm_volume >= 500 | is.na(total_pm_volume)]
events <- events[n_trades >= 20 | is.na(n_trades)]
n_dropped <- n_before_filter - nrow(events)
cat("\nLiquidity filter: dropped", n_dropped, "markets (vol<$500 or n_trades<20)\n")

# ─── I. Summary ──────────────────────────────────────────────────────────
cat("\n=== DOME EPS EVENTS ===\n")
cat("Rows:", nrow(events), "\n")
cat("Tickers:", uniqueN(events$ticker), "\n")
cat("EPS targets parsed:", sum(!is.na(events$eps_target)), "of", nrow(events), "\n")
cat("Beat prob available:", sum(!is.na(events$beat_prob_last)), "\n")
cat("Flow metrics available:", sum(!is.na(events$flow_imbalance)), "\n")
cat("\nBy eps_type:\n")
print(events[, .(.N, median_vol = median(total_pm_volume, na.rm = TRUE)),
             by = eps_type])
cat("\nEarnings date range:", as.character(range(events$earnings_date, na.rm = TRUE)), "\n")
cat("\nEPS target summary:\n")
print(summary(events$eps_target))
cat("\nFlow imbalance summary:\n")
print(summary(events$flow_imbalance))

saveRDS(events, file.path(build_dir, "dome_eps_events.rds"))
cat("\nSaved dome_eps_events.rds\n")
