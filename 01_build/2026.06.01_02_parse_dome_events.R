# 2026.06.01_02_parse_dome_events.R
# Build the Dome-side event panel for the NEW (2026-03-25) DOME export.
# Faithful mirror of 01_build/01_import.R (hourly VWAP probabilities) +
# 01_build/02_parse_dome_events.R (slug parse + 4-cell flow), pointed at the
# new compact earnings-trade table. Does not touch any original file.
#
# Input : build/2026.06.01_dome_trades_earnings.csv  (from the .py extractor)
#         build/2026.06.01_market_resolution.csv      (per-market resolution)
#         build/dome_eps_events.rds                    (original 340, for tagging)
# Output: build/2026.06.01_dome_eps_events.rds
#
# Share scaling locked at k=1e6: dollar_volume = shares/1e6 * price
# (verified against Dome's own volume_total: median ratio 1.019, r=0.996).

suppressMessages(library(data.table))
build_dir <- "~/Documents/data/corrr/390_paper/build"
K_SHARES  <- 1e6

dt  <- fread(file.path(build_dir, "2026.06.01_dome_trades_earnings.csv"))
res <- fread(file.path(build_dir, "2026.06.01_market_resolution.csv"))
old <- readRDS(file.path(build_dir, "dome_eps_events.rds"))

# ─── A. Scale + implied prob + ET timestamps (mirror 01_import.R) ──────────
dt[, dollar_volume := shares / K_SHARES * price]
dt[, implied_prob  := fifelse(bid_type == "yes", price, 1 - price)]
# Blockchain timestamps are UTC; convert to ET for session/day binning.
dt[, block_timestamp := as.POSIXct(block_timestamp, tz = "UTC")]
dt[, ts_et      := as.POSIXct(format(block_timestamp, tz = "America/New_York"),
                              tz = "America/New_York")]
dt[, trade_date := as.Date(ts_et)]
dt[, trade_hour := as.integer(format(ts_et, "%H"))]
dt[, hour_bin   := fcase(trade_hour < 10, 9L, trade_hour >= 15, 15L, default = trade_hour)]

# ─── B. Hourly VWAP probabilities + LOCF fill (mirror 01_import.R) ─────────
setorder(dt, market_slug, block_timestamp)
hourly <- dt[, .(
  prob_last     = implied_prob[.N],
  prob_vwap     = sum(implied_prob * dollar_volume) / sum(dollar_volume),
  hourly_volume = sum(dollar_volume),
  n_trades      = .N
), by = .(market_slug, trade_date, hour_bin)]

date_range <- hourly[, .(min_d = min(trade_date), max_d = max(trade_date)), by = market_slug]
market_hours <- c(9L,10L,11L,12L,13L,14L,15L)
grid <- date_range[, .(trade_date = seq.Date(min_d, max_d, by = "day")), by = market_slug]
grid <- grid[!weekdays(trade_date) %in% c("Saturday","Sunday")]
grid <- grid[, .(hour_bin = market_hours), by = .(market_slug, trade_date)]
hourly <- merge(grid, hourly, by = c("market_slug","trade_date","hour_bin"), all.x = TRUE)
setorder(hourly, market_slug, trade_date, hour_bin)
hourly[, traded := !is.na(prob_last)]
hourly[, prob_vwap := nafill(prob_vwap, type = "locf"), by = market_slug]

last_prob <- hourly[traded == TRUE,
  .(beat_prob_last = prob_vwap[.N], last_prob_date = trade_date[.N]), by = market_slug]
prob_5d <- hourly[traded == TRUE, {
  target_d <- trade_date[.N] - 5
  idx <- which.min(abs(as.numeric(trade_date - target_d)))
  list(beat_prob_5d_ago = prob_vwap[idx])
}, by = market_slug]

# ─── C. 4-cell flow (mirror 02_parse_dome_events.R) ───────────────────────
flow <- dt[, .(
  buy_yes_vol  = sum(dollar_volume[side == "BUY"  & bid_type == "yes"], na.rm = TRUE),
  sell_yes_vol = sum(dollar_volume[side == "SELL" & bid_type == "yes"], na.rm = TRUE),
  buy_no_vol   = sum(dollar_volume[side == "BUY"  & bid_type == "no"],  na.rm = TRUE),
  sell_no_vol  = sum(dollar_volume[side == "SELL" & bid_type == "no"],  na.rm = TRUE),
  n_trades     = .N,
  first_trade  = min(trade_date),
  beat_conviction = {
    i <- side == "BUY" & bid_type == "yes"
    if (any(i)) sum(price[i]*dollar_volume[i], na.rm=TRUE)/sum(dollar_volume[i], na.rm=TRUE) else NA_real_
  }
), by = market_slug]
flow[, total_pm_volume := buy_yes_vol + sell_yes_vol + buy_no_vol + sell_no_vol]
flow[, net_beat_flow   := (buy_yes_vol + sell_no_vol) - (buy_no_vol + sell_yes_vol)]
flow[, flow_imbalance  := fifelse(total_pm_volume > 0, net_beat_flow/total_pm_volume, 0)]

# ─── D. Slug parse: ticker / eps_type / eps_target / earnings_date ────────
ev <- data.table(market_slug = sort(unique(dt$market_slug)))
ev[, ticker   := toupper(sub("-quarterly-earnings.*","",market_slug))]
ev[, eps_type := fifelse(grepl("quarterly-earnings-gaap-eps", market_slug), "gaap","nongaap")]
ev[, eps_part := sub(".*-eps-[0-9]+-[0-9]+-[0-9]+-","",market_slug)]
ev[grepl("-eps-[0-9]{4}-[0-9]{2}-[0-9]{2}-",market_slug),
   eps_part := sub(".*-eps-[0-9]{4}-[0-9]{2}-[0-9]{2}-","",market_slug)]
parse_eps <- function(s){ v<-sub("^neg","-",s); v<-sub("pt",".",v); as.numeric(v) }
ev[, eps_target := parse_eps(eps_part)]
ext_date <- function(s){
  m <- regmatches(s, regexpr("[0-9]{2}-[0-9]{2}-[0-9]{4}", s))
  if(length(m)==1 && nchar(m)>0) return(as.Date(m,"%m-%d-%Y"))
  m2 <- regmatches(s, regexpr("[0-9]{4}-[0-9]{2}-[0-9]{2}", s))
  if(length(m2)==1 && nchar(m2)>0) return(as.Date(m2,"%Y-%m-%d"))
  as.Date(NA)
}
ev[, earnings_date := as.Date(vapply(market_slug, function(s) as.numeric(ext_date(s)), numeric(1)),
                              origin="1970-01-01")]
ev[, accounting_basis := fifelse(eps_type=="gaap","GAAP","Non-GAAP")]

# ─── E. Merge flow + probs + resolution; tag vs original ──────────────────
ev <- merge(ev, flow, by="market_slug", all.x=TRUE)
ev <- merge(ev, last_prob, by="market_slug", all.x=TRUE)
ev <- merge(ev, prob_5d,   by="market_slug", all.x=TRUE)
ev <- merge(ev, res[, .(market_slug, resolved_beat, volume_total = as.numeric(volume_total))],
            by="market_slug", all.x=TRUE)
# fallback earnings_date to last trade if slug parse failed
ev[is.na(earnings_date), earnings_date := last_prob_date]

CUT <- as.Date("2026-02-18")  # original sample cutoff
ev[, in_original  := market_slug %in% old$market_slug]
ev[, post_cutoff  := earnings_date > CUT]

# ─── F. Liquidity filter (mirror 02_parse_dome_events.R) ──────────────────
n0 <- nrow(ev)
ev <- ev[(total_pm_volume >= 500 | is.na(total_pm_volume)) & (n_trades >= 20 | is.na(n_trades))]
cat("Liquidity filter dropped", n0 - nrow(ev), "markets (vol<$500 or n_trades<20)\n")

# ─── G. Summary + save ────────────────────────────────────────────────────
cat("\n=== NEW DOME EPS EVENTS (2026-03-25 export) ===\n")
cat("Markets:", nrow(ev), " tickers:", uniqueN(ev$ticker), "\n")
cat("in_original (overlap w/ 340):", sum(ev$in_original), "\n")
cat("post_cutoff (>2026-02-18):", sum(ev$post_cutoff, na.rm=TRUE), "\n")
cat("eps_type:\n"); print(ev[, .N, by=eps_type])
cat("resolved beat rate:", round(100*mean(ev$resolved_beat, na.rm=TRUE),1), "%\n")
cat("  full sample:", round(100*mean(ev$resolved_beat,na.rm=TRUE),1),
    "| post_cutoff:", round(100*mean(ev[post_cutoff==TRUE]$resolved_beat,na.rm=TRUE),1), "%\n")
cat("beat_prob_last available:", sum(!is.na(ev$beat_prob_last)),
    " flow available:", sum(!is.na(ev$flow_imbalance)), "\n")
cat("earnings_date range:", as.character(range(ev$earnings_date, na.rm=TRUE)), "\n")

saveRDS(ev, file.path(build_dir, "2026.06.01_dome_eps_events.rds"))
cat("\nSaved 2026.06.01_dome_eps_events.rds\n")
