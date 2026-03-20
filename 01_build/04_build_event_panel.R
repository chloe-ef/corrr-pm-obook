# 04_build_event_panel.R — Merge Dome EPS events + IBES + Yahoo returns into panel
# Input:  build/dome_eps_events.rds, build/ibes_data.rds,
#         build/equity_daily_returns.rds, build/index_daily_returns.rds
# Output: build/event_panel.rds
#
# Rev 1:   Compute timing gap variables (consensus staleness vs PM activity)
# Rev 3:   Add day 10 holding period returns
# Patch 3: BMO/AMC look-ahead bias correction for last_prob_date

library(data.table)

build_dir <- "~/Documents/data/corrr/390_paper/build"

events <- readRDS(file.path(build_dir, "dome_eps_events.rds"))
ibes   <- readRDS(file.path(build_dir, "ibes_data.rds"))
taq    <- readRDS(file.path(build_dir, "equity_daily_returns.rds"))
idx    <- readRDS(file.path(build_dir, "index_daily_returns.rds"))

cat("Events:", nrow(events), "\n")
cat("IBES matches:", nrow(ibes), "\n")

# ─── 1. Merge Dome events ← IBES ─────────────────────────────────────────
panel <- merge(events, ibes, by = "market_slug", all.x = TRUE)

matched <- sum(!is.na(panel$actual_eps))
cat("\nIBES matched:", matched, "of", nrow(panel), "events\n")
cat("Match rate:", round(matched / nrow(panel) * 100, 1), "%\n")

# ─── 2. Core forecast error variables ────────────────────────────────────
panel[, delta_pm     := eps_target - actual_eps]
panel[, delta_an     := consensus_mean - actual_eps]
panel[, abs_delta_pm := abs(delta_pm)]
panel[, abs_delta_an := abs(delta_an)]
panel[, pm_closer    := abs_delta_pm < abs_delta_an]

# ─── 3. Beat/miss outcome variables ──────────────────────────────────────
panel[, actual_beat             := actual_eps > eps_target]
panel[, consensus_predicted_beat := consensus_mean > eps_target]
panel[, crowd_predicted_beat    := beat_prob_last > 0.5]
panel[, crowd_correct           := actual_beat == crowd_predicted_beat]
panel[, analysts_correct        := actual_beat == consensus_predicted_beat]

# ─── 4. MM line vs consensus ─────────────────────────────────────────────
panel[, target_vs_consensus    := eps_target - consensus_mean]
panel[, target_consensus_gap_pct := fifelse(
  abs(consensus_mean) > 0.001,
  (eps_target - consensus_mean) / abs(consensus_mean),
  NA_real_
)]
panel[, line_above_consensus := eps_target > consensus_mean]

# ─── 4b. BMO/AMC Look-Ahead Bias Correction (Patch 3) ────────────────────
# If earnings are released Before Market Open (BMO), the last valid PM
# probability is from the prior day's close, not the earnings date.
# Use announcement_date from IBES to infer timing. If the IBES anndats
# matches earnings_date and the last_prob_date equals earnings_date,
# we conservatively assume AMC (market closes before after-hours release).
# If anndats is the day before earnings_date, it's BMO and last_prob_date
# should be earnings_date - 1.
# Fallback: if last_prob_date > earnings_date, it's post-announcement — exclude.

panel[, last_prob_date_adj := last_prob_date]

# If last prob was captured after earnings date, flag as potentially contaminated
panel[!is.na(last_prob_date) & !is.na(earnings_date) &
      last_prob_date > earnings_date,
      last_prob_date_adj := earnings_date]

# If announcement was before earnings_date (BMO pattern), shift back 1 day
panel[!is.na(announcement_date) & !is.na(earnings_date) &
      announcement_date < earnings_date,
      last_prob_date_adj := pmin(last_prob_date_adj, earnings_date - 1L, na.rm = TRUE)]

n_adj <- sum(panel$last_prob_date_adj != panel$last_prob_date, na.rm = TRUE)
cat("BMO/AMC adjustment: corrected", n_adj, "events' last_prob_date\n")

# ─── 4c. Timing Gap Variables (Rev 1) ────────────────────────────────────
# Structural timing advantage: IBES Summary updates monthly (Thursday before
# 3rd Friday = statpers). Analysts face practical quiet period 1-3 weeks
# before earnings (Reg FD). The PM trades continuously.
# Defensive: only compute if the required columns exist (consensus_date
# requires re-running 03_pull_ibes.R with the revised WRDS query).
if ("consensus_date" %in% names(panel)) {
  panel[, days_consensus_to_earnings := as.numeric(earnings_date - consensus_date)]
  panel[, days_consensus_to_pm_close := as.numeric(last_prob_date_adj - consensus_date)]
} else {
  cat("  NOTE: consensus_date not in IBES data — re-run 03_pull_ibes.R with WRDS\n")
  panel[, days_consensus_to_earnings := NA_real_]
  panel[, days_consensus_to_pm_close := NA_real_]
}
if ("first_trade" %in% names(panel)) {
  panel[, days_pm_open_to_earnings := as.numeric(earnings_date - first_trade)]
} else {
  panel[, days_pm_open_to_earnings := NA_real_]
}

cat("\nTiming gaps (Rev 1):\n")
cat("  days_consensus_to_earnings — populated:",
    sum(!is.na(panel$days_consensus_to_earnings)), "\n")
cat("  days_pm_open_to_earnings — populated:",
    sum(!is.na(panel$days_pm_open_to_earnings)), "\n")
cat("  days_consensus_to_pm_close — populated:",
    sum(!is.na(panel$days_consensus_to_pm_close)), "\n")

# ─── 5. Merge TAQ returns for post-earnings excess returns ───────────────

# Ensure date columns are Date type
taq[, date := as.Date(date)]
idx[, date := as.Date(date)]

# Build trading day calendar
trading_days <- sort(unique(taq$date))

# Helper: shift N business days forward
shift_bdays <- function(dates, n, cal = trading_days) {
  vapply(dates, function(d) {
    if (is.na(d)) return(NA_real_)
    pos <- findInterval(d, cal)
    target <- pos + n
    if (target < 1 || target > length(cal)) return(NA_real_)
    as.numeric(cal[target])
  }, numeric(1))
}

# For each event, get 1-day, 5-day, and 10-day post-earnings returns
panel[, earnings_date_num := as.numeric(earnings_date)]

# Find the first trading day on or after earnings_date
panel[, t0 := vapply(earnings_date, function(d) {
  pos <- findInterval(d, trading_days)
  target <- pos + 1L  # next trading day
  if (target > length(trading_days)) return(NA_real_)
  as.numeric(trading_days[target])
}, numeric(1))]
panel[, t0 := as.Date(t0, origin = "1970-01-01")]

# Get t+1, t+5, and t+10 trading days (Rev 3: added t+10)
panel[, t1  := as.Date(shift_bdays(t0, 1),  origin = "1970-01-01")]
panel[, t5  := as.Date(shift_bdays(t0, 4),  origin = "1970-01-01")]
panel[, t10 := as.Date(shift_bdays(t0, 9),  origin = "1970-01-01")]

# Identify the stock return column
ret_col <- intersect(c("stock_return", "ret", "daily_return", "return"), names(taq))
if (length(ret_col) == 0) {
  taq_cols <- names(taq)
  cat("TAQ columns:", paste(taq_cols, collapse = ", "), "\n")
  if ("close" %in% taq_cols) {
    setorder(taq, ticker, date)
    taq[, ret := close / shift(close, 1L) - 1, by = ticker]
    ret_col <- "ret"
  } else {
    stop("Cannot find return column in taq_daily_returns.rds")
  }
} else {
  ret_col <- ret_col[1]
}

# Similarly for index
idx_ret_col <- intersect(c("sp500_ret", "ret", "daily_return", "return", "spy_ret"), names(idx))
if (length(idx_ret_col) == 0) {
  idx_cols <- names(idx)
  cat("Index columns:", paste(idx_cols, collapse = ", "), "\n")
  if ("close" %in% idx_cols || "Close" %in% idx_cols) {
    close_col <- intersect(c("close", "Close"), names(idx))[1]
    setorder(idx, date)
    idx[, ret := get(close_col) / shift(get(close_col), 1L) - 1]
    idx_ret_col <- "ret"
  } else {
    stop("Cannot find return column in taq_index_returns.rds")
  }
} else {
  idx_ret_col <- idx_ret_col[1]
}

# 1-day stock return on t0
taq_t0 <- taq[, .(ticker, date, stock_ret_t0 = get(ret_col))]
panel[taq_t0, on = .(ticker, t0 = date), stock_ret_1d := i.stock_ret_t0]

# Index return on t0
idx_t0 <- idx[, .(date, idx_ret_t0 = get(idx_ret_col))]
panel[idx_t0, on = .(t0 = date), idx_ret_1d := i.idx_ret_t0]

# 5-day cumulative returns: product of (1+r) from t0 to t5
panel[, excess_return_1d := stock_ret_1d - idx_ret_1d]

# Compute 5-day and 10-day cumulative returns
panel[, cum_stock_5d  := NA_real_]
panel[, cum_idx_5d    := NA_real_]
panel[, cum_stock_10d := NA_real_]
panel[, cum_idx_10d   := NA_real_]

for (i in seq_len(nrow(panel))) {
  tk <- panel$ticker[i]
  d0 <- panel$t0[i]
  d5 <- panel$t5[i]
  d10 <- panel$t10[i]
  if (is.na(d0) || is.na(tk)) next

  # 5-day
  if (!is.na(d5)) {
    stock_rets <- taq[ticker == tk & date >= d0 & date <= d5, get(ret_col)]
    if (length(stock_rets) > 0) {
      set(panel, i, "cum_stock_5d", prod(1 + stock_rets, na.rm = TRUE) - 1)
    }
    idx_rets <- idx[date >= d0 & date <= d5, get(idx_ret_col)]
    if (length(idx_rets) > 0) {
      set(panel, i, "cum_idx_5d", prod(1 + idx_rets, na.rm = TRUE) - 1)
    }
  }

  # 10-day (Rev 3)
  if (!is.na(d10)) {
    stock_rets_10 <- taq[ticker == tk & date >= d0 & date <= d10, get(ret_col)]
    if (length(stock_rets_10) > 0) {
      set(panel, i, "cum_stock_10d", prod(1 + stock_rets_10, na.rm = TRUE) - 1)
    }
    idx_rets_10 <- idx[date >= d0 & date <= d10, get(idx_ret_col)]
    if (length(idx_rets_10) > 0) {
      set(panel, i, "cum_idx_10d", prod(1 + idx_rets_10, na.rm = TRUE) - 1)
    }
  }
}

panel[, excess_return_5d  := cum_stock_5d  - cum_idx_5d]
panel[, excess_return_10d := cum_stock_10d - cum_idx_10d]

# ─── 6. Pre-earnings stock volatility (5-10 day window before earnings) ──
panel[, pre_stock_vol := NA_real_]
for (i in seq_len(nrow(panel))) {
  tk <- panel$ticker[i]
  ed <- panel$earnings_date[i]
  if (is.na(tk) || is.na(ed)) next

  pre_rets <- taq[ticker == tk & date >= (ed - 15) & date < ed, get(ret_col)]
  if (length(pre_rets) >= 5) {
    set(panel, i, "pre_stock_vol", sd(pre_rets, na.rm = TRUE))
  }
}

# ─── 7. Winsorize returns at 2/98 percentile ─────────────────────────────
winsorize <- function(x, lo = 0.02, hi = 0.98) {
  q <- quantile(x, probs = c(lo, hi), na.rm = TRUE)
  pmin(pmax(x, q[1]), q[2])
}

for (rc in c("excess_return_1d", "excess_return_5d", "excess_return_10d")) {
  valid <- !is.na(panel[[rc]])
  if (sum(valid) > 10) {
    panel[valid, (rc) := winsorize(get(rc))]
  }
}

# ─── 8. Clean up temp columns ────────────────────────────────────────────
drop_cols <- c("earnings_date_num", "t0", "t1", "t5", "t10",
               "stock_ret_1d", "idx_ret_1d",
               "cum_stock_5d", "cum_idx_5d", "cum_stock_10d", "cum_idx_10d")
drop_cols <- intersect(drop_cols, names(panel))
panel[, (drop_cols) := NULL]

# ─── 9. Summary ──────────────────────────────────────────────────────────
cat("\n=== EVENT PANEL ===\n")
cat("Rows:", nrow(panel), "\n")
cat("With actual_eps:", sum(!is.na(panel$actual_eps)), "\n")
cat("With delta_pm:", sum(!is.na(panel$delta_pm)), "\n")
cat("With excess_return_1d:", sum(!is.na(panel$excess_return_1d)), "\n")
cat("With excess_return_5d:", sum(!is.na(panel$excess_return_5d)), "\n")
cat("With excess_return_10d:", sum(!is.na(panel$excess_return_10d)), "\n")

cat("\nDelta PM (MM error) summary:\n")
print(summary(panel$delta_pm))
cat("\nDelta AN (analyst error) summary:\n")
print(summary(panel$delta_an))

cat("\nPM closer than analysts:", sum(panel$pm_closer, na.rm = TRUE), "of",
    sum(!is.na(panel$pm_closer)), "\n")

cat("\nCrowd correct:", sum(panel$crowd_correct, na.rm = TRUE), "of",
    sum(!is.na(panel$crowd_correct)),
    "(", round(mean(panel$crowd_correct, na.rm = TRUE) * 100, 1), "%)\n")
cat("Analysts correct:", sum(panel$analysts_correct, na.rm = TRUE), "of",
    sum(!is.na(panel$analysts_correct)),
    "(", round(mean(panel$analysts_correct, na.rm = TRUE) * 100, 1), "%)\n")

# Timing gap summary (Rev 1)
cat("\n=== TIMING GAPS ===\n")
for (v in c("days_consensus_to_earnings", "days_pm_open_to_earnings",
            "days_consensus_to_pm_close")) {
  vals <- panel[[v]]
  vals <- vals[!is.na(vals)]
  if (length(vals) > 0) {
    cat(sprintf("  %s: mean=%.1f, median=%.1f, range=[%d, %d]\n",
                v, mean(vals), median(vals), min(vals), max(vals)))
  }
}

saveRDS(panel, file.path(build_dir, "event_panel.rds"))
cat("\nSaved event_panel.rds\n")
