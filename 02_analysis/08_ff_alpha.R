# 08_ff_alpha.R — Fama-French 3-Factor Alpha for Short-Only Strategy
# Input:  build/event_panel.rds, build/equity_daily_returns.rds,
#         build/ff_daily.rds
# Output: analysis/ff_alpha_results.rds

library(data.table)

data_dir     <- "~/Documents/git/corrr/390_paper/data"
analysis_dir <- "~/Documents/data/corrr/390_paper/analysis"

cat("=== FAMA-FRENCH 3-FACTOR ALPHA: SHORT STRATEGY ===\n\n")

panel <- readRDS(file.path(data_dir, "event_panel.rds"))
taq   <- readRDS(file.path(data_dir, "equity_daily_returns.rds"))
ff    <- readRDS(file.path(data_dir, "ff_daily.rds"))

# Identify return column in TAQ
ret_col <- intersect(c("stock_return", "ret", "daily_return", "return"), names(taq))
if (length(ret_col) == 0) {
  setorder(taq, ticker, date)
  taq[, ret := Close / shift(Close) - 1, by = ticker]
  ret_col <- "ret"
} else {
  ret_col <- ret_col[1]
}

# Trading day calendar
taq[, date := as.Date(date)]
ff[, date := as.Date(date)]
trading_days <- sort(unique(taq$date))

# Check FF coverage
ff_range <- range(ff$date)
taq_range <- range(taq$date)
cat(sprintf("FF date range: %s to %s\n", ff_range[1], ff_range[2]))
cat(sprintf("TAQ date range: %s to %s\n", taq_range[1], taq_range[2]))
cat(sprintf("Overlap days: %d\n", sum(trading_days %in% ff$date)))

# Helper: find t0 (first trading day on or after earnings_date)
find_t0 <- function(ed) {
  pos <- findInterval(ed, trading_days)
  target <- pos + 1L
  if (target > length(trading_days)) return(as.Date(NA))
  trading_days[target]
}

results <- list()

# ═══════════════════════════════════════════════════════════════════════════
# For each threshold, build a panel of daily short returns and regress on FF3
# ═══════════════════════════════════════════════════════════════════════════

thresholds <- c(0.20, 0.25, 0.30, 0.35)
horizons   <- c(1, 5, 10)

for (thresh in thresholds) {
  cat(sprintf("\n--- Threshold: P(beat) < %.2f ---\n", thresh))

  short_events <- panel[!is.na(beat_prob_last) & beat_prob_last < thresh &
                        !is.na(actual_eps) & !is.na(consensus_mean) &
                        !is.na(excess_return_1d) & !is.na(flow_imbalance) &
                        !is.na(num_analysts)]
  cat(sprintf("  Short events: %d\n", nrow(short_events)))

  if (nrow(short_events) < 5) {
    cat("  Skipping (too few events)\n")
    next
  }

  for (h in horizons) {
    # Collect daily returns for each event from t0 through t0+(h-1)
    daily_rows <- list()

    for (j in seq_len(nrow(short_events))) {
      ev <- short_events[j]
      t0 <- find_t0(ev$earnings_date)
      if (is.na(t0)) next

      # Get h trading days starting from t0
      t0_pos <- which(trading_days == t0)
      if (length(t0_pos) == 0) next
      end_pos <- min(t0_pos + h - 1, length(trading_days))
      event_days <- trading_days[t0_pos:end_pos]

      # Stock returns
      stock_rets <- taq[ticker == ev$ticker & date %in% event_days,
                        .(date, stock_ret = get(ret_col))]
      if (nrow(stock_rets) == 0) next

      # Merge with FF
      merged <- merge(stock_rets, ff, by = "date", all.x = TRUE)
      merged <- merged[!is.na(mkt_rf) & !is.na(stock_ret)]
      if (nrow(merged) == 0) next

      # Short return excess of risk-free: -(stock_ret) - rf
      # Actually: short profit = -stock_ret, excess = short_profit - rf
      merged[, short_excess := -(stock_ret) - rf]
      merged[, event_id := ev$market_slug]

      daily_rows[[length(daily_rows) + 1]] <- merged
    }

    if (length(daily_rows) == 0) {
      cat(sprintf("  Day %2d: No data\n", h))
      next
    }

    daily_panel <- rbindlist(daily_rows)
    n_events <- uniqueN(daily_panel$event_id)
    n_days <- nrow(daily_panel)

    # Time-series regression: pool all event-days
    ff_reg <- lm(short_excess ~ mkt_rf + smb + hml, data = daily_panel)
    s <- summary(ff_reg)
    ct <- s$coefficients

    alpha     <- ct["(Intercept)", "Estimate"]
    alpha_t   <- ct["(Intercept)", "t value"]
    alpha_p   <- ct["(Intercept)", "Pr(>|t|)"]
    mkt_beta  <- ct["mkt_rf", "Estimate"]
    smb_beta  <- ct["smb", "Estimate"]
    hml_beta  <- ct["hml", "Estimate"]
    r2        <- s$r.squared

    # Annualize alpha (daily to annual)
    alpha_ann <- alpha * 252

    cat(sprintf("  Day %2d: alpha=%.4f (%.2f%% ann), t=%.2f, p=%s, Mkt=%.2f, SMB=%.2f, HML=%.2f, R2=%.3f (n=%d events, %d obs)\n",
                h, alpha, alpha_ann * 100, alpha_t, format.pval(alpha_p, digits = 3),
                mkt_beta, smb_beta, hml_beta, r2, n_events, n_days))

    results[[paste0("thresh_", thresh, "_day_", h)]] <- list(
      threshold = thresh,
      horizon = h,
      n_events = n_events,
      n_obs = n_days,
      alpha = alpha,
      alpha_ann = alpha_ann,
      alpha_t = alpha_t,
      alpha_p = alpha_p,
      mkt_beta = mkt_beta,
      smb_beta = smb_beta,
      hml_beta = hml_beta,
      r_squared = r2,
      coef_table = ct
    )
  }
}

# ═══════════════════════════════════════════════════════════════════════════
# CALENDAR-TIME PORTFOLIO APPROACH
# For each trading day, average across all active short positions,
# then regress the daily portfolio return on FF3.
# ═══════════════════════════════════════════════════════════════════════════

cat("\n=== CALENDAR-TIME PORTFOLIO REGRESSIONS ===\n")

cal_results <- list()

for (thresh in thresholds) {
  short_events <- panel[!is.na(beat_prob_last) & beat_prob_last < thresh &
                        !is.na(actual_eps) & !is.na(consensus_mean) &
                        !is.na(excess_return_1d) & !is.na(flow_imbalance) &
                        !is.na(num_analysts)]
  if (nrow(short_events) < 5) next

  for (h in horizons) {
    # Build set of (event_id, date) pairs for active short positions
    active_rows <- list()

    for (j in seq_len(nrow(short_events))) {
      ev <- short_events[j]
      t0 <- find_t0(ev$earnings_date)
      if (is.na(t0)) next

      t0_pos <- which(trading_days == t0)
      if (length(t0_pos) == 0) next
      end_pos <- min(t0_pos + h - 1, length(trading_days))
      event_days <- trading_days[t0_pos:end_pos]

      stock_rets <- taq[ticker == ev$ticker & date %in% event_days,
                        .(date, stock_ret = get(ret_col))]
      if (nrow(stock_rets) == 0) next

      stock_rets[, event_id := ev$market_slug]
      active_rows[[length(active_rows) + 1]] <- stock_rets
    }

    if (length(active_rows) == 0) next

    active_panel <- rbindlist(active_rows)

    # Calendar-time: equal-weight average short return per day
    cal_port <- active_panel[, .(
      port_ret   = -mean(stock_ret, na.rm = TRUE),
      n_positions = .N
    ), by = date]

    # Merge with FF factors
    cal_port <- merge(cal_port, ff, by = "date", all.x = TRUE)
    cal_port <- cal_port[!is.na(mkt_rf)]
    cal_port[, port_excess := port_ret - rf]

    if (nrow(cal_port) < 10) next

    cal_reg <- lm(port_excess ~ mkt_rf + smb + hml, data = cal_port)
    s <- summary(cal_reg)
    ct <- s$coefficients

    alpha     <- ct["(Intercept)", "Estimate"]
    alpha_t   <- ct["(Intercept)", "t value"]
    alpha_p   <- ct["(Intercept)", "Pr(>|t|)"]
    alpha_ann <- alpha * 252
    r2        <- s$r.squared

    key <- paste0("cal_thresh_", thresh, "_day_", h)
    cal_results[[key]] <- list(
      threshold = thresh, horizon = h,
      n_days = nrow(cal_port),
      avg_positions = mean(cal_port$n_positions),
      alpha = alpha, alpha_ann = alpha_ann,
      alpha_t = alpha_t, alpha_p = alpha_p,
      mkt_beta = ct["mkt_rf", "Estimate"],
      smb_beta = ct["smb", "Estimate"],
      hml_beta = ct["hml", "Estimate"],
      r_squared = r2
    )

    cat(sprintf("  <%.2f Day %2d: alpha=%.4f (%.2f%% ann), t=%.2f, p=%s, R2=%.3f (%d days, avg %.1f pos)\n",
                thresh, h, alpha, alpha_ann * 100, alpha_t,
                format.pval(alpha_p, digits = 3), r2,
                nrow(cal_port), mean(cal_port$n_positions)))
  }
}

# Append calendar-time results to main results list
results <- c(results, cal_results)

# ═══════════════════════════════════════════════════════════════════════════
# SUMMARY TABLE
# ═══════════════════════════════════════════════════════════════════════════

cat("\n=== SUMMARY: RISK-ADJUSTED SHORT STRATEGY ALPHA (FF3) ===\n\n")
cat(sprintf("  %-6s  %3s  %4s  %9s  %9s  %7s  %7s  %7s  %7s  %5s\n",
            "Thresh", "Day", "n", "Alpha/d", "Alpha/yr", "t(a)",
            "Mkt-RF", "SMB", "HML", "R2"))
cat(paste(rep("-", 85), collapse = ""), "\n")

pooled_results <- results[grep("^thresh_", names(results))]
for (r in pooled_results) {
  cat(sprintf("  <%.2f   %3d  %4d  %+8.4f  %+8.1f%%  %6.2f  %+6.2f  %+6.2f  %+6.2f  %.3f\n",
              r$threshold, r$horizon, r$n_events,
              r$alpha, r$alpha_ann * 100, r$alpha_t,
              r$mkt_beta, r$smb_beta, r$hml_beta, r$r_squared))
}

# Calendar-time summary
cal_only <- results[grep("^cal_", names(results))]
if (length(cal_only) > 0) {
  cat("\n=== CALENDAR-TIME PORTFOLIO ALPHA (FF3) ===\n\n")
  cat(sprintf("  %-6s  %3s  %5s  %6s  %9s  %9s  %7s  %5s\n",
              "Thresh", "Day", "Days", "AvgPos", "Alpha/d", "Alpha/yr", "t(a)", "R2"))
  cat(paste(rep("-", 65), collapse = ""), "\n")

  for (r in cal_only) {
    cat(sprintf("  <%.2f   %3d  %5d  %5.1f  %+8.4f  %+8.1f%%  %6.2f  %.3f\n",
                r$threshold, r$horizon, r$n_days, r$avg_positions,
                r$alpha, r$alpha_ann * 100, r$alpha_t, r$r_squared))
  }
}

saveRDS(results, file.path(analysis_dir, "ff_alpha_results.rds"))
cat("\nSaved ff_alpha_results.rds\n")
