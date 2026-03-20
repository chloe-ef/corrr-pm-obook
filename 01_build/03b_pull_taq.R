# 03b_pull_taq.R — Pull TAQ 5-minute bars, aggregate to daily + index returns
# Input:  build/dome_eps_events.rds (ticker list + earnings dates)
# Output: build/equity_daily_returns.rds, build/index_daily_returns.rds
#
# Uses WRDS TAQ millisecond data (taqmsec schema).
# Each trading day has its own table: taqmsec.ctm_YYYYMMDD
# We aggregate to 5-minute OHLCV bars server-side, then collapse to daily in R.
#
# Trading calendar: generated from weekdays in range; non-existent tables
# (holidays) are caught and skipped gracefully.

library(data.table)
library(RPostgres)
library(DBI)

data_root <- Sys.getenv("DATA_DIR", file.path(getwd(), "data"))
build_dir <- file.path(data_root, "build")

# =====================================================================
# LOAD EVENTS + DETERMINE QUERY SCOPE
# =====================================================================

events <- readRDS(file.path(build_dir, "dome_eps_events.rds"))
earnings_tickers <- na.omit(unique(events$ticker))
cat("Earnings tickers:", length(earnings_tickers), "\n")

# Include SPY for benchmark excess returns
tickers <- unique(c(earnings_tickers, "SPY"))

# TAQ sym_root aliases
taq_aliases <- c("GOOGL" = "GOOG", "UHAL.B" = "UHAL", "MOG.A" = "MOG")
taq_suffix_filter <- c("UHAL" = "B", "MOG" = "A")
tickers_taq <- ifelse(tickers %in% names(taq_aliases),
                      taq_aliases[tickers], tickers)
alias_reverse <- setNames(names(taq_aliases), taq_aliases)

# Per-event date windows: ±7 weekdays around each earnings_date
events_dt <- unique(events[!is.na(ticker) & !is.na(earnings_date),
                           .(ticker, earnings_date)])

# Generate candidate trading days (weekdays only)
date_min <- min(events_dt$earnings_date) - 12  # ~7 bdays buffer
date_max <- max(events_dt$earnings_date) + 12
all_days <- seq.Date(date_min, date_max, by = "day")
weekdays_only <- all_days[!weekdays(all_days) %in% c("Saturday", "Sunday")]

# For each event, expand to the ±10 calendar day window (covers ≥7 trading days)
query_dates_list <- events_dt[, {
  d_from <- earnings_date - 10
  d_to   <- earnings_date + 10
  taq_tk <- ifelse(ticker %in% names(taq_aliases), taq_aliases[ticker], ticker)
  .(taq_ticker = taq_tk,
    query_date = weekdays_only[weekdays_only >= d_from & weekdays_only <= d_to])
}, by = .(ticker, earnings_date)]

date_ticker_map <- unique(query_dates_list[, .(query_date, taq_ticker)])

# Ensure SPY is pulled for every query date
all_query_dates <- unique(date_ticker_map$query_date)
bench_rows <- data.table(query_date = all_query_dates, taq_ticker = "SPY")
date_ticker_map <- unique(rbind(date_ticker_map, bench_rows))
unique_dates <- sort(unique(date_ticker_map$query_date))

cat("Unique query dates:", length(unique_dates), "\n")
cat("Total (date, ticker) pairs:", nrow(date_ticker_map), "\n")
cat("Date range:", as.character(range(unique_dates)), "\n")

# =====================================================================
# WRDS CONNECTION
# =====================================================================

connect_to_wrds <- function() {
  wrds_user <- Sys.getenv("wrds_username")
  wrds_pass <- Sys.getenv("wrds_password")
  if (wrds_user == "" || wrds_pass == "")
    stop("Set wrds_username and wrds_password environment variables")
  dbConnect(Postgres(),
            host = "wrds-pgdata.wharton.upenn.edu", port = 9737,
            dbname = "wrds", user = wrds_user, password = wrds_pass,
            sslmode = "require")
}

con <- connect_to_wrds()
cat("Connected to WRDS\n")

# =====================================================================
# TEST TAQ ACCESS
# =====================================================================

test_date <- unique_dates[1]
test_table <- paste0("taqmsec.ctm_", format(test_date, "%Y%m%d"))
test_q <- sprintf("SELECT COUNT(*) AS n FROM %s WHERE sym_root = 'SPY' AND price > 0 LIMIT 1",
                  test_table)

taq_ok <- tryCatch({
  test_res <- dbGetQuery(con, test_q)
  cat("TAQ access test passed. Table", test_table, "exists (",
      test_res$n, "rows for SPY)\n")
  TRUE
}, error = function(e) {
  cat("TAQ access test FAILED:", conditionMessage(e), "\n")
  FALSE
})

if (!taq_ok) {
  dbDisconnect(con)
  stop("Cannot access TAQ data.")
}

# =====================================================================
# PULL 5-MINUTE BARS
# =====================================================================

build_taq_query <- function(table_name, ticker_list_sql) {
  sprintf("
    SELECT
      sym_root AS ticker,
      sym_suffix,
      CAST(
        date_trunc('hour', time_m)
          + INTERVAL '1 min' * (EXTRACT(MINUTE FROM time_m)::int / 5 * 5)
        AS text
      ) AS bar_time_str,
      (array_agg(price ORDER BY time_m ASC))[1]  AS open,
      MAX(price)  AS high,
      MIN(price)  AS low,
      (array_agg(price ORDER BY time_m DESC))[1] AS close,
      SUM(size)   AS volume,
      COUNT(*)    AS n_trades
    FROM %s
    WHERE sym_root IN (%s)
      AND price > 0
      AND tr_corr IN ('00', '01')
      AND size > 0
    GROUP BY sym_root, sym_suffix, bar_time_str
    ORDER BY sym_root, sym_suffix, bar_time_str
  ", table_name, ticker_list_sql)
}

all_bars <- list()
n_dates <- length(unique_dates)
CHUNK_SIZE <- 50L

cat("\nPulling 5-minute bars for", n_dates, "dates...\n")

for (i in seq_along(unique_dates)) {
  d <- unique_dates[i]
  table_name <- paste0("taqmsec.ctm_", format(d, "%Y%m%d"))

  day_tickers <- date_ticker_map[query_date == d, taq_ticker]
  chunks <- split(day_tickers, ceiling(seq_along(day_tickers) / CHUNK_SIZE))

  day_results <- list()
  for (ch in chunks) {
    ticker_sql <- paste0("'", ch, "'", collapse = ",")
    q <- build_taq_query(table_name, ticker_sql)

    chunk_result <- tryCatch({
      as.data.table(dbGetQuery(con, q))
    }, error = function(e) {
      msg <- conditionMessage(e)
      # Table doesn't exist = holiday, skip silently
      if (!grepl("does not exist", msg)) {
        cat("  WARNING:", format(d, "%Y-%m-%d"), ":", msg, "\n")
      }
      tryCatch({
        dbDisconnect(con)
        con <<- connect_to_wrds()
      }, error = function(e2) NULL)
      NULL
    })

    if (!is.null(chunk_result) && nrow(chunk_result) > 0) {
      day_results[[length(day_results) + 1]] <- chunk_result
    }
  }

  if (length(day_results) > 0) {
    dt <- rbindlist(day_results, fill = TRUE)
    dt[, date := d]
    dt[, bar_time := as.POSIXct(paste(as.character(d), bar_time_str),
                                 format = "%Y-%m-%d %H:%M:%S",
                                 tz = "America/New_York")]
    dt[, bar_time_str := NULL]
    all_bars[[length(all_bars) + 1]] <- dt
  }

  if (i %% 10 == 0 || i == n_dates) {
    cat(sprintf("  [%d/%d] %s — %s bars\n",
                i, n_dates, format(d, "%Y-%m-%d"),
                format(sum(vapply(all_bars, nrow, integer(1))), big.mark = ",")))
  }
}

dbDisconnect(con)
cat("Disconnected from WRDS\n")

if (length(all_bars) == 0) stop("No TAQ data retrieved.")

bars <- rbindlist(all_bars, fill = TRUE)
cat("\nRaw bars:", format(nrow(bars), big.mark = ","), "\n")

# =====================================================================
# CLEAN: SHARE-CLASS FILTERING + ALIAS MAPPING
# =====================================================================

# Filter share-class tickers
for (root in names(taq_suffix_filter)) {
  target_suffix <- taq_suffix_filter[[root]]
  bars <- bars[!(ticker == root & (is.na(sym_suffix) | sym_suffix != target_suffix))]
}

# For non-share-class tickers, keep only common stock (NA or empty suffix)
share_class_roots <- names(taq_suffix_filter)
bars <- bars[ticker %in% share_class_roots | is.na(sym_suffix) | sym_suffix == ""]
bars[, sym_suffix := NULL]

# Map TAQ sym_root back to original ticker names
for (taq_name in names(alias_reverse)) {
  orig_name <- alias_reverse[[taq_name]]
  bars[ticker == taq_name, ticker := orig_name]
}

# Flag market hours (9:30–16:00 ET)
bars[, bar_hour := as.integer(format(bar_time, "%H"))]
bars[, bar_min  := as.integer(format(bar_time, "%M"))]
bars[, market_hours := (bar_hour > 9 | (bar_hour == 9 & bar_min >= 30)) & (bar_hour < 16)]

cat("After cleaning:", format(nrow(bars), big.mark = ","), "bars\n")
cat("Tickers:", uniqueN(bars$ticker), "\n")
cat("Market-hours:", round(mean(bars$market_hours) * 100, 1), "%\n")

# =====================================================================
# AGGREGATE TO DAILY RETURNS
# =====================================================================

mkt <- bars[market_hours == TRUE]
setorder(mkt, ticker, date, bar_time)

daily <- mkt[, .(
  Open   = open[1],
  High   = max(high),
  Low    = min(low),
  Close  = close[.N],
  Volume = sum(volume, na.rm = TRUE)
), by = .(ticker, date)]

setorder(daily, ticker, date)
daily[, stock_return := Close / shift(Close) - 1, by = ticker]

cat("\n=== DAILY RETURNS ===\n")
cat("Rows:", format(nrow(daily), big.mark = ","), "\n")
cat("Tickers:", uniqueN(daily$ticker), "\n")
cat("Date range:", as.character(range(daily$date)), "\n")

saveRDS(daily, file.path(build_dir, "equity_daily_returns.rds"))
cat("Saved equity_daily_returns.rds\n")

# =====================================================================
# INDEX RETURNS (SPY)
# =====================================================================

spy_daily <- daily[ticker == "SPY", .(date, sp500_ret = stock_return)]
setorder(spy_daily, date)

cat("\n=== INDEX RETURNS ===\n")
cat("SPY trading days:", nrow(spy_daily), "\n")

saveRDS(spy_daily, file.path(build_dir, "index_daily_returns.rds"))
cat("Saved index_daily_returns.rds\n")

cat("\nDone.\n")
