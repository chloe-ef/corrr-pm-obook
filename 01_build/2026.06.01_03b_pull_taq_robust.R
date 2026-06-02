# 2026.06.01_03b_pull_taq_robust.R
# Robust, RESUMABLE re-implementation of 03b_pull_taq.R. Same TAQ 5-minute ->
# daily close-to-close (market-hours) methodology and same outputs, but:
#   - connect() with gssencmode='disable' + retry/backoff (the plain pull
#     crashed mid-run on a transient GSSAPI/SSL drop)
#   - per-date checkpoint to a cache dir; cached dates are skipped on re-run
#   - all per-date errors caught + logged; reconnect on failure; never aborts
# Does not modify any original file.
#
# Input : build/2026.06.01_dome_eps_events.rds
# Output: build/2026.06.01_equity_daily_returns.rds, build/2026.06.01_index_daily_returns.rds
# Cache : build/taq_cache_2026.06.01/ctm_YYYYMMDD.rds  (one file per date)

suppressMessages({library(data.table); library(RPostgres); library(DBI)})
build_dir <- path.expand("~/Documents/data/corrr/390_paper/build")
cache_dir <- file.path(build_dir, "taq_cache_2026.06.01")
dir.create(cache_dir, showWarnings = FALSE)

events <- readRDS(file.path(build_dir, "2026.06.01_dome_eps_events.rds"))
earnings_tickers <- na.omit(unique(events$ticker))
tickers <- unique(c(earnings_tickers, "SPY"))

taq_aliases <- c("GOOGL"="GOOG","UHAL.B"="UHAL","MOG.A"="MOG")
taq_suffix_filter <- c("UHAL"="B","MOG"="A")
alias_reverse <- setNames(names(taq_aliases), taq_aliases)

events_dt <- unique(events[!is.na(ticker) & !is.na(earnings_date), .(ticker, earnings_date)])
date_min <- min(events_dt$earnings_date) - 12
date_max <- max(events_dt$earnings_date) + 12
all_days <- seq.Date(date_min, date_max, by="day")
weekdays_only <- all_days[!weekdays(all_days) %in% c("Saturday","Sunday")]
query_dates_list <- events_dt[, {
  taq_tk <- ifelse(ticker %in% names(taq_aliases), taq_aliases[ticker], ticker)
  .(taq_ticker=taq_tk, query_date=weekdays_only[weekdays_only>=earnings_date-10 & weekdays_only<=earnings_date+10])
}, by=.(ticker, earnings_date)]
date_ticker_map <- unique(query_dates_list[, .(query_date, taq_ticker)])
all_query_dates <- unique(date_ticker_map$query_date)
date_ticker_map <- unique(rbind(date_ticker_map, data.table(query_date=all_query_dates, taq_ticker="SPY")))
unique_dates <- sort(unique(date_ticker_map$query_date))
cat("query dates:", length(unique_dates), " range:", as.character(range(unique_dates)), "\n")

connect <- function(tries=6){
  for(i in seq_len(tries)){
    con <- tryCatch(dbConnect(Postgres(), host="wrds-pgdata.wharton.upenn.edu", port=9737,
        dbname="wrds", user=Sys.getenv("wrds_username"), password=Sys.getenv("wrds_password"),
        sslmode="require", gssencmode="disable"),
      error=function(e){cat("  connect try",i,"failed\n"); NULL})
    if(!is.null(con)) return(con)
    Sys.sleep(2*i)
  }
  stop("could not connect after ", tries, " tries")
}

build_taq_query <- function(table_name, ticker_list_sql){
  sprintf("SELECT sym_root AS ticker, sym_suffix,
      CAST(date_trunc('hour', time_m) + INTERVAL '1 min' * (EXTRACT(MINUTE FROM time_m)::int / 5 * 5) AS text) AS bar_time_str,
      (array_agg(price ORDER BY time_m ASC))[1] AS open, MAX(price) AS high, MIN(price) AS low,
      (array_agg(price ORDER BY time_m DESC))[1] AS close, SUM(size) AS volume, COUNT(*) AS n_trades
    FROM %s WHERE sym_root IN (%s) AND price>0 AND tr_corr IN ('00','01') AND size>0
    GROUP BY sym_root, sym_suffix, bar_time_str ORDER BY sym_root, sym_suffix, bar_time_str",
    table_name, ticker_list_sql)
}

con <- connect(); cat("connected (gssencmode=disable)\n")
CHUNK <- 50L; n_dates <- length(unique_dates); n_done <- 0L; n_skip <- 0L; n_miss <- 0L
for(i in seq_along(unique_dates)){
  d <- unique_dates[i]
  cache_f <- file.path(cache_dir, paste0("ctm_", format(d,"%Y%m%d"), ".rds"))
  if(file.exists(cache_f)){ n_skip <- n_skip+1L; next }   # resumable
  table_name <- paste0("taqmsec.ctm_", format(d,"%Y%m%d"))
  day_tickers <- date_ticker_map[query_date==d, taq_ticker]
  chunks <- split(day_tickers, ceiling(seq_along(day_tickers)/CHUNK))
  day_results <- list(); missing <- FALSE
  for(ch in chunks){
    q <- build_taq_query(table_name, paste0("'",ch,"'",collapse=","))
    cr <- tryCatch(as.data.table(dbGetQuery(con, q)), error=function(e){
      msg <- conditionMessage(e)
      if(grepl("does not exist", msg)) { missing <<- TRUE; return(NULL) }
      # transient/connection error: reconnect and retry once
      cat(sprintf("  [%s] err: %s -- reconnecting\n", format(d,"%Y-%m-%d"), substr(msg,1,50)))
      tryCatch(dbDisconnect(con), error=function(e2) NULL)
      con <<- connect()
      tryCatch(as.data.table(dbGetQuery(con, q)), error=function(e3) NULL)
    })
    if(!is.null(cr) && nrow(cr)>0) day_results[[length(day_results)+1]] <- cr
  }
  if(length(day_results)>0){
    dt <- rbindlist(day_results, fill=TRUE); dt[, date := d]
    saveRDS(dt, cache_f); n_done <- n_done+1L
  } else {
    # write empty marker so missing/holiday dates are not retried endlessly
    saveRDS(data.table(), cache_f); if(missing) n_miss <- n_miss+1L
  }
  if(i %% 10 == 0 || i == n_dates)
    cat(sprintf("  [%d/%d] %s  done=%d skip=%d miss=%d\n", i, n_dates, format(d,"%Y-%m-%d"), n_done, n_skip, n_miss))
}
tryCatch(dbDisconnect(con), error=function(e) NULL)
cat(sprintf("\nPull loop complete. new=%d skipped=%d missing-tables=%d\n", n_done, n_skip, n_miss))

# ── Assemble cached per-date bars -> daily returns (mirror original) ───────
files <- list.files(cache_dir, pattern="^ctm_.*\\.rds$", full.names=TRUE)
bars <- rbindlist(lapply(files, readRDS), fill=TRUE)
bars <- bars[!is.na(ticker)]
cat("raw bars assembled:", format(nrow(bars), big.mark=","), "from", length(files), "date files\n")

for(root in names(taq_suffix_filter)){
  tgt <- taq_suffix_filter[[root]]
  bars <- bars[!(ticker==root & (is.na(sym_suffix) | sym_suffix!=tgt))]
}
share_class_roots <- names(taq_suffix_filter)
bars <- bars[ticker %in% share_class_roots | is.na(sym_suffix) | sym_suffix==""]
bars[, sym_suffix := NULL]
for(tn in names(alias_reverse)) bars[ticker==tn, ticker := alias_reverse[[tn]]]

bars[, bar_time := as.POSIXct(paste(as.character(date), bar_time_str), format="%Y-%m-%d %H:%M:%S", tz="America/New_York")]
bars[, bar_hour := as.integer(format(bar_time,"%H"))]
bars[, bar_min  := as.integer(format(bar_time,"%M"))]
bars[, market_hours := (bar_hour>9 | (bar_hour==9 & bar_min>=30)) & (bar_hour<16)]
mkt <- bars[market_hours==TRUE]; setorder(mkt, ticker, date, bar_time)
daily <- mkt[, .(Open=open[1], High=max(high), Low=min(low), Close=close[.N], Volume=sum(volume,na.rm=TRUE)),
             by=.(ticker, date)]
setorder(daily, ticker, date)
daily[, stock_return := Close/shift(Close) - 1, by=ticker]
cat("daily rows:", format(nrow(daily),big.mark=","), " tickers:", uniqueN(daily$ticker),
    " range:", as.character(range(daily$date)), "\n")
saveRDS(daily, file.path(build_dir, "2026.06.01_equity_daily_returns.rds"))

spy_daily <- daily[ticker=="SPY", .(date, sp500_ret=stock_return)]; setorder(spy_daily, date)
saveRDS(spy_daily, file.path(build_dir, "2026.06.01_index_daily_returns.rds"))
cat("SPY trading days:", nrow(spy_daily), "\nSaved equity + index daily returns.\n")
