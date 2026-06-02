# 03_pull_ibes.R — Pull IBES consensus & actuals for Dome EPS events
# Input:  build/dome_eps_events.rds
# Output: build/ibes_data.rds, build/ibes_history.rds
#
# Matches on oftic (= exchange ticker). IBES internal ticker differs for many
# names (JPM→CHL, META→FBK, MU→DRAM) but oftic equals the exchange symbol.
#
# Revision 4: Expanded date range to 3 years back for rolling 12-quarter sigma.
# Patch 1:    Pulls adjfac (adjustment factor) for stock split correction.
# Patch 2:    Parallel GAAP / Non-GAAP data streams with source_table tracking.

library(data.table)
library(RPostgres)
library(DBI)

build_dir <- "~/Documents/data/corrr/390_paper/build"

events <- readRDS(file.path(build_dir, "2026.06.01_dome_eps_events.rds"))
tickers <- na.omit(unique(events$ticker))
cat(length(tickers), "unique tickers to query\n")

# Expanded date range: 3 years back for 12-quarter rolling sigma (Rev 4)
date_min_history <- min(events$earnings_date, na.rm = TRUE) - 1095  # ~3 years
date_min <- min(events$earnings_date, na.rm = TRUE) - 90
date_max <- max(events$earnings_date, na.rm = TRUE) + 30
cat("Event date range:", as.character(date_min), "to", as.character(date_max), "\n")
cat("History date range:", as.character(date_min_history), "to", as.character(date_max), "\n")

# ─── WRDS connection ──────────────────────────────────────────────────────
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

ticker_sql <- paste0("'", tickers, "'", collapse = ",")

# ─── 1. Map exchange tickers to IBES tickers via oftic ────────────────────
oftic_q <- sprintf("
  SELECT DISTINCT ticker AS ibes_ticker, oftic
  FROM ibes.id
  WHERE oftic IN (%s)
", ticker_sql)

oftic_map <- as.data.table(dbGetQuery(con, oftic_q))
cat("IBES oftic matched:", uniqueN(oftic_map$oftic), "of", length(tickers), "tickers\n")

unmatched <- setdiff(tickers, oftic_map$oftic)
if (length(unmatched) > 0) {
  cat("Unmatched:", paste(head(unmatched, 20), collapse = ", "), "\n")
}

if (nrow(oftic_map) == 0) {
  dbDisconnect(con)
  stop("No IBES matches found")
}

ibes_ticker_sql <- paste0("'", unique(oftic_map$ibes_ticker), "'", collapse = ",")

# ═══════════════════════════════════════════════════════════════════════════
# PARALLEL GAAP / NON-GAAP DATA STREAMS (Patch 2)
# Keep entirely separate until after sigma computation.
# ═══════════════════════════════════════════════════════════════════════════

# ─── 2. Non-GAAP consensus (ibes.statsumu_epsus, measure = 'EPS') ────────
# Pull full 3-year history for sigma calculation (Rev 4)
cat("\nPulling non-GAAP consensus (3yr history)...\n")
nongaap_cons_q <- sprintf("
  SELECT ticker, statpers, fpedats, measure, fpi,
         meanest, medest, stdev, numest,
         numup, numdown
  FROM ibes.statsumu_epsus
  WHERE ticker IN (%s)
    AND measure = 'EPS'
    AND fpi IN ('6','7','8','9')
    AND statpers BETWEEN '%s' AND '%s'
  ORDER BY ticker, fpedats, statpers
", ibes_ticker_sql, as.character(date_min_history), as.character(date_max))

nongaap_cons <- as.data.table(dbGetQuery(con, nongaap_cons_q))
nongaap_cons[, measure_type := "nongaap"]
nongaap_cons[, source_table := "epsus"]
cat("Non-GAAP consensus rows:", format(nrow(nongaap_cons), big.mark = ","), "\n")

# ─── 3. GAAP consensus (ibes.statsumu_xepsus, measure = 'GPS') ───────────
cat("Pulling GAAP consensus (3yr history)...\n")
gaap_cons_q <- sprintf("
  SELECT ticker, statpers, fpedats, measure, fpi,
         meanest, medest, stdev, numest,
         numup, numdown
  FROM ibes.statsumu_xepsus
  WHERE ticker IN (%s)
    AND measure = 'GPS'
    AND fpi IN ('6','7','8','9')
    AND statpers BETWEEN '%s' AND '%s'
  ORDER BY ticker, fpedats, statpers
", ibes_ticker_sql, as.character(date_min_history), as.character(date_max))

gaap_cons <- as.data.table(dbGetQuery(con, gaap_cons_q))
gaap_cons[, measure_type := "gaap"]
gaap_cons[, source_table := "xepsus"]
cat("GAAP consensus rows:", format(nrow(gaap_cons), big.mark = ","), "\n")

# Combine consensus (preserving source_table)
consensus <- rbindlist(list(nongaap_cons, gaap_cons), use.names = TRUE)
consensus[, statpers := as.Date(statpers)]
consensus[, fpedats  := as.Date(fpedats)]

# ─── 4. Non-GAAP actuals (ibes.actu_epsus, measure = 'EPS') ──────────────
# Pull full 3-year history for sigma (Rev 4)
cat("Pulling non-GAAP actuals (3yr history)...\n")
nongaap_act_q <- sprintf("
  SELECT ticker, measure, pends AS fpedats, value AS actual_eps, anndats
  FROM ibes.actu_epsus
  WHERE ticker IN (%s)
    AND measure = 'EPS'
    AND pends BETWEEN '%s' AND '%s'
", ibes_ticker_sql, as.character(date_min_history), as.character(date_max))

nongaap_act <- as.data.table(dbGetQuery(con, nongaap_act_q))
nongaap_act[, measure_type := "nongaap"]
nongaap_act[, source_table := "epsus"]
cat("Non-GAAP actuals:", nrow(nongaap_act), "\n")

# ─── 5. GAAP actuals (ibes.actu_xepsus, measure = 'GPS') ─────────────────
cat("Pulling GAAP actuals (3yr history)...\n")
gaap_act_q <- sprintf("
  SELECT ticker, measure, pends AS fpedats, value AS actual_eps, anndats
  FROM ibes.actu_xepsus
  WHERE ticker IN (%s)
    AND measure = 'GPS'
    AND pends BETWEEN '%s' AND '%s'
", ibes_ticker_sql, as.character(date_min_history), as.character(date_max))

gaap_act <- as.data.table(dbGetQuery(con, gaap_act_q))
gaap_act[, measure_type := "gaap"]
gaap_act[, source_table := "xepsus"]
cat("GAAP actuals:", nrow(gaap_act), "\n")

# Combine actuals (preserving source_table)
actuals <- rbindlist(list(nongaap_act, gaap_act), use.names = TRUE)
actuals[, fpedats := as.Date(fpedats)]
actuals[, anndats := as.Date(anndats)]

# ─── 5b. Pull adjustment factors for stock split correction (Patch 1) ────
cat("Pulling adjustment factors...\n")
adjfac_q <- sprintf("
  SELECT ticker, spdates AS adj_date, adj AS adjfac
  FROM ibes.adj
  WHERE ticker IN (%s)
  ORDER BY ticker, spdates
", ibes_ticker_sql)

adjfac <- tryCatch({
  dt <- as.data.table(dbGetQuery(con, adjfac_q))
  dt[, adj_date := as.Date(adj_date)]
  cat("Adjustment factor rows:", nrow(dt), "\n")
  dt
}, error = function(e) {
  cat("WARNING: adjfac query failed:", conditionMessage(e), "\n")
  cat("Continuing without split adjustment\n")
  data.table()
})

# ─── 6. Pre-computed surprise (ibes.surpsumu) ────────────────────────────
cat("Pulling surprise data...\n")

surp_q <- sprintf("
  SELECT ticker, measure, anndats, surpmean, suescore
  FROM ibes.surpsumu
  WHERE ticker IN (%s)
    AND measure IN ('EPS', 'GPS')
    AND anndats BETWEEN '%s' AND '%s'
", ibes_ticker_sql, as.character(date_min), as.character(date_max))

surprise <- tryCatch({
  dt <- as.data.table(dbGetQuery(con, surp_q))
  dt[, anndats  := as.Date(anndats)]
  dt[, measure_type := fifelse(measure == "GPS", "gaap", "nongaap")]
  cat("Surprise rows:", nrow(dt), "\n")
  dt
}, error = function(e) {
  cat("WARNING: surpsumu query failed:", conditionMessage(e), "\n")
  cat("Continuing without pre-computed surprise data\n")
  data.table()
})

dbDisconnect(con)
cat("Disconnected from WRDS\n")

# ═══════════════════════════════════════════════════════════════════════════
# 6b. BUILD HISTORICAL CONSENSUS+ACTUALS FOR ROLLING SIGMA (Rev 4, Patch 2)
# Keep GAAP and Non-GAAP separate per Patch 2 requirements.
# ═══════════════════════════════════════════════════════════════════════════

cat("\n=== BUILDING IBES HISTORY FOR ROLLING SIGMA ===\n")

# Add oftic mapping to all tables (including sub-tables used by build_history)
consensus[oftic_map, on = .(ticker = ibes_ticker), oftic := i.oftic]
actuals[oftic_map, on = .(ticker = ibes_ticker), oftic := i.oftic]
nongaap_cons[oftic_map, on = .(ticker = ibes_ticker), oftic := i.oftic]
gaap_cons[oftic_map, on = .(ticker = ibes_ticker), oftic := i.oftic]
nongaap_act[oftic_map, on = .(ticker = ibes_ticker), oftic := i.oftic]
gaap_act[oftic_map, on = .(ticker = ibes_ticker), oftic := i.oftic]
if (nrow(surprise) > 0) {
  surprise[oftic_map, on = .(ticker = ibes_ticker), oftic := i.oftic]
}

# For each (ticker, fpedats, measure_type) pair, get the last consensus and actual
# This gives us the historical surprise series needed for rolling sigma

build_history <- function(cons_dt, act_dt, basis_label) {
  # For each actual, find the last consensus before the announcement
  hist_pairs <- lapply(seq_len(nrow(act_dt)), function(i) {
    a <- act_dt[i]
    if (is.na(a$oftic)) return(NULL)
    cs <- cons_dt[oftic == a$oftic & fpedats == a$fpedats & statpers <= a$anndats]
    if (nrow(cs) == 0) return(NULL)
    best <- cs[which.max(statpers)]
    data.table(
      oftic            = a$oftic,
      ibes_ticker      = a$ticker,
      fpedats          = a$fpedats,
      anndats          = a$anndats,
      actual_eps       = a$actual_eps,
      consensus_mean   = best$meanest,
      consensus_median = best$medest,
      statpers         = best$statpers,
      source_table     = a$source_table,
      accounting_basis = basis_label
    )
  })
  rbindlist(hist_pairs[!vapply(hist_pairs, is.null, logical(1))])
}

# Build separate histories (Patch 2: never merge until after sigma)
nongaap_history <- build_history(
  nongaap_cons[!is.na(oftic)], nongaap_act[!is.na(oftic)], "Non-GAAP"
)
gaap_history <- build_history(
  gaap_cons[!is.na(oftic)], gaap_act[!is.na(oftic)], "GAAP"
)

cat("Non-GAAP history pairs (raw):", nrow(nongaap_history), "\n")
cat("GAAP history pairs (raw):", nrow(gaap_history), "\n")

# Deduplicate: IBES returns both annual (fpi=6/CY) and quarterly actuals for the
# same fpedats. Keep only the row with the smallest |surprise| per
# (oftic, accounting_basis, fpedats) — that's the quarterly match.
dedup_history <- function(dt) {
  dt[, raw_surprise := actual_eps - consensus_mean]
  dt[, abs_surp := abs(raw_surprise)]
  dt <- dt[dt[, .I[which.min(abs_surp)], by = .(oftic, accounting_basis, fpedats)]$V1]
  dt[, c("raw_surprise", "abs_surp") := NULL]
  dt
}

nongaap_history <- dedup_history(nongaap_history)
gaap_history    <- dedup_history(gaap_history)
cat("Non-GAAP after dedup:", nrow(nongaap_history), "\n")
cat("GAAP after dedup:", nrow(gaap_history), "\n")

# Apply stock split adjustment (Patch 1)
# Divide historical EPS by adjfac relative to the most recent quarter
if (nrow(adjfac) > 0) {
  apply_adjfac <- function(hist_dt) {
    hist_dt[, adj_eps := actual_eps]
    hist_dt[, adj_cons := consensus_mean]
    for (i in seq_len(nrow(hist_dt))) {
      tk <- hist_dt$ibes_ticker[i]
      dt <- hist_dt$fpedats[i]
      # Get cumulative adjustment factor at this date
      adj_sub <- adjfac[ticker == tk & adj_date <= dt]
      if (nrow(adj_sub) > 0) {
        cum_adj <- adj_sub[which.max(adj_date), adjfac]
        if (!is.na(cum_adj) && cum_adj != 0) {
          set(hist_dt, i, "adj_eps", hist_dt$actual_eps[i] / cum_adj)
          set(hist_dt, i, "adj_cons", hist_dt$consensus_mean[i] / cum_adj)
        }
      }
    }
    hist_dt
  }
  nongaap_history <- apply_adjfac(nongaap_history)
  gaap_history <- apply_adjfac(gaap_history)
  cat("Split adjustment applied\n")
} else {
  nongaap_history[, adj_eps := actual_eps]
  nongaap_history[, adj_cons := consensus_mean]
  gaap_history[, adj_eps := actual_eps]
  gaap_history[, adj_cons := consensus_mean]
}

# Compute surprise using split-adjusted values
nongaap_history[, surprise := adj_eps - adj_cons]
gaap_history[, surprise := adj_eps - adj_cons]

# Combine and save full history (for 06_implied_eps.py)
ibes_history <- rbindlist(list(nongaap_history, gaap_history), use.names = TRUE)
setorder(ibes_history, oftic, accounting_basis, fpedats)

# Hard assertions (Patch 2): verify no cross-contamination
stopifnot(all(ibes_history[accounting_basis == "GAAP", source_table] == "xepsus"))
stopifnot(all(ibes_history[accounting_basis == "Non-GAAP", source_table] == "epsus"))

cat("History rows:", nrow(ibes_history), "\n")
cat("Unique tickers in history:", uniqueN(ibes_history$oftic), "\n")

saveRDS(ibes_history, file.path(build_dir, "2026.06.01_ibes_history.rds"))
cat("Saved ibes_history.rds\n")

# ─── 7. For each event, find best-matching consensus + actual ─────────────

ibes_matched <- events[!is.na(ticker) & !is.na(earnings_date),
                        .(ticker, earnings_date, eps_type, market_slug)]

ibes_matched[, ibes_row := seq_len(.N)]

results <- lapply(seq_len(nrow(ibes_matched)), function(i) {
  ev <- ibes_matched[i]

  # Find actuals for this ticker/eps_type with fpedats near earnings_date
  act_sub <- actuals[oftic == ev$ticker & measure_type == ev$eps_type &
                     fpedats >= ev$earnings_date - 90 &
                     fpedats <= ev$earnings_date + 7]
  if (nrow(act_sub) == 0) return(NULL)

  # Pick fpedats closest to earnings_date
  act_sub[, date_diff := abs(as.numeric(fpedats - ev$earnings_date))]
  best_act <- act_sub[which.min(date_diff)]

  # Now find consensus for same ticker/fpedats, last statpers before earnings_date
  cons_sub <- consensus[oftic == ev$ticker & measure_type == ev$eps_type &
                        fpedats == best_act$fpedats &
                        statpers <= ev$earnings_date]
  if (nrow(cons_sub) == 0) return(NULL)
  best_cons <- cons_sub[which.max(statpers)]

  # Surprise data (surpsumu uses anndats, not fpedats)
  sue <- NA_real_
  if (nrow(surprise) > 0) {
    surp_sub <- surprise[oftic == ev$ticker & measure_type == ev$eps_type &
                         anndats >= ev$earnings_date - 7 &
                         anndats <= ev$earnings_date + 7]
    if (nrow(surp_sub) > 0) sue <- surp_sub[which.max(anndats)]$suescore
  }

  data.table(
    market_slug        = ev$market_slug,
    ibes_ticker        = best_cons$ticker,
    fpedats            = best_act$fpedats,
    consensus_mean     = best_cons$meanest,
    consensus_median   = best_cons$medest,
    consensus_stdev    = best_cons$stdev,
    num_analysts       = best_cons$numest,
    num_up             = best_cons$numup,
    num_down           = best_cons$numdown,
    actual_eps         = best_act$actual_eps,
    announcement_date  = best_act$anndats,
    consensus_date     = best_cons$statpers,  # Rev 1: save statpers
    source_table       = best_cons$source_table,  # Patch 2: track source
    sue_score          = sue
  )
})

ibes_out <- rbindlist(results[!vapply(results, is.null, logical(1))])

cat("\n=== IBES MATCHING RESULTS ===\n")
cat("Events matched:", nrow(ibes_out), "of", nrow(ibes_matched), "\n")
cat("Match rate:", round(nrow(ibes_out) / nrow(ibes_matched) * 100, 1), "%\n")
cat("Unique tickers matched:", uniqueN(ibes_out$ibes_ticker), "\n")

# Patch 2 assertion on matched output
if (nrow(events[eps_type == "gaap"]) > 0) {
  gaap_slugs <- events[eps_type == "gaap", market_slug]
  gaap_matched <- ibes_out[market_slug %in% gaap_slugs]
  if (nrow(gaap_matched) > 0) {
    stopifnot(all(gaap_matched$source_table == "xepsus"))
  }
}
if (nrow(events[eps_type == "nongaap"]) > 0) {
  nongaap_slugs <- events[eps_type == "nongaap", market_slug]
  nongaap_matched <- ibes_out[market_slug %in% nongaap_slugs]
  if (nrow(nongaap_matched) > 0) {
    stopifnot(all(nongaap_matched$source_table == "epsus"))
  }
}

saveRDS(ibes_out, file.path(build_dir, "2026.06.01_ibes_data.rds"))
cat("\nSaved ibes_data.rds\n")
