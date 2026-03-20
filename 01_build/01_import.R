# 01_import.R — Import raw data + rebuild classification & hourly probabilities
#
# This script makes 390_paper self-contained by:
#   1. Copying dome_trades_combined.rds (pre-built from 6GB raw CSVs)
#   2. Copying reference files (closed_earnings_markets.csv, masterlist)
#   3. Rebuilding market_classification.rds from dome_trades_combined
#   4. Rebuilding hourly_market_probabilities.rds from dome_trades_combined
#
# Input:  <EXPLORATION_DIR>/build/dome_trades_combined.rds
#         <EXPLORATION_DIR>/export/closed_earnings_markets.csv
#         <EXPLORATION_DIR>/market_slugs_masterlist.xlsx
# Output: import/dome_trades_combined.rds
#         import/closed_earnings_markets.csv
#         import/market_slugs_masterlist.xlsx
#         build/market_classification.rds
#         build/hourly_market_probabilities.rds

library(data.table)
library(readxl)

data_root       <- Sys.getenv("DATA_DIR", file.path(getwd(), "data"))
exploration_dir <- Sys.getenv("EXPLORATION_DIR", file.path(dirname(data_root), "390_exploration"))
import_dir      <- file.path(data_root, "import")
build_dir       <- file.path(data_root, "build")

# ═══════════════════════════════════════════════════════════════════════════
# 1. COPY RAW INPUTS TO IMPORT/
# ═══════════════════════════════════════════════════════════════════════════

copy_file <- function(src, dst) {
  if (!file.exists(src)) {
    cat("SKIP (not found):", src, "\n")
    return(FALSE)
  }
  file.copy(src, dst, overwrite = TRUE)
  cat("Copied:", basename(src), "(", round(file.size(src) / 1e6), "MB)\n")
  TRUE
}

cat("=== COPYING RAW INPUTS ===\n")
copy_file(file.path(exploration_dir, "build", "dome_trades_combined.rds"),
          file.path(import_dir, "dome_trades_combined.rds"))

copy_file(file.path(exploration_dir, "export", "closed_earnings_markets.csv"),
          file.path(import_dir, "closed_earnings_markets.csv"))

copy_file(file.path(exploration_dir, "market_slugs_masterlist.xlsx"),
          file.path(import_dir, "market_slugs_masterlist.xlsx"))

# ═══════════════════════════════════════════════════════════════════════════
# 2. BUILD MARKET CLASSIFICATION (from 390_exploration/02_classify_markets.R)
# ═══════════════════════════════════════════════════════════════════════════

cat("\n=== BUILDING MARKET CLASSIFICATION ===\n")

dt <- readRDS(file.path(import_dir, "dome_trades_combined.rds"))

# Unique markets
markets <- dt[, .(
  title       = title[1L],
  n_trades    = .N,
  total_vol   = sum(dollar_volume, na.rm = TRUE),
  first_trade = min(trade_date),
  last_trade  = max(trade_date)
), by = market_slug]
cat(nrow(markets), "unique markets\n")

# Regex classification from titles
markets[, category := fcase(
  grepl("earning|EPS|revenue|beat.*quarter|miss.*quarter|quarterly",
        title, ignore.case = TRUE),                          "earnings",
  grepl("up or down after|stock price|share price|close above|close below",
        title, ignore.case = TRUE),                          "corporate_event",
  grepl("president|election|senat|congress|governor|democrat|republican|trump|biden|poll",
        title, ignore.case = TRUE),                          "politics",
  grepl("GDP|inflation|CPI|fed.*rate|unemployment|jobs report|payroll|interest rate|recession",
        title, ignore.case = TRUE),                          "macro",
  default = "other"
)]

cat("\nCategory counts:\n")
print(markets[, .N, by = category][order(-N)])

# Extract ticker symbols from parenthetical like "(NVDA)" or "(MOG.A)"
markets[, ticker := fifelse(
  grepl("\\([A-Z]{1,5}(\\.[A-Z])?\\)", title),
  sub(".*\\(([A-Z]{1,5}(\\.[A-Z])?)\\).*", "\\1", title),
  NA_character_
)]

cat("\nTickers extracted:", sum(!is.na(markets$ticker)), "of", nrow(markets), "markets\n")
cat("Unique tickers:", uniqueN(na.omit(markets$ticker)), "\n")

# Cross-reference: closed_earnings_markets.csv
earnings_ref_path <- file.path(import_dir, "closed_earnings_markets.csv")
if (file.exists(earnings_ref_path)) {
  earnings_ref <- fread(earnings_ref_path)
  setnames(earnings_ref, tolower(names(earnings_ref)))
  if ("market_slug" %in% names(earnings_ref)) {
    markets[earnings_ref, on = "market_slug",
            `:=`(ref_tags = i.tags, ref_volume = i.volume_total)]
    cat("Matched to closed_earnings_markets:", sum(!is.na(markets$ref_volume)), "markets\n")
  }
}

# Cross-reference: market_slugs_masterlist.xlsx
master_path <- file.path(import_dir, "market_slugs_masterlist.xlsx")
if (file.exists(master_path)) {
  master <- as.data.table(read_xlsx(master_path))
  setnames(master, tolower(names(master)))
  if ("market_slug" %in% names(master)) {
    override_cols <- intersect(names(master), c("category", "ticker", "tags"))
    if (length(override_cols) > 0) {
      for (col in override_cols) {
        ocol <- paste0("master_", col)
        markets[master, on = "market_slug", (ocol) := get(paste0("i.", col))]
      }
    }
    markets[master, on = "market_slug", master_matched := TRUE]
    cat("Matched to masterlist:", sum(markets$master_matched == TRUE, na.rm = TRUE), "markets\n")
  }
}

cat("\n=== CLASSIFICATION SUMMARY ===\n")
print(markets[, .(n = .N, has_ticker = sum(!is.na(ticker)),
                   total_vol = sum(total_vol)), by = category][order(-n)])

saveRDS(markets, file.path(build_dir, "market_classification.rds"))
cat("Saved market_classification.rds\n")

# ═══════════════════════════════════════════════════════════════════════════
# 3. BUILD HOURLY MARKET PROBABILITIES (from 390_exploration/01_hourly_probabilities.R)
# ═══════════════════════════════════════════════════════════════════════════

cat("\n=== BUILDING HOURLY PROBABILITIES ===\n")

# Implied probability per trade
dt[, implied_prob := fifelse(bid_type == "yes", price, 1 - price)]

# Convert timestamps to ET and derive hour bins
dt[, ts_et := as.POSIXct(format(block_timestamp, tz = "America/New_York"),
                          tz = "America/New_York")]
dt[, trade_date := as.Date(ts_et)]
dt[, trade_hour := as.integer(format(ts_et, "%H"))]

# Bin to market-hour bars (9:30=9, 10:30=10, ..., 15:30=15)
dt[, hour_bin := fcase(
  trade_hour < 10,  9L,
  trade_hour >= 15, 15L,
  default = trade_hour
)]

# Create hour_timestamp for joining
dt[, hour_ts := as.POSIXct(paste(trade_date, sprintf("%02d:30:00", hour_bin)),
                            format = "%Y-%m-%d %H:%M:%S", tz = "America/New_York")]

# Hourly aggregation per market
setorder(dt, market_slug, block_timestamp)

hourly <- dt[, .(
  prob_last     = implied_prob[.N],
  prob_vwap     = sum(implied_prob * dollar_volume) / sum(dollar_volume),
  prob_median   = median(implied_prob),
  hourly_volume = sum(dollar_volume),
  n_trades      = .N,
  n_unique_wallets = uniqueN(maker_address),
  pct_buy       = mean(side == "BUY")
), by = .(market_slug, trade_date, hour_bin, hour_ts)]

cat("Hourly obs from trades:", format(nrow(hourly), big.mark = ","), "\n")

# Fill forward within market hours on trading days
date_range <- hourly[, .(min_d = min(trade_date), max_d = max(trade_date)),
                     by = market_slug]
market_hours <- c(9L, 10L, 11L, 12L, 13L, 14L, 15L)

grid <- date_range[, .(trade_date = seq.Date(min_d, max_d, by = "day")),
                   by = market_slug]
grid <- grid[!weekdays(trade_date) %in% c("Saturday", "Sunday")]
grid <- grid[, .(hour_bin = market_hours), by = .(market_slug, trade_date)]
grid[, hour_ts := as.POSIXct(paste(trade_date, sprintf("%02d:30:00", hour_bin)),
                              format = "%Y-%m-%d %H:%M:%S", tz = "America/New_York")]

hourly <- merge(grid, hourly, by = c("market_slug", "trade_date", "hour_bin", "hour_ts"),
                all.x = TRUE)
setorder(hourly, market_slug, trade_date, hour_bin)

# Mark traded hours, then fill forward
hourly[, traded := !is.na(prob_last)]
hourly[, prob_last   := nafill(prob_last,   type = "locf"), by = market_slug]
hourly[, prob_vwap   := nafill(prob_vwap,   type = "locf"), by = market_slug]
hourly[, prob_median := nafill(prob_median, type = "locf"), by = market_slug]
setnafill(hourly, fill = 0,
          cols = c("hourly_volume", "n_trades", "n_unique_wallets", "pct_buy"))

cat("Hourly obs after fill:", format(nrow(hourly), big.mark = ","), "\n")
cat("Pct carried forward:", round(mean(!hourly$traded) * 100, 1), "%\n")

# PM return: first difference in probability space
hourly[, pm_return := prob_last - shift(prob_last, 1L), by = market_slug]

cat("\nMarkets:", uniqueN(hourly$market_slug), "\n")
cat("Date range:", as.character(range(hourly$trade_date)), "\n")

saveRDS(hourly, file.path(build_dir, "hourly_market_probabilities.rds"))
cat("Saved hourly_market_probabilities.rds\n")

# ═══════════════════════════════════════════════════════════════════════════

cat("\n=== IMPORT COMPLETE ===\n")
cat("import/:", paste(list.files(import_dir), collapse = ", "), "\n")
cat("build/:", paste(list.files(build_dir), collapse = ", "), "\n")
