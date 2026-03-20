# 05_pull_oneshot_wallets.R — Pull Dome API trade history for one-shot wallets
# Input:  import/dome_trades_combined.rds, build/dome_eps_events.rds,
#         build/event_panel.rds
# Output: data/oneshot_wallet_profiles.rds (frozen in repo)
#
# Identifies the "perfect + big-bet" one-shot wallets (100% correct,
# 1-3 earnings markets, $500+ volume) and pulls their FULL Polymarket
# trade histories from the Dome API orders endpoint.
# This is a one-time pull; analysis reads from the frozen output.
#
# Note: The /polymarket/wallet profile endpoint returns 404 as of 2026-03,
# so we use /polymarket/orders which still works and gives us what we need
# for activity classification (all markets traded, not just earnings).

library(data.table)
library(httr2)
library(jsonlite)

import_dir <- "~/Documents/data/corrr/390_paper/import"
build_dir  <- "~/Documents/data/corrr/390_paper/build"
frozen_dir <- "~/Documents/git/corrr/390_paper/data"

cat("=== PULL ONE-SHOT WALLET TRADE HISTORIES FROM DOME API ===\n\n")

# ═══════════════════════════════════════════════════════════════════════════
# 0. DOME API HELPER
# ═══════════════════════════════════════════════════════════════════════════

query_dome <- function(endpoint, params = list()) {
  api_key <- Sys.getenv("dome_api_key")
  if (api_key == "") stop("dome_api_key not found. Set it in your .Renviron file")

  req <- request(paste0("https://api.domeapi.io/v1", endpoint)) |>
    req_headers("Authorization" = paste("Bearer", api_key),
                "Accept" = "application/json")
  if (length(params) > 0) req <- req |> req_url_query(!!!params)

  tryCatch({
    resp <- req |> req_perform()
    resp |> resp_body_json()
  }, error = function(e) {
    cat("  Error:", e$message, "\n")
    return(NULL)
  })
}

# ═══════════════════════════════════════════════════════════════════════════
# 1. IDENTIFY FLAGGED WALLETS (replicate cohort selection from 10)
# ═══════════════════════════════════════════════════════════════════════════

trades <- readRDS(file.path(import_dir, "dome_trades_combined.rds"))
events <- readRDS(file.path(build_dir, "dome_eps_events.rds"))
panel  <- readRDS(file.path(build_dir, "event_panel.rds"))

earnings_slugs <- events$market_slug
earn_trades <- trades[market_slug %in% earnings_slugs]

outcomes <- panel[!is.na(actual_beat), .(market_slug, actual_beat)]
earn_trades <- merge(earn_trades, outcomes, by = "market_slug", all.x = FALSE)

earn_trades[, bullish_trade := (side == "BUY" & bid_type == "yes") |
                               (side == "SELL" & bid_type == "no")]
earn_trades[, bearish_trade := (side == "SELL" & bid_type == "yes") |
                               (side == "BUY" & bid_type == "no")]

wallet_market <- earn_trades[, .(
  bullish_vol = sum(dollar_volume[bullish_trade], na.rm = TRUE),
  bearish_vol = sum(dollar_volume[bearish_trade], na.rm = TRUE),
  n_trades    = .N,
  actual_beat = actual_beat[1]
), by = .(maker_address, market_slug)]

wallet_market[, net_position := bullish_vol - bearish_vol]
wallet_market[, wallet_called_beat := net_position > 0]
wallet_market[, wallet_correct := wallet_called_beat == actual_beat]
wallet_market[, market_volume := bullish_vol + bearish_vol]

wallets <- wallet_market[, .(
  n_markets      = .N,
  n_correct      = sum(wallet_correct),
  hit_rate       = mean(wallet_correct),
  total_volume   = sum(market_volume),
  n_trades_total = sum(n_trades)
), by = maker_address]

oneshot <- wallets[n_markets <= 3]
perfect <- oneshot[hit_rate == 1.0]
median_vol_all <- median(wallets$total_volume)
big_bet_threshold <- max(500, median_vol_all)
perfect_big <- perfect[total_volume >= big_bet_threshold]

flagged_addrs <- perfect_big$maker_address
cat(sprintf("Flagged wallets to pull: %d\n", length(flagged_addrs)))
cat(sprintf("Big-bet threshold: $%.0f\n\n", big_bet_threshold))

# Free memory
rm(trades, earn_trades)
gc()

# ═══════════════════════════════════════════════════════════════════════════
# 2. PULL FULL TRADE HISTORY VIA ORDERS ENDPOINT
# ═══════════════════════════════════════════════════════════════════════════

cat("--- Fetching trade histories via /polymarket/orders ---\n")

all_orders <- vector("list", length(flagged_addrs))

for (i in seq_along(flagged_addrs)) {
  addr <- flagged_addrs[i]
  if (i %% 25 == 0 || i == 1 || i == length(flagged_addrs)) {
    cat(sprintf("[%d/%d] %s...\n", i, length(flagged_addrs), substr(addr, 1, 10)))
  }

  orders_acc <- list()
  pagination_key <- NULL
  page <- 1

  repeat {
    params <- list(user = addr, limit = 1000)
    if (!is.null(pagination_key)) params$pagination_key <- pagination_key

    resp <- query_dome(endpoint = "/polymarket/orders", params = params)

    if (is.null(resp) || length(resp$orders) == 0) break

    orders_batch <- rbindlist(lapply(resp$orders, function(o) {
      data.table(
        maker_address = addr,
        market_slug   = o$market_slug %||% NA_character_,
        side          = o$side %||% NA_character_,
        price         = as.numeric(o$price %||% NA_real_),
        shares        = as.numeric(o$shares %||% o$shares_normalized %||% NA_real_),
        timestamp     = as.numeric(o$timestamp %||% NA_real_),
        dollar_volume = as.numeric(o$price %||% 0) * as.numeric(o$shares %||% o$shares_normalized %||% 0)
      )
    }), fill = TRUE)

    orders_acc[[page]] <- orders_batch
    page <- page + 1

    new_key <- resp$pagination$pagination_key %||% resp$pagination$next_cursor
    if (is.null(new_key) || identical(new_key, pagination_key)) break
    pagination_key <- new_key

    Sys.sleep(0.5)
  }

  if (length(orders_acc) > 0) {
    all_orders[[i]] <- rbindlist(orders_acc, fill = TRUE)
  }

  Sys.sleep(0.5)
}

orders_dt <- rbindlist(all_orders, fill = TRUE)
cat(sprintf("\nTotal orders retrieved: %s across %d wallets\n",
            format(nrow(orders_dt), big.mark = ","),
            uniqueN(orders_dt$maker_address)))

# ═══════════════════════════════════════════════════════════════════════════
# 3. BUILD ACTIVITY CLASSIFICATION
# ═══════════════════════════════════════════════════════════════════════════

orders_dt[, is_earnings := market_slug %in% earnings_slugs]

wallet_activity <- orders_dt[, .(
  n_total_markets    = uniqueN(market_slug),
  n_earnings_markets = uniqueN(market_slug[is_earnings]),
  n_other_markets    = uniqueN(market_slug[!is_earnings]),
  n_total_orders     = .N,
  n_earnings_orders  = sum(is_earnings),
  n_other_orders     = sum(!is_earnings),
  total_volume       = sum(dollar_volume, na.rm = TRUE),
  first_trade        = min(timestamp, na.rm = TRUE),
  last_trade         = max(timestamp, na.rm = TRUE)
), by = maker_address]

wallet_activity[, wallet_type := fifelse(
  n_other_markets == 0 & n_total_markets <= 3, "fresh_earnings_only",
  fifelse(n_other_markets == 0, "earnings_only",
          "diversified")
)]

# Account for wallets that had no orders returned (e.g., API issues)
missing_addrs <- flagged_addrs[!flagged_addrs %in% wallet_activity$maker_address]
if (length(missing_addrs) > 0) {
  cat(sprintf("WARNING: %d wallets returned no orders from API\n", length(missing_addrs)))
}

# ═══════════════════════════════════════════════════════════════════════════
# 4. SAVE AND FREEZE
# ═══════════════════════════════════════════════════════════════════════════

output <- list(
  flagged_addresses = flagged_addrs,
  orders            = orders_dt,
  wallet_activity   = wallet_activity,
  pull_date         = Sys.time(),
  n_wallets         = length(flagged_addrs),
  big_bet_threshold = big_bet_threshold
)

# Save to build dir
build_path <- file.path(build_dir, "oneshot_wallet_profiles.rds")
saveRDS(output, build_path)
cat(sprintf("\nSaved to build: %s\n", build_path))

# Freeze to data/ in the repo
frozen_path <- file.path(frozen_dir, "oneshot_wallet_profiles.rds")
file.copy(build_path, frozen_path, overwrite = TRUE)
cat(sprintf("Frozen to repo: %s\n", frozen_path))

# Summary
cat("\nActivity classification:\n")
type_tab <- wallet_activity[, .N, by = wallet_type]
for (j in seq_len(nrow(type_tab))) {
  cat(sprintf("  %-25s %d (%.1f%%)\n",
              type_tab$wallet_type[j], type_tab$N[j],
              type_tab$N[j] / nrow(wallet_activity) * 100))
}

div <- wallet_activity[wallet_type == "diversified"]
if (nrow(div) > 0) {
  cat(sprintf("\nDiversified wallets:\n"))
  cat(sprintf("  Mean total markets: %.1f\n", mean(div$n_total_markets)))
  cat(sprintf("  Mean non-earnings markets: %.1f\n", mean(div$n_other_markets)))
}

fresh <- wallet_activity[wallet_type == "fresh_earnings_only"]
if (nrow(fresh) > 0) {
  cat(sprintf("\nFresh earnings-only wallets: %d\n", nrow(fresh)))
  cat(sprintf("  These wallets have NO other Polymarket activity.\n"))
}

cat("\n=== DONE ===\n")
