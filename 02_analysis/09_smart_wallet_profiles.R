# 09_smart_wallet_profiles.R — Deep Dive on Smart Money Wallets via Dome API
# Input:  analysis/wallet_results.rds (22 smart wallet addresses)
# Output: analysis/smart_wallet_profiles.rds
#
# Profiles the 22 smart wallets identified in 07_wallet_analysis.R across
# ALL Polymarket activity (not just earnings) to test whether their accuracy
# generalizes beyond earnings beat/miss markets.

library(data.table)
library(httr2)
library(jsonlite)
library(ggplot2)

data_root    <- Sys.getenv("DATA_DIR", file.path(getwd(), "data"))
analysis_dir <- file.path(data_root, "analysis")

cat("=== SMART WALLET DEEP DIVE VIA DOME API ===\n\n")

# ═══════════════════════════════════════════════════════════════════════════
# 0. DOME API HELPER (replicating from 390_exploration/00_import/02_import.R)
# ═══════════════════════════════════════════════════════════════════════════

query_dome <- function(endpoint, params = list(), verbose = FALSE) {
  api_key <- Sys.getenv("dome_api_key")
  if (api_key == "") stop("dome_api_key not found. Set it in your .Renviron file")

  base_url <- "https://api.domeapi.io/v1"

  req <- request(paste0(base_url, endpoint)) |>
    req_headers(
      "Authorization" = paste("Bearer", api_key),
      "Accept" = "application/json"
    )

  if (length(params) > 0) {
    req <- req |> req_url_query(!!!params)
  }

  if (verbose) cat("Request URL:", req$url, "\n")

  tryCatch({
    resp <- req |> req_perform()
    resp |> resp_body_json()
  }, error = function(e) {
    cat("Error in API request:", e$message, "\n")
    return(NULL)
  })
}

# ═══════════════════════════════════════════════════════════════════════════
# 1. LOAD SMART WALLET ADDRESSES
# ═══════════════════════════════════════════════════════════════════════════

results <- readRDS(file.path(analysis_dir, "wallet_results.rds"))
smart   <- results$smart_wallets
addresses <- smart$maker_address

cat(sprintf("Loaded %d smart wallet addresses\n", length(addresses)))
cat(sprintf("Earnings hit rate range: %.0f%% - %.0f%%\n",
            min(smart$hit_rate) * 100, max(smart$hit_rate) * 100))

# ═══════════════════════════════════════════════════════════════════════════
# 2. PULL WALLET PROFILES + METRICS
# ═══════════════════════════════════════════════════════════════════════════

cat("\n--- Fetching wallet profiles ---\n")

profiles_list <- vector("list", length(addresses))

for (i in seq_along(addresses)) {
  addr <- addresses[i]
  cat(sprintf("[%d/%d] %s...\n", i, length(addresses), substr(addr, 1, 10)))

  resp <- query_dome(
    endpoint = "/polymarket/wallet",
    params = list(eoa = addr, with_metrics = "true")
  )

  if (!is.null(resp)) {
    wm <- resp$wallet_metrics  # metrics are nested
    profiles_list[[i]] <- data.table(
      maker_address  = addr,
      handle         = resp$handle %||% NA_character_,
      pseudonym      = resp$pseudonym %||% NA_character_,
      image          = resp$image %||% NA_character_,
      total_volume   = as.numeric(wm$total_volume %||% NA_real_),
      total_trades   = as.integer(wm$total_trades %||% NA_integer_),
      total_markets  = as.integer(wm$total_markets %||% NA_integer_)
    )
  } else {
    profiles_list[[i]] <- data.table(
      maker_address = addr, handle = NA_character_, pseudonym = NA_character_,
      image = NA_character_, total_volume = NA_real_,
      total_trades = NA_integer_, total_markets = NA_integer_
    )
  }

  Sys.sleep(0.5)
}

profiles <- rbindlist(profiles_list)

# Merge with earnings-only stats
profiles <- merge(profiles, smart[, .(maker_address, hit_rate, n_markets, total_volume)],
                  by = "maker_address", suffixes = c("_all", "_earnings"))

cat(sprintf("\nProfiles retrieved: %d\n", nrow(profiles)))
cat(sprintf("Named accounts (have handle): %d\n", sum(!is.na(profiles$handle))))
cat(sprintf("Anonymous addresses: %d\n", sum(is.na(profiles$handle))))

# ═══════════════════════════════════════════════════════════════════════════
# 3. PULL PnL HISTORY
# ═══════════════════════════════════════════════════════════════════════════

cat("\n--- Fetching PnL histories ---\n")

pnl_cumulative <- vector("list", length(addresses))
pnl_monthly    <- vector("list", length(addresses))

for (i in seq_along(addresses)) {
  addr <- addresses[i]
  cat(sprintf("[%d/%d] PnL for %s...\n", i, length(addresses), substr(addr, 1, 10)))

  # Cumulative (all-time)
  resp_all <- query_dome(
    endpoint = paste0("/polymarket/wallet/pnl/", addr),
    params = list(granularity = "all")
  )

  if (!is.null(resp_all) && length(resp_all$pnl_over_time) > 0) {
    # Last entry in pnl_over_time has the cumulative total
    last_entry <- resp_all$pnl_over_time[[length(resp_all$pnl_over_time)]]
    pnl_cumulative[[i]] <- data.table(
      maker_address = addr,
      cumulative_pnl = as.numeric(last_entry$pnl_to_date %||% NA_real_)
    )
  } else {
    pnl_cumulative[[i]] <- data.table(maker_address = addr, cumulative_pnl = NA_real_)
  }

  Sys.sleep(0.5)

  # Monthly time series
  resp_mo <- query_dome(
    endpoint = paste0("/polymarket/wallet/pnl/", addr),
    params = list(granularity = "month")
  )

  if (!is.null(resp_mo) && length(resp_mo$pnl_over_time) > 0) {
    monthly <- rbindlist(lapply(resp_mo$pnl_over_time, function(x) {
      data.table(
        date = as.POSIXct(as.numeric(x$timestamp), origin = "1970-01-01", tz = "UTC"),
        pnl_to_date = as.numeric(x$pnl_to_date %||% 0)
      )
    }))
    monthly[, maker_address := addr]
    pnl_monthly[[i]] <- monthly
  }

  Sys.sleep(0.5)
}

pnl_cum <- rbindlist(pnl_cumulative)
pnl_mo  <- rbindlist(pnl_monthly, fill = TRUE)

# Merge cumulative PnL into profiles
profiles <- merge(profiles, pnl_cum, by = "maker_address", all.x = TRUE)

cat(sprintf("\nPnL data retrieved for %d wallets\n", sum(!is.na(pnl_cum$cumulative_pnl))))
cat(sprintf("Positive PnL: %d  |  Negative PnL: %d\n",
            sum(profiles$cumulative_pnl > 0, na.rm = TRUE),
            sum(profiles$cumulative_pnl < 0, na.rm = TRUE)))

# ═══════════════════════════════════════════════════════════════════════════
# 4. PULL RECENT TRADE HISTORY (ALL MARKETS)
# ═══════════════════════════════════════════════════════════════════════════

cat("\n--- Fetching trade histories ---\n")

all_orders <- vector("list", length(addresses))

for (i in seq_along(addresses)) {
  addr <- addresses[i]
  cat(sprintf("[%d/%d] Orders for %s...\n", i, length(addresses), substr(addr, 1, 10)))

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
        timestamp     = as.numeric(o$timestamp %||% NA_real_)
      )
    }), fill = TRUE)

    orders_acc[[page]] <- orders_batch
    page <- page + 1

    # Check pagination
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
# 4b. CLASSIFY EARNINGS vs NON-EARNINGS MARKETS
# ═══════════════════════════════════════════════════════════════════════════

# Earnings slugs contain patterns like "earnings", "eps", "revenue", "quarterly"
earnings_pattern <- "earnings|\\beps\\b|revenue|quarterly-earnings|nongaap"
orders_dt[, is_earnings := grepl(earnings_pattern, market_slug, ignore.case = TRUE)]

cat(sprintf("\nMarket classification:\n"))
cat(sprintf("  Earnings markets: %s orders (%d unique slugs)\n",
            format(sum(orders_dt$is_earnings), big.mark = ","),
            uniqueN(orders_dt[is_earnings == TRUE]$market_slug)))
cat(sprintf("  Non-earnings markets: %s orders (%d unique slugs)\n",
            format(sum(!orders_dt$is_earnings), big.mark = ","),
            uniqueN(orders_dt[is_earnings == FALSE]$market_slug)))

# Per-wallet breakdown
wallet_market_counts <- orders_dt[, .(
  n_earnings_slugs     = uniqueN(market_slug[is_earnings]),
  n_non_earnings_slugs = uniqueN(market_slug[!is_earnings]),
  n_total_orders       = .N
), by = maker_address]

profiles <- merge(profiles, wallet_market_counts, by = "maker_address", all.x = TRUE)

# ═══════════════════════════════════════════════════════════════════════════
# 5. ASSEMBLE OUTPUT
# ═══════════════════════════════════════════════════════════════════════════

cat("\n\n")
cat("═══════════════════════════════════════════════════════════════════\n")
cat("TABLE 1: SMART WALLET PROFILES\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

# Truncate addresses for display
profiles[, addr_short := paste0(substr(maker_address, 1, 6), "...", substr(maker_address, nchar(maker_address) - 3, nchar(maker_address)))]

# Sort by earnings hit rate descending
setorder(profiles, -hit_rate)

cat(sprintf("%-14s %-16s %6s %12s %12s %6s %10s\n",
            "Address", "Handle", "E.Hit%", "Overall PnL", "All Volume", "Mkts", "All Trades"))
cat(paste(rep("-", 82), collapse = ""), "\n")

for (j in seq_len(nrow(profiles))) {
  r <- profiles[j]
  handle_str <- if (!is.na(r$handle)) substr(r$handle, 1, 16) else if (!is.na(r$pseudonym)) substr(r$pseudonym, 1, 16) else "(anon)"
  cat(sprintf("%-14s %-16s %5.0f%% %12s %12s %6s %10s\n",
              r$addr_short,
              handle_str,
              r$hit_rate * 100,
              if (!is.na(r$cumulative_pnl)) paste0("$", format(round(r$cumulative_pnl), big.mark = ",")) else "N/A",
              if (!is.na(r$total_volume_all)) paste0("$", format(round(r$total_volume_all), big.mark = ",")) else "N/A",
              if (!is.na(r$total_markets)) format(r$total_markets, big.mark = ",") else "N/A",
              if (!is.na(r$total_trades)) format(r$total_trades, big.mark = ",") else "N/A"))
}

cat("\n\n")
cat("═══════════════════════════════════════════════════════════════════\n")
cat("TABLE 2: EARNINGS vs NON-EARNINGS PARTICIPATION\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

cat(sprintf("%-14s %6s %8s %10s\n",
            "Address", "E.Hit%", "Earn.Mkts", "Other Mkts"))
cat(paste(rep("-", 42), collapse = ""), "\n")

for (j in seq_len(nrow(profiles))) {
  r <- profiles[j]
  cat(sprintf("%-14s %5.0f%% %9d %10d\n",
              r$addr_short,
              r$hit_rate * 100,
              r$n_earnings_slugs %||% 0L,
              r$n_non_earnings_slugs %||% 0L))
}

# Summary stats
cat(sprintf("\nSummary:\n"))
cat(sprintf("  Mean earnings hit rate: %.1f%%\n", mean(profiles$hit_rate) * 100))
cat(sprintf("  Mean cumulative PnL: $%s\n",
            format(round(mean(profiles$cumulative_pnl, na.rm = TRUE)), big.mark = ",")))
cat(sprintf("  Wallets with positive overall PnL: %d / %d\n",
            sum(profiles$cumulative_pnl > 0, na.rm = TRUE),
            sum(!is.na(profiles$cumulative_pnl))))

# Verification: all-market volume should be >= earnings-only volume
profiles[, volume_check := fifelse(!is.na(total_volume_all) & !is.na(total_volume_earnings),
                                   total_volume_all >= total_volume_earnings, NA)]
cat(sprintf("  Volume sanity check (all >= earnings): %d / %d pass\n",
            sum(profiles$volume_check == TRUE, na.rm = TRUE),
            sum(!is.na(profiles$volume_check))))

# ═══════════════════════════════════════════════════════════════════════════
# 6. PnL TIME SERIES FIGURE (top 5 by volume)
# ═══════════════════════════════════════════════════════════════════════════

if (nrow(pnl_mo) > 0) {
  cat("\n--- Plotting PnL time series ---\n")

  # Pick top 5 wallets by all-market volume
  top5 <- profiles[order(-total_volume_all)][1:min(5, nrow(profiles))]$maker_address

  pnl_plot <- pnl_mo[maker_address %in% top5]

  if (nrow(pnl_plot) > 0) {
    # pnl_to_date is already cumulative from the API
    pnl_plot <- pnl_plot[order(maker_address, date)]
    pnl_plot[, cum_pnl := pnl_to_date]
    pnl_plot[, addr_short := paste0(substr(maker_address, 1, 6), "...",
                                     substr(maker_address, nchar(maker_address) - 3, nchar(maker_address)))]

    g <- ggplot(pnl_plot, aes(x = date, y = cum_pnl, color = addr_short)) +
      geom_line(linewidth = 0.8) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
      scale_y_continuous(labels = scales::dollar_format()) +
      labs(x = NULL, y = "Cumulative PnL",
           title = "Cumulative PnL: Top 5 Smart Wallets by Volume",
           subtitle = "All Polymarket activity (not just earnings)",
           color = "Wallet") +
      theme_minimal(base_size = 12) +
      theme(legend.position = "bottom")

    ggsave(file.path(analysis_dir, "fig_smart_pnl_timeseries.pdf"), g, width = 10, height = 6)
    cat("Saved fig_smart_pnl_timeseries.pdf\n")
  } else {
    cat("No monthly PnL data available for plotting\n")
  }
} else {
  cat("No monthly PnL data retrieved; skipping figure\n")
}

# ═══════════════════════════════════════════════════════════════════════════
# SAVE RESULTS
# ═══════════════════════════════════════════════════════════════════════════

output <- list(
  profiles      = profiles,
  pnl_monthly   = pnl_mo,
  orders        = orders_dt,
  n_wallets     = length(addresses),
  addresses     = addresses
)

saveRDS(output, file.path(analysis_dir, "smart_wallet_profiles.rds"))
cat("\nSaved smart_wallet_profiles.rds\n")
cat("Done.\n")
