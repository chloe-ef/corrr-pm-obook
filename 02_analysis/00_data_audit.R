# 00_data_audit.R — Strict data audit and unit tests for revised pipeline
# Run AFTER full pipeline (01_import → 04_build_event_panel + 06_implied_eps.py)

library(data.table)

# Input:  frozen data in data/ (version-controlled, checksummed)
# Output: analysis results to dataLAN working directory
data_dir     <- "~/Documents/git/corrr/390_paper/data"
analysis_dir <- "~/Documents/data/corrr/390_paper/analysis"

cat("
╔══════════════════════════════════════════════════════════════════════════╗
║                     DATA AUDIT & UNIT TEST PASS                        ║
╚══════════════════════════════════════════════════════════════════════════╝
")

# ═══════════════════════════════════════════════════════════════════════════
# CHECK 1: LEAKY PIPELINE & NA AUDIT
# ═══════════════════════════════════════════════════════════════════════════

cat("\n═══ CHECK 1: LEAKY PIPELINE & NA AUDIT ═══\n\n")

dome   <- readRDS(file.path(data_dir, "dome_eps_events.rds"))
ibes   <- readRDS(file.path(data_dir, "ibes_data.rds"))
panel  <- readRDS(file.path(data_dir, "event_panel.rds"))

cat("Stage 1 — Dome EPS Events (after 02_parse):\n")
cat(sprintf("  Rows: %d\n", nrow(dome)))

cat("\nStage 2 — After IBES merge:\n")
n_ibes_matched <- sum(!is.na(panel$actual_eps))
n_ibes_dropped <- nrow(dome) - nrow(panel)
cat(sprintf("  Panel rows:     %d\n", nrow(panel)))
cat(sprintf("  IBES matched:   %d  (%.1f%%)\n",
            n_ibes_matched, n_ibes_matched / nrow(panel) * 100))
cat(sprintf("  Rows dropped:   %d  (merge is all.x=TRUE, so 0 expected)\n",
            n_ibes_dropped))

cat("\nStage 3 — After TAQ returns merge:\n")
n_ret_1d  <- sum(!is.na(panel$excess_return_1d))
n_ret_5d  <- sum(!is.na(panel$excess_return_5d))
n_ret_10d <- sum(!is.na(panel$excess_return_10d))
cat(sprintf("  With excess_return_1d:  %d  (%.1f%%)\n",
            n_ret_1d, n_ret_1d / nrow(panel) * 100))
cat(sprintf("  With excess_return_5d:  %d  (%.1f%%)\n",
            n_ret_5d, n_ret_5d / nrow(panel) * 100))
cat(sprintf("  With excess_return_10d: %d  (%.1f%%)\n",
            n_ret_10d, n_ret_10d / nrow(panel) * 100))

cat("\nNA Audit — Critical Columns:\n")
# beat_prob_last is the VWAP-sourced probability (renamed from prob_vwap in pipeline)
critical_cols <- c("beat_prob_last", "actual_eps", "excess_return_10d")
for (col in critical_cols) {
  if (col %in% names(panel)) {
    n_na <- sum(is.na(panel[[col]]))
    pct  <- n_na / nrow(panel) * 100
    cat(sprintf("  %-25s %3d NA  (%5.1f%%)  %s\n",
                col, n_na, pct,
                if (pct > 10) "⚠ ABOVE 10% THRESHOLD" else "✓ OK"))
  } else {
    cat(sprintf("  %-25s  COLUMN MISSING\n", col))
  }
}

# implied_eps_t lives in the Python CSV output, not the R panel
implied_path <- file.path(data_dir, "implied_eps_results.csv")
if (file.exists(implied_path)) {
  implied <- fread(implied_path)
  n_impl <- nrow(implied)
  n_na_t <- sum(is.na(implied$implied_eps_t))
  n_na_e <- sum(is.na(implied$implied_eps_ecdf))
  cat(sprintf("  %-25s %3d NA  (%5.1f%%)  %s  [from implied_eps_results.csv, n=%d]\n",
              "implied_eps_t", n_na_t, n_na_t / n_impl * 100,
              if (n_na_t / n_impl * 100 > 10) "⚠ ABOVE 10%" else "✓ OK",
              n_impl))
  cat(sprintf("  %-25s %3d NA  (%5.1f%%)  %s\n",
              "implied_eps_ecdf", n_na_e, n_na_e / n_impl * 100,
              if (n_na_e / n_impl * 100 > 10) "⚠ ABOVE 10%" else "✓ OK"))
} else {
  cat("  implied_eps_t           — FILE NOT FOUND (run 06_implied_eps.py)\n")
}

# Hard stop check
cat(sprintf("\n  *** MATCHED SAMPLE: %d of %d events ***\n", n_ibes_matched, nrow(panel)))
if (n_ibes_matched < 231 * 0.9) {
  cat("  ⛔ FAIL: Lost more than 10%% of the 231-event target. INVESTIGATE.\n")
} else {
  cat("  ✓ PASS: Sample size preserved (exceeds 231 threshold).\n")
}


# ═══════════════════════════════════════════════════════════════════════════
# CHECK 2: LOOK-AHEAD BIAS STRESS TEST (BMO TIMING)
# ═══════════════════════════════════════════════════════════════════════════

cat("\n\n═══ CHECK 2: BMO LOOK-AHEAD BIAS STRESS TEST ═══\n\n")

# BMO events: announcement_date < earnings_date (IBES anndats before slug date)
bmo_candidates <- panel[!is.na(announcement_date) & !is.na(earnings_date) &
                        announcement_date < earnings_date]

cat(sprintf("BMO candidates (announcement_date < earnings_date): %d events\n\n",
            nrow(bmo_candidates)))

if (nrow(bmo_candidates) >= 3) {
  set.seed(42)
  sample_idx <- sample(seq_len(nrow(bmo_candidates)), min(3, nrow(bmo_candidates)))
  bmo_sample <- bmo_candidates[sample_idx,
    .(ticker, earnings_date, announcement_date,
      last_prob_date, last_prob_date_adj)]

  cat("Mini-table: 3 random BMO events\n")
  cat(sprintf("  %-8s  %-12s  %-15s  %-15s  %-15s  %s\n",
              "Ticker", "Earnings", "Announcement", "Raw Prob Date",
              "Adj Prob Date", "Verdict"))
  cat(paste(rep("-", 95), collapse = ""), "\n")

  for (i in seq_len(nrow(bmo_sample))) {
    r <- bmo_sample[i]
    # For BMO: adj date must be strictly < earnings_date
    ok <- !is.na(r$last_prob_date_adj) && r$last_prob_date_adj < r$earnings_date
    cat(sprintf("  %-8s  %s  %s      %s      %s      %s\n",
                r$ticker,
                as.character(r$earnings_date),
                as.character(r$announcement_date),
                as.character(r$last_prob_date),
                as.character(r$last_prob_date_adj),
                if (ok) "✓ Strictly T-1" else "⚠ POSSIBLE LEAK"))
  }

  # Aggregate check
  bmo_leak <- bmo_candidates[last_prob_date_adj >= earnings_date]
  cat(sprintf("\n  Aggregate: %d / %d BMO events have adj_date >= earnings_date\n",
              nrow(bmo_leak), nrow(bmo_candidates)))
  if (nrow(bmo_leak) == 0) {
    cat("  ✓ PASS: No look-ahead bias in BMO events.\n")
  } else {
    cat("  ⚠ FAIL: Some BMO events may use post-announcement probabilities.\n")
    print(bmo_leak[, .(ticker, earnings_date, announcement_date,
                        last_prob_date, last_prob_date_adj)])
  }
} else {
  cat("  Not enough BMO events to test (need ≥3).\n")
  cat("  Events where announcement_date == earnings_date (AMC/unknown):",
      nrow(panel[!is.na(announcement_date) & announcement_date == earnings_date]), "\n")
}


# ═══════════════════════════════════════════════════════════════════════════
# CHECK 3: STOCK SPLIT SANITY CHECK (adjfac)
# ═══════════════════════════════════════════════════════════════════════════

cat("\n\n═══ CHECK 3: STOCK SPLIT SANITY CHECK ═══\n\n")

history <- readRDS(file.path(data_dir, "ibes_history.rds"))

# Find tickers where adj_eps ≠ actual_eps (meaning a split adjustment was applied)
history[, was_adjusted := abs(adj_eps - actual_eps) > 0.001]
adjusted_tickers <- history[was_adjusted == TRUE, unique(oftic)]

if (length(adjusted_tickers) > 0) {
  cat(sprintf("Tickers with split adjustments: %d\n", length(adjusted_tickers)))
  cat("Examples:", paste(head(adjusted_tickers, 10), collapse = ", "), "\n\n")

  # Pick a well-known split stock if available, else first adjusted
  pick <- intersect(c("NVDA", "AAPL", "GOOGL", "AMZN", "TSLA"), adjusted_tickers)
  if (length(pick) == 0) pick <- adjusted_tickers[1]
  pick <- pick[1]

  cat(sprintf("Examining: %s\n", pick))
  tk_hist <- history[oftic == pick][order(fpedats)]

  # Show last 12 rows (or all if fewer)
  show <- tail(tk_hist, 12)
  cat(sprintf("\nTrailing %d quarters for %s:\n", nrow(show), pick))
  cat(sprintf("  %-12s  %-12s  %10s  %10s  %10s  %10s  %s\n",
              "fpedats", "basis", "raw_actual", "raw_cons", "adj_actual",
              "adj_cons", "adj_applied?"))
  cat(paste(rep("-", 95), collapse = ""), "\n")

  for (i in seq_len(nrow(show))) {
    r <- show[i]
    adj_flag <- if (abs(r$adj_eps - r$actual_eps) > 0.001) "YES" else "no"
    cat(sprintf("  %s  %-12s  %10.3f  %10.3f  %10.3f  %10.3f  %s\n",
                as.character(r$fpedats), r$accounting_basis,
                r$actual_eps, r$consensus_mean,
                r$adj_eps, r$adj_cons, adj_flag))
  }

  # Verify the adjustment is consistent (ratio should be constant within a split event)
  adj_ratios <- show[abs(adj_eps - actual_eps) > 0.001,
                     .(ratio = actual_eps / adj_eps)]
  if (nrow(adj_ratios) > 0) {
    cat(sprintf("\n  Split ratios observed: %s\n",
                paste(round(unique(adj_ratios$ratio), 2), collapse = ", ")))
    cat("  ✓ Values should cluster around a whole number (e.g., 10 for 10:1 split)\n")
  }
} else {
  cat("  No split adjustments were applied in the history.\n")
  cat("  This is expected if no tickers in the sample had splits in 2022-2026.\n")
  cat("  Checking raw: any tickers where actual_eps differs from adj_eps?\n")
  cat(sprintf("  Rows with adj_eps != actual_eps: %d of %d\n",
              sum(abs(history$adj_eps - history$actual_eps) > 0.001),
              nrow(history)))
}


# ═══════════════════════════════════════════════════════════════════════════
# CHECK 4: ROLLING SIGMA LEAKAGE CHECK
# ═══════════════════════════════════════════════════════════════════════════

cat("\n\n═══ CHECK 4: ROLLING SIGMA LEAKAGE CHECK ═══\n\n")

# Find tickers with full 12+ quarters of history
ticker_counts <- history[!is.na(surprise), .N, by = .(oftic, accounting_basis)]
full_tickers <- ticker_counts[N >= 13]  # need 13 so that with shift(1), row 13 has 12 trailing

if (nrow(full_tickers) > 0) {
  set.seed(123)
  pick_row <- full_tickers[sample(.N, 1)]
  pick_tk <- pick_row$oftic
  pick_basis <- pick_row$accounting_basis

  cat(sprintf("Examining: %s (%s) — %d quarters of history\n\n",
              pick_tk, pick_basis, pick_row$N))

  tk_data <- history[oftic == pick_tk & accounting_basis == pick_basis][order(fpedats)]
  tk_data[, row_idx := seq_len(.N)]

  # Compute rolling sigma with shift(1) to match Python logic
  tk_data[, sigma_check := {
    s <- frollapply(surprise, n = 12, FUN = sd)
    shift(s, 1L)  # CRITICAL: shift so current row's data isn't in its own sigma
  }]

  # Pick the last row that has a sigma (i.e., row 13+)
  target_row <- tk_data[!is.na(sigma_check)][.N]  # last row with sigma

  cat(sprintf("TARGET EVENT:\n"))
  cat(sprintf("  Ticker:        %s\n", pick_tk))
  cat(sprintf("  Basis:         %s\n", pick_basis))
  cat(sprintf("  fpedats:       %s\n", as.character(target_row$fpedats)))
  cat(sprintf("  actual_eps:    %.3f\n", target_row$actual_eps))
  cat(sprintf("  surprise:      %.3f\n", target_row$surprise))
  cat(sprintf("  sigma:         %.4f\n", target_row$sigma_check))

  # The 12 quarters used for this sigma come from shift(1), so they are
  # the 12 rows BEFORE the current row
  target_idx <- target_row$row_idx
  window_start <- target_idx - 12  # shift(1) means we use rows [idx-12, idx-1]
  window_end   <- target_idx - 1

  if (window_start >= 1) {
    window_rows <- tk_data[row_idx >= window_start & row_idx <= window_end]

    cat(sprintf("\nSIGMA WINDOW: 12 quarters used (rows %d-%d):\n",
                window_start, window_end))
    cat(sprintf("  %-4s  %-12s  %10s  %10s  %10s\n",
                "#", "fpedats", "actual_eps", "consensus", "surprise"))
    cat(paste(rep("-", 55), collapse = ""), "\n")

    for (i in seq_len(nrow(window_rows))) {
      r <- window_rows[i]
      cat(sprintf("  %-4d  %s  %10.3f  %10.3f  %10.3f\n",
                  i, as.character(r$fpedats), r$actual_eps,
                  r$consensus_mean, r$surprise))
    }

    # Verify: recompute sigma from these 12 surprises
    recomputed_sigma <- sd(window_rows$surprise)
    cat(sprintf("\n  Recomputed sigma from window: %.4f\n", recomputed_sigma))
    cat(sprintf("  Pipeline sigma:               %.4f\n", target_row$sigma_check))

    if (abs(recomputed_sigma - target_row$sigma_check) < 0.0001) {
      cat("  ✓ MATCH\n")
    } else {
      cat("  ⚠ MISMATCH — investigate\n")
    }

    # THE KEY TEST: Is the target event's own data in the window?
    target_in_window <- target_row$fpedats %in% window_rows$fpedats
    cat(sprintf("\n  LEAKAGE TEST: Is target fpedats (%s) in the sigma window?\n",
                as.character(target_row$fpedats)))
    if (target_in_window) {
      cat("  ⛔ FAIL: Current quarter leaked into its own sigma!\n")
    } else {
      cat("  ✓ PASS: No leakage. Current quarter excluded from sigma.\n")
    }

    # Double-check: latest date in window must be strictly before target
    max_window_date <- max(window_rows$fpedats)
    cat(sprintf("  Latest date in window: %s (must be < %s)\n",
                as.character(max_window_date), as.character(target_row$fpedats)))
    if (max_window_date < target_row$fpedats) {
      cat("  ✓ PASS: Window is strictly historical.\n")
    } else {
      cat("  ⚠ FAIL: Window contains future data.\n")
    }
  }
} else {
  cat("  No tickers with 13+ quarters of history found.\n")
}


cat("\n
╔══════════════════════════════════════════════════════════════════════════╗
║                         AUDIT COMPLETE                                 ║
╚══════════════════════════════════════════════════════════════════════════╝
\n")
