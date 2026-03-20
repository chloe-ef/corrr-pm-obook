# Changelog: Crowd Wisdom in Corporate Earnings

## March 2026 Revision (from February 2026 draft)

### IBES Match Rate: 68% → 99.4%
The February 2026 draft reported a 68% match rate (231/340) because the IBES query date range was restricted to ±90 days around earnings dates. Expanding the query window to three years, as required for the implied EPS analysis, captured an additional 107 events whose fiscal period end dates fell outside the original narrow window. Final sample: 338 matched events across 254 tickers.

### New Sections Added
- **Section 7 (Analyst Walk-Down):** Pooled OLS bias regression, alpha = +1.55 (t = 5.88)
- **Section 8 (Implied EPS):** Two methods (Student's t, ECDF) for converting P(Beat) to dollar EPS
- **Section 9 (Short-Side Alpha):** Short-only strategy returns, +5.50% by day 10 (t = 2.59)

### Pipeline Patches
1. Stock split adjustment via IBES adjfac (15 tickers)
2. Parallel GAAP / Non-GAAP data streams with hard assertions
3. BMO look-ahead bias correction (9 events adjusted to T-1)
4. Penny discretization on implied EPS
5. VWAP probability replaces last-trade price for beat_prob
6. Liquidity filter (vol >= $500, n_trades >= 20)

### Data Fixes
- Deduplicated annual vs. quarterly IBES actuals (8,461 → 6,569 history rows)
- Fixed IBES schema column names (adj, surpsumu)
- Mapped oftic onto sub-tables before build_history()

### Structural Changes
- MM Line vs. Consensus demoted from standalone section to data validation subsection
- Summary statistics table updated to reflect 338-event sample
- 10 new references added to bibliography
