# Kalshi Mention Markets: Continuation Project (Next Quarter)

## Background
Cut from the current paper ("Crowd Wisdom in Corporate Earnings") to keep scope manageable. The data and research questions are worth a standalone follow-up.

## Data Available
- **File:** `/Users/chloe_1.0/Documents/data/corrr/390_paper/import/DOME_kalshi_chloe_202603012021.csv`
- **184,068 trades** across **618 unique markets**, **26 companies**
- **Date range:** Oct 11, 2025 to Feb 25, 2026
- **Fields:** trade_id, market_ticker, count, yes_price, no_price, yes_price_dollars, no_price_dollars, taker_side, created_time, indexed_at
- **Market structure:** `KXEARNINGSMENTIONAAPL-25OCT30-WEAR` = "Will Apple say 'wearable' during the Oct 30 2025 earnings call?"
- **Caveat from Dome founders:** Data quality may not be fully reliable. Validate before centering analysis on it.

## Companies Covered
AAPL, AMZN, COST, CRM, DIS, FDX, GOOGL, INTC, JPM, KO, LYFT, MA, META, MSFT, NFLX, NKE, NVDA, PLTR, PYPL, SNAP, SPOT, TSLA, UBER, V, WFC, WMT

## Research Questions

### 1. Stat Arb via Base Rate Mispricing
**Source:** Hedge fund manager conversation (March 2026). He compared Kalshi mention markets to 1970s stock markets with easy stat arb opportunities.

**Hypothesis:** Markets where the historical base rate of a word appearing in earnings calls is calculable (e.g., "AI" appears in 95% of NVDA calls) should be priced near that base rate. Systematic deviations from base rates represent mispricing.

**Analysis:**
- Parse company, earnings date, and keyword from each market_ticker
- Source historical earnings call transcripts (e.g., from Seeking Alpha, FactSet, or SEC EDGAR)
- Compute historical frequency of each keyword in that company's past N earnings calls
- Compare base rate to Kalshi market-implied probability
- Flag mispricings where |base_rate - market_prob| > threshold
- Compute theoretical P&L from a base-rate strategy
- Control for market liquidity (volume, bid-ask spread)

### 2. Cross-Market Information Flow
**Connection to main paper:** These mention markets trade alongside the EPS beat/miss markets during the same earnings calls. Questions:
- Does order flow in mention markets predict EPS beat/miss outcomes?
- Do mention market prices move in response to the same information as EPS markets?
- Is there cross-market arbitrage between Kalshi mentions and Polymarket EPS?

### 3. Market Microstructure of Thin Markets
**Source:** Hedge fund manager's observation that these markets resemble 1970s stock markets.

**Analysis:**
- Characterize liquidity: volume distribution, trade frequency, bid-ask spreads (if available)
- Identify markets with theoretical 50/50 payoff that are mispriced
- Measure price discovery speed: how quickly do prices converge to fair value?
- Compare microstructure metrics (Kyle's lambda, Amihud illiquidity) to early equity markets

## Data Pipeline (To Build)
1. Parse market_ticker into (company, earnings_date, keyword) tuples
2. Pull historical earnings call transcripts for each company
3. Compute keyword base rates per company
4. Compute end-of-market implied probability from Kalshi trade data
5. Merge base rates with market prices
6. Run mispricing analysis + P&L simulation

## Key Papers to Review
- Kyle (1985) — market microstructure, informed trading in thin markets
- Glosten & Milgrom (1985) — bid-ask spread as information asymmetry measure
- Roll (1984) — bid-ask bounce effect
- Wolfers & Zitzewitz (2004) — prediction market accuracy
- Page (2012) — diversity prediction theorem

## Notes
- Dome founders warned data quality is unreliable. First step next quarter should be validation: compare Dome's Kalshi data against Kalshi's public API to check for missing trades or price discrepancies.
- Consider requesting order book snapshots from Dome/Kalshi for true bid-ask analysis.
- The hedge fund manager also noted it's illegal for Americans to trade on Polymarket — worth exploring whether Kalshi (which is CFTC-regulated and US-legal) has different trader composition and therefore different information content.
