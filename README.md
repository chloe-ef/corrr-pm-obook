# Minority Report: Contrarian Traders, Prediction Markets, and the Return of Post-Earnings Drift

Does prediction market crowd flow reveal independent information about earnings outcomes beyond analyst consensus?

## Research Question

Each Polymarket earnings beat/miss market has an **EPS target** set by the market maker, independent of analyst consensus. We compare:
- **ΔPM** = `eps_target - actual_eps` (market maker forecast error)
- **ΔAN** = `consensus_mean - actual_eps` (analyst consensus forecast error)

PM crowd trading flow (11.1M trades across buy/sell × yes/no) reveals whether the crowd agrees with the MM line or leans toward analysts — and we measure who turns out right.

## Data

- **338 beat/miss markets** (254 tickers), Sep 2025 through Feb 2026
- **IBES** analyst consensus (EPS for non-GAAP, GPS for GAAP) + actuals via WRDS
- **WRDS TAQ** daily returns (from 5-min bars) for post-earnings excess returns
- **Fama-French** daily 3-factor data from Kenneth French's data library

## Reproducibility: Frozen Data

All processed data files used in the paper are committed in `data/` with SHA-256 checksums (`data/SHA256SUMS`). **Analysis scripts read directly from `data/`.** Re-pulling from WRDS or the Dome API may return different data due to backfills, restatements, or API changes.

### Quick start (analysis only, no credentials needed)

```bash
# Create output directory for figures and results
mkdir -p ~/Documents/data/corrr/390_paper/analysis

# Run analysis scripts — all read from frozen data/ in the repo
Rscript 02_analysis/00_data_audit.R
Rscript 02_analysis/01_descriptive.R
Rscript 02_analysis/02_calibration.R
Rscript 02_analysis/03_disagreement.R
Rscript 02_analysis/04_surprise_decomp.R
Rscript 02_analysis/05_returns_by_correctness.R
pip install -r requirements.txt
python 02_analysis/06_implied_eps.py
Rscript 02_analysis/07_wallet_analysis.R
Rscript 02_analysis/08_ff_alpha.R
Rscript 02_analysis/10_oneshot_wallet_forensics.R

# Compile paper
cd 03_paper && pdflatex main && bibtex main && pdflatex main && pdflatex main
```

### Frozen data files (`data/`)

| File | Description | Source |
|------|-------------|--------|
| `event_panel.rds` | Main analysis panel (338 events, all variables) | `04_build_event_panel.R` |
| `dome_eps_events.rds` | Parsed earnings markets with flow metrics | `02_parse_dome_events.R` |
| `ibes_data.rds` | IBES consensus + actuals matched to events | `03_pull_ibes.R` (WRDS) |
| `ibes_history.rds` | 3-year trailing IBES history for rolling sigma | `03_pull_ibes.R` (WRDS) |
| `equity_daily_returns.rds` | Daily stock returns from TAQ 5-min bars | `03b_pull_taq.R` (WRDS) |
| `index_daily_returns.rds` | SPY daily returns | `03b_pull_taq.R` (WRDS) |
| `ff_daily.rds` | Fama-French 3-factor daily data | `05_pull_ff.R` (Ken French) |
| `market_classification.rds` | Market slug classification | `01_import.R` |
| `hourly_market_probabilities.rds` | Hourly VWAP probabilities | `01_import.R` |
| `implied_eps_results.csv` | Implied EPS output (Methods A + B) | `06_implied_eps.py` |
| `oneshot_wallet_profiles.rds` | Dome API trade histories for 282 flagged wallets | `05_pull_oneshot_wallets.R` |
| `SHA256SUMS` | Checksums to verify file integrity | |

### Verifying data integrity

```bash
cd data && shasum -a 256 -c SHA256SUMS
```

### What is NOT included

- **`dome_trades_combined.rds`** (~186MB raw trade data): Too large for git. Required only for `07_wallet_analysis.R` and `10_oneshot_wallet_forensics.R`. Contact the author for access.
- **Dome API wallet profiles**: `09_smart_wallet_profiles.R` requires a Dome API key. Dome was acquired by Polymarket; API availability is uncertain.

## Directory Structure

```
├── 01_build/          # Build scripts (for re-pulling from source)
├── 02_analysis/       # Analysis scripts (read from data/)
├── 03_paper/          # LaTeX source
├── 04_citations/      # Citation notes and PDFs
├── data/              # Frozen processed data (committed, checksummed)
├── requirements.txt   # Python dependencies
└── README.md
```

Analysis outputs (figures, result RDS files) are written to `~/Documents/data/corrr/390_paper/analysis/`. To change this path, search all `.R`, `.py`, and `.tex` files for `analysis_dir` and update accordingly.

## Prerequisites

- **R 4.x** with packages: `data.table`, `readxl`, `quantmod`, `ggplot2`, `fixest`, `RPostgres`, `DBI`
- **Python 3.9+** with packages listed in `requirements.txt`
- **WRDS credentials** (only if re-pulling from source): `wrds_username`, `wrds_password` environment variables
- **Dome API key** (only for `09_smart_wallet_profiles.R`): `dome_api_key` environment variable

## Pipeline

### Build (`01_build/`) — only needed to re-pull from source
| Script | What it does | Output |
|--------|-------------|--------|
| `01_import.R` | Copies dome_trades_combined.rds + ref CSVs, rebuilds classification + hourly probs | market_classification.rds, hourly_market_probabilities.rds |
| `02_parse_dome_events.R` | Parses EPS targets from slugs, computes crowd flow metrics | dome_eps_events.rds |
| `03_pull_ibes.R` | Pulls IBES consensus + actuals from WRDS | ibes_data.rds, ibes_history.rds |
| `03b_pull_taq.R` | Pulls TAQ 5-min bars from WRDS, aggregates to daily + SPY index | equity_daily_returns.rds, index_daily_returns.rds |
| `04_build_event_panel.R` | Merges all sources into analysis panel | event_panel.rds |
| `05_pull_ff.R` | Downloads daily Fama-French 3-factor data | ff_daily.rds |
| `05_pull_oneshot_wallets.R` | Pulls full trade histories from Dome API for flagged wallets | oneshot_wallet_profiles.rds |

### Analysis (`02_analysis/`)
| Script | What it does |
|--------|-------------|
| `00_data_audit.R` | Sanity checks on panel: missingness, IBES match rate, implied EPS coverage |
| `01_descriptive.R` | MM line vs consensus scatter, beat prob distribution, flow decomposition, summary stats |
| `02_calibration.R` | Calibration bins, flow-based calibration, Brier scores, analyst-implied Brier |
| `03_disagreement.R` | 2x2 crowd x analyst disagreement, conditional accuracy, return regressions, ICs |
| `04_surprise_decomp.R` | Analyst bias regression, who-was-closer, PEAD decomposition, EPS waterfall figure |
| `05_returns_by_correctness.R` | Four-cell returns, trading strategy, drift figure, conviction analysis |
| `06_implied_eps.py` | Implied EPS via t-distribution inversion (Method A) and empirical CDF (Method B) |
| `07_wallet_analysis.R` | Smart wallet identification, logistic regressions, disagreement analysis |
| `08_ff_alpha.R` | Fama-French 3-factor alpha regressions (pooled event-day + calendar-time) |
| `09_smart_wallet_profiles.R` | Dome API wallet profiles, PnL time series, archetype classification |
| `10_oneshot_wallet_forensics.R` | Low-activity wallet forensic analysis for insider trading patterns |

### Paper (`03_paper/`)
Compile with: `pdflatex main && bibtex main && pdflatex main && pdflatex main`

## Key Design Decisions

- Match IBES on `oftic` (exchange ticker), not IBES internal `ticker`
- GAAP markets → GPS measure in IBES; non-GAAP → EPS measure
- Flow imbalance = `(buy_yes + sell_no - buy_no - sell_yes) / total_volume`
- Beat conviction = dollar-weighted avg price on buy-yes trades
- Winsorize returns at 2nd/98th percentile
- One row per market (both GAAP and non-GAAP kept if same ticker/date)
- Equity returns from WRDS TAQ 5-min bars aggregated to daily
- Processed data frozen and committed for reproducibility
