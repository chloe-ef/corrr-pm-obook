"""
06_implied_eps.py — Implied EPS from Prediction Market Probabilities (Rev 4)

Converts P(EPS > line) from the prediction market into a point estimate of
expected EPS using two methods:

  Method A (Parametric): Assume EPS ~ Student's t(mu, sigma, df=4) with fat
    tails. implied_eps = line + sigma * t.ppf(beat_prob, df=4)

  Method B (Non-Parametric): Use empirical CDF of historical surprises.
    Q_{1-p} = empirical (1-p) quantile of trailing surprises.
    implied_eps = line - Q_{1-p}

Both require a rolling estimate of surprise volatility (sigma) using 12
quarters of trailing data with a .shift(1) to avoid look-ahead bias.

Patch 1: Uses split-adjusted EPS (adj_eps, adj_cons) from ibes_history.rds.
Patch 2: Computes sigma separately per accounting_basis (GAAP vs Non-GAAP).
Patch 4: Rounds implied EPS to 2 decimal places (penny discretization).

Input:  build/event_panel.rds (or CSV export), build/ibes_history.rds
Output: analysis/implied_eps_results.csv, analysis/implied_eps_summary.txt
"""

import sys
import os
import warnings
import numpy as np
import pandas as pd
from scipy.stats import t as t_dist

warnings.filterwarnings("ignore", category=FutureWarning)

# ─── Paths ────────────────────────────────────────────────────────────────
DATA_DIR = os.path.expanduser("~/Documents/git/corrr/390_paper/data")
ANALYSIS_DIR = os.path.expanduser("~/Documents/data/corrr/390_paper/analysis")

os.makedirs(ANALYSIS_DIR, exist_ok=True)

# ─── Load data ────────────────────────────────────────────────────────────
try:
    import pyreadr
    panel = pyreadr.read_r(os.path.join(DATA_DIR, "event_panel.rds"))[None]
except ImportError:
    panel = pd.read_csv(os.path.join(DATA_DIR, "event_panel.csv"))

history_path_rds = os.path.join(DATA_DIR, "ibes_history.rds")
history_path_csv = os.path.join(DATA_DIR, "ibes_history.csv")

if os.path.exists(history_path_rds):
    history = pyreadr.read_r(history_path_rds)[None]
elif os.path.exists(history_path_csv):
    history = pd.read_csv(history_path_csv)
else:
    print("ERROR: ibes_history.rds not found. Re-run 03_pull_ibes.R with WRDS credentials.")
    print("The revised script expands the date range to 3 years and saves ibes_history.rds.")
    sys.exit(1)

print(f"Event panel: {len(panel)} rows")
print(f"IBES history: {len(history)} rows")

# Ensure date columns are datetime
for col in ["earnings_date", "fpedats", "anndats", "consensus_date"]:
    if col in panel.columns:
        panel[col] = pd.to_datetime(panel[col], errors="coerce")
    if col in history.columns:
        history[col] = pd.to_datetime(history[col], errors="coerce")

# Map eps_type to accounting_basis for panel if not present
if "accounting_basis" not in panel.columns:
    panel["accounting_basis"] = panel["eps_type"].map(
        {"gaap": "GAAP", "nongaap": "Non-GAAP"}
    )

# ═══════════════════════════════════════════════════════════════════════════
# STEP 1: Rolling Surprise Volatility (sigma)
# Uses split-adjusted values (adj_eps, adj_cons) from ibes_history.
# 12-quarter rolling window, .shift(1) to prevent look-ahead bias.
# Grouped by (ticker, accounting_basis) per Patch 2.
# ═══════════════════════════════════════════════════════════════════════════

print("\n=== STEP 1: Rolling Surprise Volatility ===")

# Use split-adjusted surprise if available, else raw
if "surprise" in history.columns:
    surprise_col = "surprise"
elif "adj_eps" in history.columns and "adj_cons" in history.columns:
    history["surprise"] = history["adj_eps"] - history["adj_cons"]
    surprise_col = "surprise"
else:
    history["surprise"] = history["actual_eps"] - history["consensus_mean"]
    surprise_col = "surprise"

# Sort and compute rolling sigma per (ticker, accounting_basis)
# Use 'oftic' as the ticker identifier (matches panel's 'ticker')
ticker_col = "oftic" if "oftic" in history.columns else "ticker"

history = history.sort_values([ticker_col, "accounting_basis", "fpedats"])

def rolling_sigma(group, window=12, min_periods=4):
    """Compute rolling std of surprise with shift(1) to avoid look-ahead."""
    sigma = (
        group[surprise_col]
        .rolling(window=window, min_periods=min_periods)
        .std()
        .shift(1)  # CRITICAL: sigma for period T uses only T-12 to T-1
    )
    return sigma

history["sigma"] = (
    history
    .groupby([ticker_col, "accounting_basis"], group_keys=False)
    .apply(rolling_sigma)
    .reset_index(drop=True)
)

print(f"Sigma computed: {history['sigma'].notna().sum()} of {len(history)} rows")
print(f"Sigma summary:\n{history['sigma'].describe()}")

# ═══════════════════════════════════════════════════════════════════════════
# STEP 2: Match sigma to event panel
# For each event, find the most recent sigma for that ticker+basis
# ═══════════════════════════════════════════════════════════════════════════

print("\n=== STEP 2: Match sigma to events ===")

# Filter panel to events with required fields
events = panel.dropna(subset=["eps_target", "beat_prob_last", "ticker"]).copy()
print(f"Events with eps_target + beat_prob: {len(events)}")

def get_sigma_for_event(row):
    """Get the most recent sigma for this ticker/basis before earnings_date."""
    tk = row["ticker"]
    basis = row["accounting_basis"]
    ed = row["earnings_date"]

    mask = (
        (history[ticker_col] == tk) &
        (history["accounting_basis"] == basis) &
        (history["fpedats"] < ed) &
        (history["sigma"].notna())
    )
    sub = history.loc[mask]
    if len(sub) == 0:
        return np.nan
    return sub.iloc[-1]["sigma"]  # most recent

events["sigma"] = events.apply(get_sigma_for_event, axis=1)
print(f"Events with sigma: {events['sigma'].notna().sum()} of {len(events)}")

# ═══════════════════════════════════════════════════════════════════════════
# STEP 3: Method A — Parametric (Student's t, df=4)
# EPS ~ t(mu, sigma, nu=4) with fat tails for earnings
# implied_eps = line + sigma * t.ppf(beat_prob, df=4)
# ═══════════════════════════════════════════════════════════════════════════

print("\n=== STEP 3: Method A (Student's t, df=4) ===")

DF = 4  # degrees of freedom for fat-tailed earnings distribution

# Clamp beat_prob to [0.001, 0.999] to avoid infinity from PPF
events["beat_prob_clamped"] = events["beat_prob_last"].clip(0.001, 0.999)

# P(EPS > line) = beat_prob
# => P(EPS <= line) = 1 - beat_prob
# => line = mu + sigma * t.ppf(1 - beat_prob, df)
# => mu = line - sigma * t.ppf(1 - beat_prob, df)
# => implied_eps (= mu) = line - sigma * t.ppf(1 - beat_prob, df)
# Equivalently: implied_eps = line + sigma * t.ppf(beat_prob, df)
# because t.ppf(p) = -t.ppf(1-p) for symmetric distributions

events["implied_eps_t"] = np.where(
    events["sigma"].notna(),
    events["eps_target"] + events["sigma"] * t_dist.ppf(
        events["beat_prob_clamped"], df=DF
    ),
    np.nan
)

# Patch 4: Round to 2 decimal places (penny discretization)
events["implied_eps_t"] = events["implied_eps_t"].round(2)

print(f"Method A computed: {events['implied_eps_t'].notna().sum()}")
print(f"Method A summary:\n{events['implied_eps_t'].describe()}")

# ═══════════════════════════════════════════════════════════════════════════
# STEP 4: Method B — Non-Parametric (Empirical CDF)
# For each event, get trailing 12 quarters of raw surprises for that ticker.
# Compute Q_{1-p} = empirical (1-p) quantile of historical surprises.
# implied_eps = line - Q_{1-p}
# ═══════════════════════════════════════════════════════════════════════════

print("\n=== STEP 4: Method B (Empirical CDF) ===")

def implied_eps_ecdf(row):
    """Compute implied EPS using empirical CDF of trailing surprises."""
    tk = row["ticker"]
    basis = row["accounting_basis"]
    ed = row["earnings_date"]
    line = row["eps_target"]
    p_beat = row["beat_prob_clamped"]

    mask = (
        (history[ticker_col] == tk) &
        (history["accounting_basis"] == basis) &
        (history["fpedats"] < ed) &
        (history[surprise_col].notna())
    )
    sub = history.loc[mask].tail(12)  # last 12 quarters

    if len(sub) < 4:
        return np.nan

    surprises = sub[surprise_col].values
    # Q_{1-p}: the (1-p) quantile of historical surprises
    q_val = np.quantile(surprises, 1 - p_beat)
    return round(line - q_val, 2)  # Patch 4: penny discretization

events["implied_eps_ecdf"] = events.apply(implied_eps_ecdf, axis=1)

print(f"Method B computed: {events['implied_eps_ecdf'].notna().sum()}")
print(f"Method B summary:\n{events['implied_eps_ecdf'].describe()}")

# ═══════════════════════════════════════════════════════════════════════════
# STEP 5: Evaluation — MAE Comparison
# ═══════════════════════════════════════════════════════════════════════════

print("\n=== STEP 5: MAE Evaluation ===")

eval_df = events.dropna(subset=["actual_eps", "consensus_mean"]).copy()

# Baseline: consensus MAE
eval_df["ae_consensus"] = (eval_df["consensus_mean"] - eval_df["actual_eps"]).abs()

# Method A: t-distribution implied EPS
eval_a = eval_df.dropna(subset=["implied_eps_t"])
eval_a["ae_method_a"] = (eval_a["implied_eps_t"] - eval_a["actual_eps"]).abs()

# Method B: ECDF implied EPS
eval_b = eval_df.dropna(subset=["implied_eps_ecdf"])
eval_b["ae_method_b"] = (eval_b["implied_eps_ecdf"] - eval_b["actual_eps"]).abs()

print("\n--- MAE Comparison ---")
mae_consensus = eval_df["ae_consensus"].mean()
print(f"Baseline (Consensus):     MAE = {mae_consensus:.4f}  (n={len(eval_df)})")

if len(eval_a) > 0:
    mae_a = eval_a["ae_method_a"].mean()
    mae_cons_a = eval_a["ae_consensus"].mean()
    print(f"Method A (t-dist, df={DF}): MAE = {mae_a:.4f}  (n={len(eval_a)})")
    print(f"  vs consensus on same sample: MAE = {mae_cons_a:.4f}")
    print(f"  Improvement: {(mae_cons_a - mae_a) / mae_cons_a * 100:.1f}%")

if len(eval_b) > 0:
    mae_b = eval_b["ae_method_b"].mean()
    mae_cons_b = eval_b["ae_consensus"].mean()
    print(f"Method B (ECDF):          MAE = {mae_b:.4f}  (n={len(eval_b)})")
    print(f"  vs consensus on same sample: MAE = {mae_cons_b:.4f}")
    print(f"  Improvement: {(mae_cons_b - mae_b) / mae_cons_b * 100:.1f}%")

# By accounting basis
print("\n--- MAE by Accounting Basis ---")
for basis in ["GAAP", "Non-GAAP"]:
    sub = eval_df[eval_df["accounting_basis"] == basis]
    if len(sub) == 0:
        continue
    print(f"\n{basis} (n={len(sub)}):")
    print(f"  Consensus MAE: {sub['ae_consensus'].mean():.4f}")

    sub_a = eval_a[eval_a["accounting_basis"] == basis]
    if len(sub_a) > 0:
        print(f"  Method A MAE:  {sub_a['ae_method_a'].mean():.4f}")

    sub_b = eval_b[eval_b["accounting_basis"] == basis]
    if len(sub_b) > 0:
        print(f"  Method B MAE:  {sub_b['ae_method_b'].mean():.4f}")

# ═══════════════════════════════════════════════════════════════════════════
# SAVE RESULTS
# ═══════════════════════════════════════════════════════════════════════════

# Save full results
output_cols = [
    "market_slug", "ticker", "earnings_date", "eps_type", "accounting_basis",
    "eps_target", "actual_eps", "consensus_mean", "beat_prob_last",
    "sigma", "implied_eps_t", "implied_eps_ecdf"
]
output_cols = [c for c in output_cols if c in events.columns]
events[output_cols].to_csv(
    os.path.join(ANALYSIS_DIR, "implied_eps_results.csv"), index=False
)
print(f"\nSaved implied_eps_results.csv ({len(events)} rows)")

# Save summary text
summary_path = os.path.join(ANALYSIS_DIR, "implied_eps_summary.txt")
with open(summary_path, "w") as f:
    f.write("Implied EPS Accuracy: PM-Derived vs. Consensus\n")
    f.write("=" * 55 + "\n\n")
    f.write(f"Sample size: {len(eval_df)} events with actual EPS + consensus\n")
    f.write(f"Method A sample: {len(eval_a)} events (require sigma)\n")
    f.write(f"Method B sample: {len(eval_b)} events (require 4+ trailing quarters)\n\n")
    f.write(f"{'Method':<25} {'MAE':>8} {'n':>6}\n")
    f.write("-" * 42 + "\n")
    f.write(f"{'Consensus (baseline)':<25} {mae_consensus:>8.4f} {len(eval_df):>6}\n")
    if len(eval_a) > 0:
        f.write(f"{'Method A (t-dist df=4)':<25} {mae_a:>8.4f} {len(eval_a):>6}\n")
    if len(eval_b) > 0:
        f.write(f"{'Method B (ECDF)':<25} {mae_b:>8.4f} {len(eval_b):>6}\n")
    f.write("\nNote: Methods A and B use VWAP-based beat probability.\n")
    f.write("Sigma computed with .shift(1) to avoid look-ahead bias.\n")
    f.write("Split-adjusted EPS used where adjustment factors available.\n")
    f.write("GAAP and Non-GAAP sigma computed separately (Patch 2).\n")
    f.write("Implied EPS rounded to 2 decimal places (Patch 4).\n")

print(f"Saved implied_eps_summary.txt")

# ═══════════════════════════════════════════════════════════════════════════
# STEP 6: Diebold-Mariano Test
# Tests whether the MAE difference between consensus and PM-implied EPS
# is statistically significant. Since events are cross-sectional (different
# firms, roughly same dates), we use simple variance (no HAC needed).
# DM = mean(d) / sqrt(var(d) / n), where d_i = |e_consensus_i| - |e_method_i|
# Positive DM = Method A/B has lower error than consensus.
# ═══════════════════════════════════════════════════════════════════════════

print("\n=== STEP 6: Diebold-Mariano Test ===")
from scipy.stats import norm

def diebold_mariano(ae_baseline, ae_alternative, label=""):
    """
    Compute the Diebold-Mariano test statistic and p-value.
    d_i = ae_baseline_i - ae_alternative_i  (positive = alternative wins)
    """
    d = ae_baseline - ae_alternative
    n = len(d)
    d_bar = d.mean()
    d_var = d.var(ddof=1)

    if d_var == 0 or n < 3:
        return {"label": label, "n": n, "mean_d": d_bar,
                "dm_stat": np.nan, "p_value": np.nan}

    dm_stat = d_bar / np.sqrt(d_var / n)
    p_value = 2 * (1 - norm.cdf(abs(dm_stat)))  # two-sided

    print(f"\n  {label}")
    print(f"    n = {n}, mean(d) = {d_bar:.6f}")
    print(f"    DM stat = {dm_stat:.4f}, p-value = {p_value:.4f}")
    direction = "method wins" if d_bar > 0 else "consensus wins"
    sig = "***" if p_value < 0.01 else "**" if p_value < 0.05 else "*" if p_value < 0.10 else ""
    print(f"    Direction: {direction} {sig}")

    return {"label": label, "n": n, "mean_d": d_bar,
            "dm_stat": dm_stat, "p_value": p_value}

dm_results = []

# 1. Full sample: Consensus vs Method A
if len(eval_a) > 0:
    r = diebold_mariano(
        eval_a["ae_consensus"].values,
        eval_a["ae_method_a"].values,
        label="Full Sample: Consensus vs Method A (t-dist)"
    )
    dm_results.append(r)

# 2. Full sample: Consensus vs Method B
if len(eval_b) > 0:
    r = diebold_mariano(
        eval_b["ae_consensus"].values,
        eval_b["ae_method_b"].values,
        label="Full Sample: Consensus vs Method B (ECDF)"
    )
    dm_results.append(r)

# 3. Non-GAAP only: Consensus vs Method A
eval_a_nongaap = eval_a[eval_a["accounting_basis"] == "Non-GAAP"]
if len(eval_a_nongaap) > 0:
    r = diebold_mariano(
        eval_a_nongaap["ae_consensus"].values,
        eval_a_nongaap["ae_method_a"].values,
        label="Non-GAAP: Consensus vs Method A"
    )
    dm_results.append(r)

# 4. GAAP only: Consensus vs Method A
eval_a_gaap = eval_a[eval_a["accounting_basis"] == "GAAP"]
if len(eval_a_gaap) > 0:
    r = diebold_mariano(
        eval_a_gaap["ae_consensus"].values,
        eval_a_gaap["ae_method_a"].values,
        label="GAAP: Consensus vs Method A"
    )
    dm_results.append(r)

# Append DM results to summary file
print("\n--- Appending DM results to implied_eps_summary.txt ---")
with open(summary_path, "a") as f:
    f.write("\n\nDiebold-Mariano Test: Equal Predictive Accuracy\n")
    f.write("=" * 70 + "\n")
    f.write("H0: Consensus and PM-implied EPS have equal MAE\n")
    f.write("d_i = |e_consensus| - |e_method|; positive DM = method wins\n\n")
    f.write(f"{'Comparison':<45} {'n':>5} {'DM stat':>9} {'p-value':>9}\n")
    f.write("-" * 70 + "\n")
    for r in dm_results:
        pval_str = f"{r['p_value']:.4f}" if not np.isnan(r['p_value']) else "   N/A"
        dm_str   = f"{r['dm_stat']:+.4f}" if not np.isnan(r['dm_stat']) else "   N/A"
        f.write(f"{r['label']:<45} {r['n']:>5} {dm_str:>9} {pval_str:>9}\n")
    f.write("\nNote: Two-sided test. Simple variance used (cross-sectional data).\n")

print(f"Updated implied_eps_summary.txt with DM test results")
print("\nDone.")
