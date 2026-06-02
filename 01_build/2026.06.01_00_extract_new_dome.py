#!/usr/bin/env python3
# 2026.06.01_00_extract_new_dome.py
# Stream the 8.3GB DOME re-export -> compact earnings-trade table + per-market
# resolution/metadata table. Drops title_embedding/description/etc.
#
# Run with the Anaconda interpreter that has duckdb installed:
#   /opt/anaconda3/bin/python 01_build/2026.06.01_00_extract_new_dome.py
#
# Does NOT modify any original file. Outputs are 2026.06.01_-prefixed.
# Resolution is recoverable from Dome alone via outcome_token_id (which token
# paid $1). bid_type is derived from the token label (Yes/No). dollar_volume is
# left as raw shares*price here; the share-scaling factor k is locked downstream
# in R by matching an overlapping market to the original dome_trades_combined.rds.

import duckdb, time, os

IMPORT_DIR = os.path.expanduser("~/Documents/data/corrr/390_paper/import")
BUILD_DIR  = os.path.expanduser("~/Documents/data/corrr/390_paper/build")
CSV = os.path.join(IMPORT_DIR, "DOME_chloe_data_202603251241.csv")

TRADES_OUT = os.path.join(BUILD_DIR, "2026.06.01_dome_trades_earnings.csv")
RESOLU_OUT = os.path.join(BUILD_DIR, "2026.06.01_market_resolution.csv")

EARNINGS_RE = 'quarterly-earnings-(gaap|nongaap)-eps'

con = duckdb.connect()
con.execute("PRAGMA threads=8")

read = (f"read_csv('{CSV}', all_varchar=true, ignore_errors=true)")

# Reusable derivation: bid_type from token label; yes_token_id from labels.
base = f"""
  SELECT
    market_slug, title, category, start_date, end_date, close_time,
    block_timestamp, side, maker_address, taker_address,
    TRY_CAST(shares AS DOUBLE)  AS shares,
    TRY_CAST(price  AS DOUBLE)  AS price,
    token_id, primary_token_id, secondary_token_id,
    primary_token_label, secondary_token_label, outcome_token_id,
    volume_total,
    CASE WHEN token_id = primary_token_id   THEN lower(primary_token_label)
         WHEN token_id = secondary_token_id THEN lower(secondary_token_label)
         ELSE NULL END AS bid_type,
    CASE WHEN primary_token_label = 'Yes' THEN primary_token_id
         WHEN secondary_token_label = 'Yes' THEN secondary_token_id
         ELSE NULL END AS yes_token_id
  FROM {read}
  WHERE regexp_matches(market_slug, '{EARNINGS_RE}')
"""

t = time.time()
print("Scanning 8.3GB CSV once -> materializing earnings subset ...")
con.execute(f"CREATE TEMP TABLE e AS ({base})")
print(f"  scan complete ({time.time()-t:.1f}s)")

t = time.time()
con.execute(f"""
COPY (
  SELECT market_slug, title, start_date, end_date, close_time,
         block_timestamp, side, maker_address, taker_address,
         bid_type, shares, price
  FROM e
) TO '{TRADES_OUT}' (HEADER, DELIMITER ',')
""")
print(f"  trades written -> {TRADES_OUT}  ({time.time()-t:.1f}s)")

# Per-market resolution + metadata (one row per slug).
t = time.time()
con.execute(f"""
COPY (
  SELECT
    market_slug,
    any_value(title)                 AS title,
    any_value(category)              AS category,
    any_value(primary_token_id)      AS primary_token_id,
    any_value(secondary_token_id)    AS secondary_token_id,
    any_value(primary_token_label)   AS primary_token_label,
    any_value(secondary_token_label) AS secondary_token_label,
    any_value(outcome_token_id)      AS outcome_token_id,
    any_value(yes_token_id)          AS yes_token_id,
    CASE WHEN any_value(outcome_token_id) = any_value(yes_token_id) THEN TRUE
         WHEN any_value(outcome_token_id) IS NULL OR any_value(outcome_token_id) = '' THEN NULL
         ELSE FALSE END              AS resolved_beat,
    any_value(volume_total)          AS volume_total,
    count(*)                         AS n_trades,
    min(block_timestamp)             AS first_ts,
    max(block_timestamp)             AS last_ts
  FROM e
  GROUP BY market_slug
) TO '{RESOLU_OUT}' (HEADER, DELIMITER ',')
""")
print(f"  resolution written -> {RESOLU_OUT}  ({time.time()-t:.1f}s)")

# Quick summary
n_tr = con.sql(f"SELECT count(*) FROM read_csv('{TRADES_OUT}')").fetchone()[0]
n_mk = con.sql(f"SELECT count(*) FROM read_csv('{RESOLU_OUT}')").fetchone()[0]
print(f"\nearnings trades: {n_tr:,}   markets: {n_mk:,}")
print("\nbid_type distribution:")
print(con.sql(f"SELECT bid_type, count(*) n FROM read_csv('{TRADES_OUT}') GROUP BY 1 ORDER BY 2 DESC"))
print("\nresolved_beat distribution (markets):")
print(con.sql(f"SELECT resolved_beat, count(*) n FROM read_csv('{RESOLU_OUT}') GROUP BY 1 ORDER BY 2 DESC"))
