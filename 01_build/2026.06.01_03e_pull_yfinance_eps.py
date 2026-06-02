#!/usr/bin/env python3
# 2026.06.01_03e_pull_yfinance_eps.py
# Street-basis reported EPS + consensus estimate from yfinance for the non-GAAP
# markets (Compustat only has GAAP). yfinance "Reported EPS" / "EPS Estimate"
# are the analyst/adjusted basis the non-GAAP markets and IBES consensus use,
# and are current to today. Output: build/2026.06.01_yfinance_eps.csv
# Run: /opt/anaconda3/bin/python 01_build/2026.06.01_03e_pull_yfinance_eps.py
import yfinance as yf, pandas as pd, time, os
B = os.path.expanduser("~/Documents/data/corrr/390_paper/build")
ev = pd.read_csv(os.path.join(B, "2026.06.01_oos_events_for_actuals.csv"))
tickers = sorted(ev.loc[ev.eps_type == "nongaap", "ticker"].unique())
print(f"pulling yfinance earnings history for {len(tickers)} non-GAAP tickers")
rows, fails = [], []
for i, tk in enumerate(tickers):
    try:
        df = yf.Ticker(tk).get_earnings_dates(limit=20)
        if df is None or df.empty:
            fails.append(tk)
        else:
            df = df.reset_index()
            dcol = "Earnings Date" if "Earnings Date" in df.columns else df.columns[0]
            for _, r in df.iterrows():
                rows.append({"ticker": tk,
                             "report_date": str(r[dcol])[:10],
                             "eps_actual": r.get("Reported EPS"),
                             "eps_estimate": r.get("EPS Estimate")})
    except Exception as e:
        fails.append(f"{tk}:{type(e).__name__}")
    time.sleep(0.25)
    if (i + 1) % 25 == 0:
        print(f"  {i+1}/{len(tickers)}  rows={len(rows)} fails={len(fails)}")
out = pd.DataFrame(rows)
out = out.dropna(subset=["eps_actual"])
out.to_csv(os.path.join(B, "2026.06.01_yfinance_eps.csv"), index=False)
print(f"wrote {len(out)} rows for {out.ticker.nunique()} tickers; {len(fails)} tickers with no/failed data")
if fails:
    print("  no-data/failed:", ", ".join(map(str, fails[:40])))
