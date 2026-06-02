# 2026.06.01_03f_build_actuals_and_walkdown.R
# Route-2 reconstruction of the analyst-dependent findings on the OOS holdout,
# using current actuals (Compustat GAAP + yfinance street) since WRDS IBES lags
# to 2026-02-19. Consensus is proxied by the MM line (paper: line==consensus,
# R^2=0.999). Builds basis-matched actual EPS, validates it against the on-chain
# resolution, and re-estimates the analyst walk-down + beat-vs-consensus.
#
# Input : build/2026.06.01_dome_eps_events.rds, 2026.06.01_compustat_eps.rds,
#         build/2026.06.01_yfinance_eps.csv
# Output: build/2026.06.01_actuals_oos.rds + console summary

suppressMessages(library(data.table))
B <- path.expand("~/Documents/data/corrr/390_paper/build")
ev <- readRDS(file.path(B,"2026.06.01_dome_eps_events.rds"))
cs <- readRDS(file.path(B,"2026.06.01_compustat_eps.rds"))
yf <- fread(file.path(B,"2026.06.01_yfinance_eps.csv"))
yf[, report_date := as.Date(report_date)]
cs[, rdq := as.Date(rdq)]

# nearest-report match within +/- 7 days of the slug earnings_date
nearest <- function(tk, ed, dt, datecol, valcol, win=7){
  s <- dt[get("tic_or_ticker")==tk]
  if(!nrow(s)) return(NA_real_)
  d <- abs(as.numeric(s[[datecol]] - ed)); i <- which.min(d)
  if(length(i) && d[i] <= win) return(as.numeric(s[[valcol]][i])); NA_real_
}
cs2 <- cs[, .(tic_or_ticker=tic, rdq, val=epsfxq)]          # GAAP diluted
yf2 <- yf[, .(tic_or_ticker=ticker, report_date, val=eps_actual)]

ev[, actual_eps_oos := NA_real_]; ev[, actual_src := NA_character_]
for(i in seq_len(nrow(ev))){
  tk <- ev$ticker[i]; ed <- ev$earnings_date[i]; if(is.na(ed)) next
  if(ev$eps_type[i]=="gaap"){
    v <- nearest(tk, ed, cs2, "rdq", "val"); if(!is.na(v)){ ev$actual_eps_oos[i]<-v; ev$actual_src[i]<-"compustat" }
  } else {
    v <- nearest(tk, ed, yf2, "report_date", "val"); if(!is.na(v)){ ev$actual_eps_oos[i]<-v; ev$actual_src[i]<-"yfinance" }
  }
}

cat("=== actual EPS coverage (new actuals, current sources) ===\n")
cat("full sample matched:", sum(!is.na(ev$actual_eps_oos)), "of", nrow(ev), "\n")
cat("holdout matched    :", sum(!is.na(ev[post_cutoff==TRUE]$actual_eps_oos)), "of", nrow(ev[post_cutoff==TRUE]), "\n")
print(ev[, .(matched=sum(!is.na(actual_eps_oos)), n=.N), by=.(eps_type, post_cutoff)][order(eps_type,post_cutoff)])

# VALIDATION: does (actual_oos > line) agree with the on-chain resolution?
chk <- ev[!is.na(actual_eps_oos) & !is.na(resolved_beat)]
chk[, beat_oos := actual_eps_oos > eps_target]
cat(sprintf("\n=== VALIDATION: (actual_oos > line) vs Dome resolved_beat: %d/%d (%.1f%%) ===\n",
    sum(chk$beat_oos==chk$resolved_beat), nrow(chk), 100*mean(chk$beat_oos==chk$resolved_beat)))
cat("  (recall IBES actual>line agreed only 88.8%% due to annual contamination)\n")

saveRDS(ev, file.path(B,"2026.06.01_actuals_oos.rds"))

# ── Analyst walk-down + beat-vs-consensus (line as consensus proxy) ─────────
walkdown <- function(d, label){
  d <- d[!is.na(actual_eps_oos) & !is.na(eps_target)]
  if(nrow(d) < 8){ cat(sprintf("\n[%s] n=%d too few\n", label, nrow(d))); return(invisible()) }
  m <- lm(actual_eps_oos ~ eps_target, d); s <- summary(m)$coefficients
  cat(sprintf("\n[%s] walk-down  actual ~ line  (n=%d): alpha=%+.3f (t=%.2f) beta=%.3f R2=%.3f\n",
      label, nrow(d), s[1,1], s[1,3], s[2,1], summary(m)$r.squared))
  cat(sprintf("    beat-vs-line rate=%.1f%%  median surprise(actual-line)=%+.3f  mean=%+.3f\n",
      100*mean(d$actual_eps_oos>d$eps_target), median(d$actual_eps_oos-d$eps_target), mean(d$actual_eps_oos-d$eps_target)))
}
cat("\n================= ANALYST WALK-DOWN (OOS, route-2 actuals) =================")
cat("\n--- paper baseline: alpha=+1.55 (t=5.88), beta=1.15, R2=0.675, beat 81%% ---\n")
walkdown(ev[post_cutoff==TRUE], "HOLDOUT all")
walkdown(ev[post_cutoff==TRUE & eps_type=="gaap"], "HOLDOUT GAAP (Compustat)")
walkdown(ev[post_cutoff==TRUE & eps_type=="nongaap"], "HOLDOUT non-GAAP (yfinance)")
walkdown(ev, "FULL NEW all")
cat("\nSaved 2026.06.01_actuals_oos.rds\n")
