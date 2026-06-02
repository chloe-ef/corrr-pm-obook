# 2026.06.01_oos_tests.R — Out-of-sample test of the four findings on the
# new (2026-03-25) DOME export. Faithful re-use of the original computations
# (02_calibration.R, 05_returns_by_correctness.R, 08_ff_alpha.R) but:
#   - outcome = resolved_beat (on-chain Dome resolution), available for all 321
#   - return/strategy filters require only beat_prob_last + flow + returns
#     (NOT IBES actual/consensus/num_analysts, which lag for the holdout)
# Reports two subsets: FULL (all 321, independent of original) and
# HOLDOUT (post_cutoff = earnings_date > 2026-02-18, strict out-of-time).
#
# Input : build/2026.06.01_event_panel.rds, build/2026.06.01_equity_daily_returns.rds,
#         build/2026.06.01_ff_daily.rds
# Output: analysis/2026.06.01_oos_results.rds  + console summary

suppressMessages({library(data.table)})
build_dir    <- "~/Documents/data/corrr/390_paper/build"
analysis_dir <- "~/Documents/data/corrr/390_paper/analysis"

panel <- readRDS(file.path(build_dir, "2026.06.01_event_panel.rds"))
taq   <- readRDS(file.path(build_dir, "2026.06.01_equity_daily_returns.rds"))
ff    <- readRDS(file.path(build_dir, "2026.06.01_ff_daily.rds"))

t_stat <- function(x){ x<-x[!is.na(x)]; if(length(x)<3) return(NA_real_); mean(x)/(sd(x)/sqrt(length(x))) }

# ── return column + trading calendar (mirror 08_ff_alpha.R) ───────────────
ret_col <- intersect(c("stock_return","ret","daily_return","return"), names(taq))[1]
taq[, date := as.Date(date)]; ff[, date := as.Date(date)]
trading_days <- sort(unique(taq$date))
find_t0 <- function(ed){ pos<-findInterval(ed,trading_days); tgt<-pos+1L
  if(any(is.na(tgt))||tgt>length(trading_days)) return(as.Date(NA)); trading_days[tgt] }

results <- list()

# ════════════════════════════════════════════════════════════════════════
analyze <- function(p, label){
  cat(sprintf("\n################## %s ##################\n", label))
  R <- list(label=label, n=nrow(p))

  # ---- 1. Calibration + Brier (outcome = resolved_beat) ----
  pc <- p[!is.na(beat_prob_last) & !is.na(actual_beat)]
  base <- mean(pc$actual_beat)
  bs_c <- mean((pc$beat_prob_last - pc$actual_beat)^2)
  bs_n <- mean((base - pc$actual_beat)^2)
  R$brier <- list(n=nrow(pc), base=base, crowd=bs_c, naive=bs_n, bss=1-bs_c/bs_n)
  cat(sprintf("\n[1] Calibration/Brier (n=%d): base=%.3f  Brier crowd=%.3f naive=%.3f  BSS=%+.3f\n",
      nrow(pc), base, bs_c, bs_n, 1-bs_c/bs_n))
  pc[, bin := cut(beat_prob_last, c(0,.2,.4,.6,.8,1), include.lowest=TRUE,
                  labels=c("0-20","20-40","40-60","60-80","80-100"))]
  cal <- pc[, .(n=.N, mean_prob=round(mean(beat_prob_last),3),
                actual=round(mean(actual_beat),3)), by=bin][order(bin)]
  print(cal); R$calibration <- cal

  # ---- 2. Flow quintiles + logistic ----
  pf <- pc[!is.na(flow_imbalance)]
  pf[, fq := cut(flow_imbalance, quantile(flow_imbalance, seq(0,1,.2), na.rm=TRUE),
                 include.lowest=TRUE, labels=paste0("Q",1:5))]
  fcal <- pf[!is.na(fq), .(n=.N, mean_flow=round(mean(flow_imbalance),3),
                           beat=round(mean(actual_beat),3)), by=fq][order(fq)]
  cat("\n[2] Flow quintiles:\n"); print(fcal); R$flow_cal <- fcal
  m <- glm(actual_beat ~ flow_imbalance, data=pf, family=binomial)
  s <- summary(m)$coefficients
  R$flow_logit <- s["flow_imbalance",]
  cat(sprintf("    logit beat~flow: beta=%.3f z=%.2f p=%.2g\n",
      s["flow_imbalance","Estimate"], s["flow_imbalance","z value"], s["flow_imbalance","Pr(>|z|)"]))

  # ---- 3. Four-cell returns (crowd predicted x correct) ----
  pr <- p[!is.na(excess_return_1d) & !is.na(actual_beat) & !is.na(crowd_predicted_beat)]
  pr[, cell := fcase(
    crowd_predicted_beat & actual_beat,  "Predicted beat, correct",
    crowd_predicted_beat & !actual_beat, "Predicted beat, WRONG",
    !crowd_predicted_beat & !actual_beat,"Predicted miss, correct",
    !crowd_predicted_beat & actual_beat, "Predicted miss, WRONG")]
  fourcell <- pr[, .(n=.N,
    d1=round(mean(excess_return_1d)*100,2), t1=round(t_stat(excess_return_1d),2),
    d5=round(mean(excess_return_5d,na.rm=TRUE)*100,2), t5=round(t_stat(excess_return_5d),2),
    d10=round(mean(excess_return_10d,na.rm=TRUE)*100,2), t10=round(t_stat(excess_return_10d),2)),
    by=cell][order(-d1)]
  cat("\n[3] Four-cell returns (%):\n"); print(fourcell); R$four_cell <- fourcell

  # ---- 4. Short-only by threshold (strategy return = -excess) ----
  thr <- c(0.20,0.25,0.30,0.35)
  short_tab <- rbindlist(lapply(thr, function(th){
    su <- p[beat_prob_last < th & !is.na(excess_return_1d)]
    data.table(threshold=th, n=nrow(su),
      d1 =round(mean(-su$excess_return_1d)*100,2),  t1 =round(t_stat(-su$excess_return_1d),2),
      d5 =round(mean(-su$excess_return_5d,na.rm=TRUE)*100,2),  t5 =round(t_stat(-su$excess_return_5d),2),
      d10=round(mean(-su$excess_return_10d,na.rm=TRUE)*100,2), t10=round(t_stat(-su$excess_return_10d),2))
  }))
  cat("\n[4] Short-only by threshold (strategy returns %):\n"); print(short_tab); R$short_only <- short_tab

  # ---- 5. Long/short + flow-enhanced (mirror 05) ----
  p[, side := fcase(beat_prob_last>0.7,"long", beat_prob_last<0.3,"short", default="no")]
  p[, sr1 := fcase(side=="long",excess_return_1d, side=="short",-excess_return_1d, default=NA_real_)]
  p[, sr5 := fcase(side=="long",excess_return_5d, side=="short",-excess_return_5d, default=NA_real_)]
  p[, sr10:= fcase(side=="long",excess_return_10d,side=="short",-excess_return_10d,default=NA_real_)]
  ls <- p[side!="no" & !is.na(sr1)]
  byside <- ls[, .(n=.N, d1=round(mean(sr1)*100,2), t1=round(t_stat(sr1),2),
                   d5=round(mean(sr5,na.rm=TRUE)*100,2), t5=round(t_stat(sr5),2),
                   d10=round(mean(sr10,na.rm=TRUE)*100,2), t10=round(t_stat(sr10),2)), by=side]
  comb <- ls[, .(side="combined", n=.N, d1=round(mean(sr1)*100,2), t1=round(t_stat(sr1),2),
                 d5=round(mean(sr5,na.rm=TRUE)*100,2), t5=round(t_stat(sr5),2),
                 d10=round(mean(sr10,na.rm=TRUE)*100,2), t10=round(t_stat(sr10),2))]
  ls_tab <- rbind(byside, comb)
  cat("\n[5] Long/Short (prob>0.7 / <0.3):\n"); print(ls_tab); R$long_short <- ls_tab

  # ---- 7. Bias regression + beat-vs-consensus (IBES matched subset only) ----
  pm <- p[!is.na(actual_eps) & !is.na(consensus_mean)]
  if (nrow(pm) >= 10){
    br <- lm(actual_eps ~ consensus_mean, data=pm); sb <- summary(br)$coefficients
    R$bias_reg <- list(n=nrow(pm), alpha=sb[1,1], alpha_t=sb[1,3], beta=sb[2,1], r2=summary(br)$r.squared)
    cat(sprintf("\n[7] Bias reg actual~consensus (IBES n=%d): alpha=%.3f (t=%.2f) beta=%.3f R2=%.3f\n",
        nrow(pm), sb[1,1], sb[1,3], sb[2,1], summary(br)$r.squared))
    cat(sprintf("    beat-vs-consensus rate: %.1f%%  (median |IBES surprise| caveat: annual contamination)\n",
        100*mean(pm$actual_eps > pm$consensus_mean)))
  } else cat(sprintf("\n[7] Bias reg: only %d IBES-matched events in subset — skipped (IBES lag)\n", nrow(pm)))

  R
}

# ── FF3 alpha on short portfolio (mirror 08_ff_alpha.R, pooled + cal-time) ──
ff3_short <- function(p, thresholds=c(0.20,0.25,0.30,0.35), horizon=10){
  out <- list()
  for (th in thresholds){
    se <- p[!is.na(beat_prob_last) & beat_prob_last < th & !is.na(excess_return_1d)]
    if (nrow(se) < 5){ out[[paste0("th",th)]] <- list(threshold=th, n=nrow(se), note="too few"); next }
    rows <- list()
    for (j in seq_len(nrow(se))){
      ev <- se[j]; t0 <- find_t0(ev$earnings_date); if(is.na(t0)) next
      pos <- which(trading_days==t0); if(!length(pos)) next
      days <- trading_days[pos:min(pos+horizon-1, length(trading_days))]
      sr <- taq[ticker==ev$ticker & date %in% days, .(date, stock_ret=get(ret_col))]
      if(!nrow(sr)) next
      mg <- merge(sr, ff, by="date"); mg <- mg[!is.na(mkt_rf)&!is.na(stock_ret)]
      if(!nrow(mg)) next
      mg[, short_excess := -(stock_ret) - rf]; mg[, ev:=ev$market_slug]
      rows[[length(rows)+1]] <- mg
    }
    if(!length(rows)){ out[[paste0("th",th)]]<-list(threshold=th,n=nrow(se),note="no taq"); next }
    dp <- rbindlist(rows)
    reg <- lm(short_excess ~ mkt_rf + smb + hml, data=dp); ct <- summary(reg)$coefficients
    out[[paste0("th",th)]] <- list(threshold=th, n_events=uniqueN(dp$ev), n_obs=nrow(dp),
      alpha=ct[1,1], alpha_t=ct[1,3], alpha_p=ct[1,4],
      mkt=ct["mkt_rf",1], smb=ct["smb",1], hml=ct["hml",1], r2=summary(reg)$r.squared)
    cat(sprintf("    FF3 P<%.2f: n=%d alpha/d=%+.4f (%.1f%%/yr) t=%.2f Mkt=%.2f SMB=%.2f HML=%.2f\n",
        th, uniqueN(dp$ev), ct[1,1], ct[1,1]*252*100, ct[1,3], ct["mkt_rf",1], ct["smb",1], ct["hml",1]))
  }
  out
}

# ── Finding #4: smart-wallet persistence (out-of-sample) ──────────────────
wallet_persistence <- function(){
  sw  <- readRDS(file.path(analysis_dir, "smart_wallet_profiles.rds"))
  addr <- tolower(sw$addresses)
  tr  <- fread(file.path(build_dir, "2026.06.01_dome_trades_earnings.csv"))
  res <- fread(file.path(build_dir, "2026.06.01_market_resolution.csv"))
  tr[, maker_address := tolower(maker_address)]
  tr[, dollar_volume := shares/1e6 * price]
  tr[, beatflow := fcase(side=="BUY"&bid_type=="yes", dollar_volume,
                         side=="SELL"&bid_type=="no", dollar_volume,
                         side=="BUY"&bid_type=="no", -dollar_volume,
                         side=="SELL"&bid_type=="yes", -dollar_volume)]
  wm <- tr[maker_address %in% addr, .(net=sum(beatflow)), by=.(maker_address, market_slug)]
  wm <- merge(wm, res[, .(market_slug, resolved_beat)], by="market_slug")
  wm <- merge(wm, panel[, .(market_slug, post_cutoff)], by="market_slug")
  wm <- wm[net != 0 & !is.na(resolved_beat)]
  wm[, correct := (net>0) == resolved_beat]
  r <- list(
    n_wallets_present = length(intersect(addr, unique(tr$maker_address))),
    calls_full = nrow(wm), hit_full = mean(wm$correct),
    calls_holdout = nrow(wm[post_cutoff==TRUE]), hit_holdout = mean(wm[post_cutoff==TRUE]$correct),
    baseline = mean(unique(wm[, .(market_slug, resolved_beat)])$resolved_beat))
  cat(sprintf("\n[Finding 4] Smart-wallet persistence: %d/22 wallets present; pooled hit %.1f%% (n=%d calls); holdout hit %.1f%% (n=%d); baseline %.1f%%\n",
      r$n_wallets_present, 100*r$hit_full, r$calls_full, 100*r$hit_holdout, r$calls_holdout, 100*r$baseline))
  r
}
results$wallets <- wallet_persistence()

results$full    <- analyze(copy(panel), "FULL NEW SAMPLE (321, independent of original)")
cat("\n[6] FF3 alpha — FULL:\n");    results$full$ff3    <- ff3_short(copy(panel))

results$holdout <- analyze(panel[post_cutoff==TRUE], "HOLDOUT (post-2026-02-18, strict out-of-time)")
cat("\n[6] FF3 alpha — HOLDOUT:\n"); results$holdout$ff3 <- ff3_short(panel[post_cutoff==TRUE])

saveRDS(results, file.path(analysis_dir, "2026.06.01_oos_results.rds"))
cat("\nSaved 2026.06.01_oos_results.rds\n")
