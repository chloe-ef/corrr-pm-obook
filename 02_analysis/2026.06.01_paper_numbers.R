# 2026.06.01_paper_numbers.R
# =============================================================================
# SINGLE SOURCE OF TRUTH for every number cited in the SSRN draft
# (03_paper/2026.06.01_main.tex). Run this; cite the paper ONLY from its stdout
# / saved RDS. The paper-wide verifier (03_paper/2026.06.01_verify_memo.R)
# independently recomputes each cited number from source and must match.
#
# CONSISTENT METHODOLOGY (applied UNIFORMLY):
#  - Beat/miss outcome:  primary 340 -> clean IBES (qtr_eps > eps_target);
#                        OOS 321      -> Dome on-chain resolution (resolved_beat).
#  - Actual EPS magnitude (walk-down, implied-EPS, disagreement): clean IBES
#                        quarterly only (pdicity='QTR'); yfinance/Compustat dropped.
#  - Consensus = market line (slug eps_target) ~ IBES consensus_mean.
#  - Returns / strategy / four-cell / FF3 (ALL): t0 = first trading day AFTER the
#    actual IBES announcement (anndats), fall back to slug earnings_date when
#    anndats missing; DROP look-ahead BMO events (last PM trade not strictly
#    before anndats); winsorize excess returns 2/98; short return = -(SPY-excess);
#    thresholds 0.20/0.25/0.30/0.35; horizons 1/5/10 trading days.
#  - Samples: 340 primary; 321 OOS; pooled 661 (disjoint) as a power check.
#
# Inputs (source data only):
#   PRIMARY 340: build/event_panel.rds, build/equity_daily_returns.rds,
#                build/index_daily_returns.rds, build/ff_daily.rds,
#                build/2026.06.01_walkdown_clean.rds, build/dome_eps_events.rds,
#                analysis/implied_eps_results.csv, import/dome_trades_combined.rds
#   OOS 321    : build/2026.06.01_event_panel.rds, build/2026.06.01_dome_eps_events.rds,
#                build/2026.06.01_equity_daily_returns.rds,
#                build/2026.06.01_index_daily_returns.rds, build/2026.06.01_ff_daily.rds,
#                build/2026.06.01_ibes_history.rds, build/2026.06.01_dome_trades_earnings.csv,
#                build/2026.06.01_market_resolution.csv, analysis/smart_wallet_profiles.rds
# Output: stdout (tagged by paper location) + analysis/2026.06.01_paper_numbers.rds
# =============================================================================
suppressMessages(library(data.table))
B <- path.expand("~/Documents/data/corrr/390_paper")
options(width = 140)

ts  <- function(x){ x <- x[!is.na(x)]; if(length(x) < 2) return(NA_real_); mean(x)/(sd(x)/sqrt(length(x))) }
win <- function(x){ q <- quantile(x, c(.02,.98), na.rm = TRUE); pmin(pmax(x, q[1]), q[2]) }
RES <- list()
hdr <- function(t) cat(sprintf("\n\n=============== %s ===============\n", t))

# ---------------------------------------------------------------------------
# Re-anchoring engine: recompute er1/er5/er10 around t0 = next trading day after
# the announcement (anndats) [fallback earnings_date]; flag look-ahead BMO events.
# Generic so primary (OLD data) and OOS (NEW data) use IDENTICAL methodology.
# ---------------------------------------------------------------------------
reanchor <- function(panel, taq, idx){
  p <- copy(panel); taq <- copy(taq); idx <- copy(idx)
  taq[, date := as.Date(date)]; idx[, date := as.Date(date)]
  rc <- intersect(c("stock_return","ret"), names(taq))[1]
  ic <- intersect(c("sp500_ret","ret"), names(idx))[1]
  td <- sort(unique(taq$date))
  p[, `:=`(announcement_date = as.Date(announcement_date),
           earnings_date     = as.Date(earnings_date),
           last_prob_date    = as.Date(last_prob_date))]
  p[, anchor := fifelse(!is.na(announcement_date), announcement_date, earnings_date)]
  t0f <- function(d){ pos <- findInterval(d, td); tt <- pos + 1L
    if(is.na(tt) || tt > length(td)) return(as.Date(NA)); td[tt] }
  p[, t0      := as.Date(sapply(anchor,        function(d) as.numeric(t0f(d))), origin="1970-01-01")]
  p[, t0_slug := as.Date(sapply(earnings_date, function(d) as.numeric(t0f(d))), origin="1970-01-01")]
  cumret <- function(tk, d0, h, dt, col){
    if(is.na(d0)) return(NA_real_); pos <- which(td == d0); if(!length(pos)) return(NA_real_)
    days <- td[pos:min(pos + h - 1, length(td))]
    r <- if(is.null(tk)) dt[date %in% days, get(col)] else dt[ticker == tk & date %in% days, get(col)]
    if(!length(r)) return(NA_real_); prod(1 + r, na.rm = TRUE) - 1 }
  for(cc in c("cs1","ci1","cs5","ci5","cs10","ci10")) p[, (cc) := NA_real_]
  for(i in seq_len(nrow(p))){ tk <- p$ticker[i]; d0 <- p$t0[i]
    set(p, i, "cs1",  cumret(tk,  d0, 1,  taq, rc)); set(p, i, "ci1",  cumret(NULL, d0, 1,  idx, ic))
    set(p, i, "cs5",  cumret(tk,  d0, 5,  taq, rc)); set(p, i, "ci5",  cumret(NULL, d0, 5,  idx, ic))
    set(p, i, "cs10", cumret(tk,  d0, 10, taq, rc)); set(p, i, "ci10", cumret(NULL, d0, 10, idx, ic)) }
  p[, er1 := cs1 - ci1][, er5 := cs5 - ci5][, er10 := cs10 - ci10]
  for(cc in c("er1","er5","er10")) p[!is.na(get(cc)), (cc) := win(get(cc))]
  p[, bmo        := !is.na(announcement_date) & announcement_date < earnings_date]
  p[, lookahead  := bmo & (is.na(last_prob_date) | last_prob_date >= announcement_date)]
  p[, reanchored := !is.na(t0) & !is.na(t0_slug) & t0 != t0_slug]
  p[]
}

# short-only strategy table (Panel B): strategy return = -(excess return)
short_table <- function(d, thresholds = c(0.20,0.25,0.30,0.35)){
  rbindlist(lapply(thresholds, function(th){
    s <- d[!is.na(beat_prob_last) & beat_prob_last < th]
    data.table(threshold = th, n = sum(!is.na(s$er1)),
      d1  = mean(-s$er1,  na.rm=TRUE)*100, t1  = ts(-s$er1),
      d5  = mean(-s$er5,  na.rm=TRUE)*100, t5  = ts(-s$er5),
      d10 = mean(-s$er10, na.rm=TRUE)*100, t10 = ts(-s$er10)) }))
}
# directional long/short table (Panel A): long if prob>0.7, short if prob<0.3
ls_table <- function(d){
  q <- copy(d)
  q[, side := fcase(beat_prob_last > 0.7, "long", beat_prob_last < 0.3, "short", default="no")]
  q[, sr1  := fcase(side=="long", er1,  side=="short", -er1,  default=NA_real_)]
  q[, sr5  := fcase(side=="long", er5,  side=="short", -er5,  default=NA_real_)]
  q[, sr10 := fcase(side=="long", er10, side=="short", -er10, default=NA_real_)]
  ls <- q[side != "no" & !is.na(sr1)]
  by <- ls[, .(n=.N, d1=mean(sr1)*100, t1=ts(sr1), d5=mean(sr5,na.rm=TRUE)*100, t5=ts(sr5),
               d10=mean(sr10,na.rm=TRUE)*100, t10=ts(sr10)), by=side]
  comb <- ls[, .(side="combined", n=.N, d1=mean(sr1)*100, t1=ts(sr1), d5=mean(sr5,na.rm=TRUE)*100,
                 t5=ts(sr5), d10=mean(sr10,na.rm=TRUE)*100, t10=ts(sr10))]
  rbind(by[order(match(side,c("long","short")))], comb)
}
# build pooled event-DAY short-excess panel (for FF3), re-anchored on t0
short_day_panel <- function(d, taq, ff, thr, horizon = 10){
  taq <- copy(taq); ff <- copy(ff); taq[, date:=as.Date(date)]; ff[, date:=as.Date(date)]
  rc <- intersect(c("stock_return","ret"), names(taq))[1]
  td <- sort(unique(taq$date))
  se <- d[!is.na(beat_prob_last) & beat_prob_last < thr & !is.na(t0)]
  rows <- list()
  for(j in seq_len(nrow(se))){ ev <- se[j]; t0 <- ev$t0
    pos <- which(td == t0); if(!length(pos)) next
    days <- td[pos:min(pos + horizon - 1, length(td))]
    sr <- taq[ticker == ev$ticker & date %in% days, .(date, stock_ret = get(rc))]; if(!nrow(sr)) next
    mg <- merge(sr, ff, by="date"); mg <- mg[!is.na(mkt_rf) & !is.na(stock_ret)]; if(!nrow(mg)) next
    mg[, short_excess := -(stock_ret) - rf]; mg[, ev := ev$market_slug]
    rows[[length(rows)+1]] <- mg }
  if(!length(rows)) return(NULL)
  rbindlist(rows)
}
# FF3 regression + alpha-vs-factor decomposition + 95% CI from a day panel
ff3_fit <- function(dp){
  if(is.null(dp) || nrow(dp) < 5) return(NULL)
  reg <- lm(short_excess ~ mkt_rf + smb + hml, data = dp); ct <- summary(reg)$coefficients
  ci  <- tryCatch(confint(reg)["(Intercept)", ], error=function(e) c(NA,NA))
  b   <- coef(reg)
  contrib <- c(alpha = unname(b[1]),
               mkt = unname(b["mkt_rf"]*mean(dp$mkt_rf)),
               smb = unname(b["smb"]*mean(dp$smb)),
               hml = unname(b["hml"]*mean(dp$hml)))
  list(n_events = uniqueN(dp$ev), n_obs = nrow(dp),
       alpha = ct[1,1], alpha_se = ct[1,2], alpha_t = ct[1,3], alpha_p = ct[1,4],
       ci_lo = ci[1], ci_hi = ci[2],
       mkt = ct["mkt_rf",1], smb = ct["smb",1], hml = ct["hml",1], r2 = summary(reg)$r.squared,
       mean_excess = mean(dp$short_excess), contrib = contrib)
}
print_ff3 <- function(f, lab){
  if(is.null(f)){ cat(sprintf("  %-16s : too few obs\n", lab)); return(invisible()) }
  cat(sprintf("  %-16s n=%d obs=%d  alpha/d=%+.4f (%.2f%%/yr) t=%.2f p=%.3f CI[%+.4f,%+.4f]  Mkt=%.2f SMB=%.2f HML=%.2f R2=%.3f\n",
      lab, f$n_events, f$n_obs, f$alpha, f$alpha*252*100, f$alpha_t, f$alpha_p, f$ci_lo, f$ci_hi, f$mkt, f$smb, f$hml, f$r2))
  cat(sprintf("    decomp daily short excess %+.4f = alpha %+.4f + mkt %+.4f + smb %+.4f + hml %+.4f\n",
      f$mean_excess, f$contrib["alpha"], f$contrib["mkt"], f$contrib["smb"], f$contrib["hml"]))
}

# ===========================================================================
# PRIMARY 340 (clean IBES labels; re-anchored returns)
# ===========================================================================
hdr("PRIMARY 340  -- load + re-anchor")
pan  <- readRDS(file.path(B,"build/event_panel.rds"))
taqP <- readRDS(file.path(B,"build/equity_daily_returns.rds"))
idxP <- readRDS(file.path(B,"build/index_daily_returns.rds"))
ffP  <- readRDS(file.path(B,"build/ff_daily.rds"))
wc   <- readRDS(file.path(B,"build/2026.06.01_walkdown_clean.rds"))[, .(market_slug, qtr_eps)]

paR  <- reanchor(pan, taqP, idxP)                       # re-anchored returns for all 340
paR  <- merge(paR, wc, by="market_slug", all.x=TRUE)
paR[, actual_beat := qtr_eps > eps_target]              # clean beat/miss
paR[, crowd_pred  := beat_prob_last > 0.5]
cat(sprintf("re-anchored (t0 moved): %d | look-ahead dropped: %d | with qtr_eps: %d\n",
    sum(paR$reanchored, na.rm=TRUE), sum(paR$lookahead, na.rm=TRUE), sum(!is.na(paR$qtr_eps))))
clnP <- paR[lookahead == FALSE]                         # return sample (look-ahead removed)

# clean label sample (for #1/#2: needs qtr_eps + eps_target + beat_prob_last)
pc <- paR[!is.na(qtr_eps) & !is.na(eps_target) & !is.na(beat_prob_last)]
RES$primary$n_labelled <- nrow(pc)
RES$primary$beat_rate  <- 100*mean(pc$actual_beat)
cat(sprintf("\n[Data] primary clean-labelled n=%d  beat rate=%.1f%%\n", nrow(pc), 100*mean(pc$actual_beat)))

hdr("PRIMARY 340  -- #1 Calibration + Brier  (tab:calibration / Brier subsec)")
base <- mean(pc$actual_beat); bc <- mean((pc$beat_prob_last-pc$actual_beat)^2); bn <- mean((base-pc$actual_beat)^2)
RES$primary$brier <- list(n=nrow(pc), crowd=bc, naive=bn, bss=1-bc/bn)
cat(sprintf("Brier crowd=%.3f naive=%.3f BSS=%+.3f (n=%d)\n", bc, bn, 1-bc/bn, nrow(pc)))
pc[, bin := cut(beat_prob_last, c(0,.2,.4,.6,.8,1), include.lowest=TRUE, labels=c("0-20","20-40","40-60","60-80","80-100"))]
cal <- pc[, .(n=.N, mean_p=round(mean(beat_prob_last),3), actual=round(mean(actual_beat),3)), by=bin][order(bin)]
print(cal); RES$primary$calibration <- cal

hdr("PRIMARY 340  -- #2 Flow quintiles + logit + linear  (tab:flow_calibration / flow regs)")
pf <- pc[!is.na(flow_imbalance)]
pf[, fq := cut(flow_imbalance, quantile(flow_imbalance, seq(0,1,.2), na.rm=TRUE), include.lowest=TRUE, labels=paste0("Q",1:5))]
fcal <- pf[!is.na(fq), .(n=.N, mean_flow=round(mean(flow_imbalance),3), beat=round(mean(actual_beat),3)), by=fq][order(fq)]
print(fcal); RES$primary$flow_quintiles <- fcal
ml <- glm(actual_beat ~ flow_imbalance, pf, family=binomial); sl <- summary(ml)$coefficients
RES$primary$flow_logit <- list(beta=sl[2,1], z=sl[2,3], p=sl[2,4])
cat(sprintf("logit beat~flow: beta=%.3f z=%.2f p=%.2g\n", sl[2,1], sl[2,3], sl[2,4]))
pfd <- pf[!is.na(consensus_mean)]; pfd[, delta_an_clean := consensus_mean - qtr_eps]
mlin <- lm(delta_an_clean ~ flow_imbalance, pfd); slin <- summary(mlin)$coefficients
RES$primary$flow_linear <- list(n=nrow(pfd), beta=slin[2,1], t=slin[2,3])
cat(sprintf("linear delta_an(=consensus-actual)~flow: beta=%.3f t=%.2f (n=%d)\n", slin[2,1], slin[2,3], nrow(pfd)))

hdr("PRIMARY 340  -- #2 Flow terciles  (tab:refinement)")
pt <- pf[!is.na(fq)]
pt[, ft := cut(flow_imbalance, quantile(flow_imbalance, c(0,1/3,2/3,1), na.rm=TRUE), include.lowest=TRUE, labels=c("T1 bearish","T2","T3 bullish"))]
ftab <- pt[!is.na(ft), .(n=.N, mean_flow=round(mean(flow_imbalance),3), beat=round(mean(actual_beat),3),
                         delta_an=round(mean(consensus_mean-qtr_eps,na.rm=TRUE),3)), by=ft][order(ft)]
print(ftab); RES$primary$flow_terciles <- ftab

hdr("PRIMARY 340  -- Walk-down regression  (tab:bias_reg)  [source: walkdown_clean.rds]")
wcfull <- readRDS(file.path(B,"build/2026.06.01_walkdown_clean.rds"))
pw <- wcfull[!is.na(qtr_eps) & !is.na(consensus_mean)]
mw <- lm(qtr_eps ~ consensus_mean, pw); sw <- summary(mw)$coefficients
RES$primary$walkdown <- list(n=nrow(pw), alpha=sw[1,1], alpha_se=sw[1,2], alpha_t=sw[1,3],
                             beta=sw[2,1], beta_se=sw[2,2], beta_t=sw[2,3], r2=summary(mw)$r.squared,
                             med_surprise=median(pw$qtr_eps-pw$consensus_mean),
                             beat_cons=100*mean(pw$qtr_eps>pw$consensus_mean))
cat(sprintf("actual~consensus n=%d: alpha=%+.3f SE=%.3f t=%.2f beta=%.3f R2=%.3f median surprise=%+.3f\n",
    nrow(pw), sw[1,1], sw[1,2], sw[1,3], sw[2,1], summary(mw)$r.squared, median(pw$qtr_eps-pw$consensus_mean)))
cat(sprintf("  actuals changed >$1: %d/%d ; (contaminated paper: alpha=+1.55 t=5.88 beta=1.15 R2=0.675)\n",
    sum(abs(wcfull$actual_eps-wcfull$qtr_eps)>1, na.rm=TRUE), nrow(wcfull)))

hdr("PRIMARY 340  -- #3 Four-cell returns RE-ANCHORED  (tab:four_cell)")
pr <- clnP[!is.na(er1) & !is.na(actual_beat) & !is.na(crowd_pred)]
pr[, cell := fcase(crowd_pred & actual_beat, "beat-correct", crowd_pred & !actual_beat, "beat-WRONG",
                   !crowd_pred & !actual_beat, "miss-correct", !crowd_pred & actual_beat, "miss-WRONG")]
fourcell <- pr[, .(n=.N, d1=round(mean(er1)*100,2), t1=round(ts(er1),2),
                   d5=round(mean(er5,na.rm=TRUE)*100,2), t5=round(ts(er5),2),
                   d10=round(mean(er10,na.rm=TRUE)*100,2), t10=round(ts(er10),2)), by=cell][order(-d1)]
print(fourcell); RES$primary$four_cell <- fourcell

hdr("PRIMARY 340  -- #3 Strategy Panel A (long/short/combined) RE-ANCHORED  (tab:strategies A)")
paneA <- ls_table(clnP); print(paneA); RES$primary$panelA <- paneA

hdr("PRIMARY 340  -- #3 Strategy Panel B (short 0.20-0.35) RE-ANCHORED  (tab:strategies B)")
paneB <- short_table(clnP)
print(paneB[, lapply(.SD, function(x) if(is.numeric(x)) round(x,2) else x)]); RES$primary$panelB <- paneB

hdr("PRIMARY 340  -- #3 FF3 alpha RE-ANCHORED (pooled event-day, horizon 10)  (tab:ff3)")
RES$primary$ff3 <- list()
for(th in c(0.20,0.25,0.30,0.35)){
  f <- ff3_fit(short_day_panel(clnP, taqP, ffP, th)); RES$primary$ff3[[paste0("p",th)]] <- f
  print_ff3(f, sprintf("P<%.2f", th)) }

hdr("PRIMARY 340  -- #3 Linear return prediction (re-anchored er1)  (sec:returns linear)")
lr <- clnP[!is.na(er1)]
lr[, surprise := qtr_eps - consensus_mean]
mlr <- lm(er1 ~ beat_prob_last + flow_imbalance + surprise + consensus_stdev + pre_stock_vol, lr)
slr <- summary(mlr); clr <- slr$coefficients
RES$primary$linret <- list(n=nrow(model.frame(mlr)), r2=slr$r.squared,
   pv_t=clr["pre_stock_vol","t value"], pv_p=clr["pre_stock_vol","Pr(>|t|)"],
   sig_other=rownames(clr)[clr[,4]<0.05 & rownames(clr)!="(Intercept)"])
print(round(clr,4))
cat(sprintf("R2=%.3f n=%d | pre_stock_vol t=%.2f p=%.3f | other p<.05: %s\n", slr$r.squared,
   nrow(model.frame(mlr)), clr["pre_stock_vol","t value"], clr["pre_stock_vol","Pr(>|t|)"],
   paste(setdiff(RES$primary$linret$sig_other,"pre_stock_vol"), collapse=", ")))

# ===========================================================================
# OOS 321 (Dome resolution labels; re-anchored returns)
# ===========================================================================
hdr("OOS 321  -- load + re-anchor")
panN <- readRDS(file.path(B,"build/2026.06.01_event_panel.rds"))
taqN <- readRDS(file.path(B,"build/2026.06.01_equity_daily_returns.rds"))
idxN <- readRDS(file.path(B,"build/2026.06.01_index_daily_returns.rds"))
ffN  <- readRDS(file.path(B,"build/2026.06.01_ff_daily.rds"))
oldev<- readRDS(file.path(B,"build/dome_eps_events.rds"))
naR  <- reanchor(panN, taqN, idxN)                      # actual_beat already = resolved_beat in panel
cat(sprintf("OOS n=%d | shared slugs w/340: %d | re-anchored: %d | look-ahead dropped: %d | post-Feb-18 holdout: %d\n",
    nrow(naR), length(intersect(naR$market_slug, oldev$market_slug)),
    sum(naR$reanchored, na.rm=TRUE), sum(naR$lookahead, na.rm=TRUE), sum(naR$post_cutoff)))
RES$oos$overlap <- list(
  shared_tickers = length(intersect(unique(pan$ticker), unique(naR$ticker))),
  shared_ticker_quarters = length(intersect(unique(pan[!is.na(fpedats), paste(ticker, as.Date(fpedats))]),
                                            unique(naR[!is.na(fpedats), paste(ticker, as.Date(fpedats))]))),
  n_oos = nrow(naR), n_holdout = sum(naR$post_cutoff),
  n_gaap = nrow(naR[eps_type=="gaap"]), n_nongaap = nrow(naR[eps_type=="nongaap"]),
  crowd_acc_full = 100*mean(naR[!is.na(crowd_predicted_beat)&!is.na(actual_beat), crowd_predicted_beat==actual_beat]))
ibN <- readRDS(file.path(B,"build/2026.06.01_ibes_data.rds")); setDT(ibN)
mAg <- merge(naR[,.(market_slug,eps_target,actual_beat)], ibN[,.(market_slug,actual_eps)], by="market_slug")
mAg <- mAg[!is.na(actual_eps) & !is.na(actual_beat)]
RES$oos$overlap$ibes_agree   <- 100*mean(mAg$actual_beat == (mAg$actual_eps > mAg$eps_target))
RES$oos$overlap$ibes_agree_n <- nrow(mAg)
cat(sprintf("OOS overlap: shared tickers=%d, ticker-quarters=%d | GAAP=%d nonGAAP=%d | IBES agree=%.1f%% (n=%d) | crowd acc=%.1f%%\n",
    RES$oos$overlap$shared_tickers, RES$oos$overlap$shared_ticker_quarters, RES$oos$overlap$n_gaap, RES$oos$overlap$n_nongaap,
    RES$oos$overlap$ibes_agree, RES$oos$overlap$ibes_agree_n, RES$oos$overlap$crowd_acc_full))
clnN  <- naR[lookahead == FALSE]
hoN   <- naR[post_cutoff == TRUE]            # full holdout panel (labels)
hoNc  <- clnN[post_cutoff == TRUE]           # holdout return sample

oos_block <- function(p, pret, lab){
  pc <- p[!is.na(beat_prob_last) & !is.na(actual_beat)]
  base <- mean(pc$actual_beat); bc <- mean((pc$beat_prob_last-pc$actual_beat)^2); bn <- mean((base-pc$actual_beat)^2)
  cat(sprintf("\n-- %s  n=%d  base=%.3f --\n", lab, nrow(pc), base))
  cat(sprintf("[#1] Brier crowd=%.3f naive=%.3f BSS=%+.3f\n", bc, bn, 1-bc/bn))
  pc[, bin := cut(beat_prob_last, c(0,.2,.4,.6,.8,1), include.lowest=TRUE, labels=c("0-20","20-40","40-60","60-80","80-100"))]
  cal <- pc[, .(n=.N, mean_p=round(mean(beat_prob_last),3), actual=round(mean(actual_beat),3)), by=bin][order(bin)]
  print(cal)
  pf <- pc[!is.na(flow_imbalance)]
  pf[, fq := cut(flow_imbalance, quantile(flow_imbalance, seq(0,1,.2), na.rm=TRUE), include.lowest=TRUE, labels=paste0("Q",1:5))]
  fcal <- pf[!is.na(fq), .(n=.N, beat=round(mean(actual_beat),3)), by=fq][order(fq)]
  m <- glm(actual_beat ~ flow_imbalance, pf, family=binomial); s <- summary(m)$coefficients
  cat(sprintf("[#2] flow Q1 beat=%.3f Q5 beat=%.3f | logit beta=%.2f z=%.2f p=%.2g\n",
      fcal[1,beat], fcal[.N,beat], s[2,1], s[2,3], s[2,4]))
  list(n=nrow(pc), base=base, brier=bc, naive=bn, bss=1-bc/bn, calibration=cal,
       flow=fcal, flow_logit=list(beta=s[2,1], z=s[2,3], p=s[2,4]))
}
hdr("OOS 321  -- #1/#2 FULL + HOLDOUT")
RES$oos$full    <- oos_block(naR, clnN,  "FULL 321")
RES$oos$holdout <- oos_block(hoN, hoNc, "HOLDOUT post-Feb-18")

hdr("OOS 321  -- #3 Strategy Panel B RE-ANCHORED  (full / holdout)")
sbF <- short_table(clnN); sbH <- short_table(hoNc)
cat("FULL:\n");    print(sbF[, lapply(.SD, function(x) if(is.numeric(x)) round(x,2) else x)])
cat("HOLDOUT:\n"); print(sbH[, lapply(.SD, function(x) if(is.numeric(x)) round(x,2) else x)])
RES$oos$panelB_full <- sbF; RES$oos$panelB_holdout <- sbH

hdr("OOS 321  -- #3 FF3 alpha RE-ANCHORED  (full / holdout)")
RES$oos$ff3_full <- list(); RES$oos$ff3_holdout <- list()
for(th in c(0.20,0.30)){
  fF <- ff3_fit(short_day_panel(clnN, taqN, ffN, th)); RES$oos$ff3_full[[paste0("p",th)]] <- fF
  print_ff3(fF, sprintf("FULL P<%.2f", th))
  fH <- ff3_fit(short_day_panel(hoNc, taqN, ffN, th)); RES$oos$ff3_holdout[[paste0("p",th)]] <- fH
  print_ff3(fH, sprintf("HOLD P<%.2f", th)) }

# ===========================================================================
# POOLED 661 (disjoint primary + OOS) -- power check, re-anchored throughout
# ===========================================================================
hdr("POOLED 661  -- #3 Strategy Panel B RE-ANCHORED")
pool_ret <- rbind(clnP[, .(market_slug, ticker, beat_prob_last, t0, er1, er5, er10)],
                  clnN[, .(market_slug, ticker, beat_prob_last, t0, er1, er5, er10)])
cat(sprintf("pooled return-sample n=%d (primary %d + OOS %d)\n", nrow(pool_ret), nrow(clnP), nrow(clnN)))
sbP <- short_table(pool_ret)
print(sbP[, lapply(.SD, function(x) if(is.numeric(x)) round(x,2) else x)]); RES$pooled$panelB <- sbP
cat("POOLED Panel A (long/short/combined):\n")
paneAP <- ls_table(pool_ret); print(paneAP); RES$pooled$panelA <- paneAP

hdr("POOLED 661  -- #3 FF3 alpha RE-ANCHORED (pool event-day panels)")
RES$pooled$ff3 <- list()
for(th in c(0.20,0.25,0.30,0.35)){
  dpP <- short_day_panel(clnP, taqP, ffP, th)
  dpN <- short_day_panel(clnN, taqN, ffN, th)
  dp  <- rbindlist(list(dpP, dpN), use.names=TRUE)
  f <- ff3_fit(dp); RES$pooled$ff3[[paste0("p",th)]] <- f
  print_ff3(f, sprintf("P<%.2f", th)) }

# ===========================================================================
# CROWD-vs-CONSENSUS DISAGREEMENT  (full + OOS)  [sec:implied reframe]
# ===========================================================================
hdr("DISAGREEMENT  -- crowd implied EPS vs market line (no actual EPS)")
DF <- 4; clampp <- function(p) pmin(pmax(p,.001),.999)
disagree_report <- function(ev, rets, lab){
  ev <- ev[!is.na(implied_eps) & !is.na(eps_target)]
  ev[, disagree := implied_eps - eps_target]
  m <- merge(ev, rets, by="market_slug")
  m[, tb := cut(disagree, quantile(disagree, c(0,1/3,2/3,1), na.rm=TRUE), include.lowest=TRUE,
                labels=c("T1 below line","T2","T3 above line"))]
  terc <- m[!is.na(tb), .(n=.N, mean_disagree=round(mean(disagree),3),
            d1=round(mean(er1,na.rm=TRUE)*100,2), d5=round(mean(er5,na.rm=TRUE)*100,2),
            d10=round(mean(er10,na.rm=TRUE)*100,2)), by=tb][order(tb)]
  cat(sprintf("\n-- %s n=%d -- median disagree=%+.3f | crowd above line=%.1f%% | materially(|.|>$0.05)=%.1f%%\n",
      lab, nrow(ev), median(ev$disagree), 100*mean(ev$disagree>0), 100*mean(abs(ev$disagree)>0.05)))
  print(terc)
  list(n=nrow(ev), median=median(ev$disagree), pct_above=100*mean(ev$disagree>0),
       pct_material=100*mean(abs(ev$disagree)>0.05), terciles=terc)
}
impl <- fread(file.path(B,"analysis/implied_eps_results.csv")); impl[, implied_eps := implied_eps_t]
RES$disagree$primary <- disagree_report(impl[, .(market_slug, eps_target, implied_eps)],
  clnP[, .(market_slug, er1, er5, er10)], "PRIMARY 340")
# OOS implied EPS from OOS history sigma
make_sigma <- function(hist){ setDT(hist); hist <- hist[!is.na(surprise)]; hist[, fpedats:=as.Date(fpedats)]
  setorder(hist, oftic, accounting_basis, fpedats)
  hist[, sigma := shift(frollapply(surprise, 12, sd, align="right", fill=NA, partial=TRUE), 1), by=.(oftic, accounting_basis)]
  hist }
sigma_for <- function(hist, tk, basis, ed){ s <- hist[oftic==tk & accounting_basis==basis & fpedats<ed & !is.na(sigma)]
  if(!nrow(s)) return(NA_real_); s[.N, sigma] }
histN <- make_sigma(readRDS(file.path(B,"build/2026.06.01_ibes_history.rds")))
evN   <- readRDS(file.path(B,"build/2026.06.01_dome_eps_events.rds"))
evN[, earnings_date := as.Date(earnings_date)]
evN[, sigma := mapply(function(tk,bs,ed) sigma_for(histN,tk,bs,ed), ticker, accounting_basis, earnings_date)]
evN[, implied_eps := ifelse(!is.na(sigma), eps_target + sigma*qt(clampp(beat_prob_last), df=DF), NA_real_)]
RES$disagree$oos <- disagree_report(evN[, .(market_slug, eps_target, implied_eps)],
  clnN[, .(market_slug, er1, er5, er10)], "OOS 321")

# ===========================================================================
# IMPLIED-EPS DOLLAR ACCURACY (clean) -- the null  [HOLD pending Codex]
# ===========================================================================
hdr("IMPLIED-EPS MAE clean vs qtr_eps  (the dollar-accuracy null)")
im <- merge(impl, wc, by="market_slug", all.x=TRUE); im <- im[!is.na(qtr_eps) & !is.na(consensus_mean)]
mae <- function(d, lab){ a <- d[!is.na(implied_eps_t)]
  if(!nrow(a)){ cat(sprintf("  %-10s n=0\n", lab)); return(NULL) }
  mac <- mean(abs(a$consensus_mean-a$qtr_eps)); ma <- mean(abs(a$implied_eps_t-a$qtr_eps))
  cat(sprintf("  %-10s n=%d consensus MAE=%.3f implied MAE=%.3f improvement=%+.1f%%\n", lab, nrow(a), mac, ma, (mac-ma)/mac*100))
  list(n=nrow(a), consensus_mae=mac, implied_mae=ma, improvement_pct=(mac-ma)/mac*100) }
cat(sprintf("accounting_basis values: %s\n", paste(unique(im$accounting_basis), collapse=", ")))
RES$implied$all     <- mae(im, "ALL")
RES$implied$nongaap <- mae(im[accounting_basis=="Non-GAAP"], "Non-GAAP")
RES$implied$gaap    <- mae(im[accounting_basis=="GAAP"], "GAAP")

# ===========================================================================
# WALLETS #4 (22 smart wallets)  -- primary clean + OOS
# ===========================================================================
hdr("WALLETS #4  -- 22 smart wallets, per-(wallet,market) hit rate")
sw   <- readRDS(file.path(B,"analysis/smart_wallet_profiles.rds")); addr <- tolower(sw$addresses)
# primary clean: original trades + clean labels (qtr_eps>eps_target)
trP  <- readRDS(file.path(B,"import/dome_trades_combined.rds")); setDT(trP)
trP[, maker_address := tolower(maker_address)]
oeP  <- readRDS(file.path(B,"build/dome_eps_events.rds"))[, .(market_slug, eps_target)]
labP <- merge(oeP, wc, by="market_slug"); labP <- labP[!is.na(qtr_eps)]; labP[, cb := qtr_eps > eps_target]
trPe <- trP[market_slug %in% labP$market_slug & maker_address %in% addr]
trPe[, bf := fcase(side=="BUY"&bid_type=="yes", dollar_volume, side=="SELL"&bid_type=="no", dollar_volume,
                   side=="BUY"&bid_type=="no", -dollar_volume, side=="SELL"&bid_type=="yes", -dollar_volume)]
wmP <- trPe[, .(net=sum(bf, na.rm=TRUE)), by=.(maker_address, market_slug)]
wmP <- merge(wmP, labP[, .(market_slug, cb)], by="market_slug"); wmP <- wmP[net != 0]
wmP[, correct := (net>0) == cb]
RES$wallets$primary_clean <- list(present=length(intersect(addr, unique(trP$maker_address))),
                                  calls=nrow(wmP), hit=100*mean(wmP$correct))
cat(sprintf("PRIMARY clean: %d/22 wallets present | hit %.1f%% (%d calls)\n",
    length(intersect(addr, unique(trP$maker_address))), 100*mean(wmP$correct), nrow(wmP)))
# smart-wallet share of TOTAL earnings dollar volume (Section 6 microstructure)
etrAll <- trP[market_slug %in% oeP$market_slug]
RES$wallets$vol_share_total <- 100*sum(etrAll[maker_address %in% addr]$dollar_volume, na.rm=TRUE) /
                                    sum(etrAll$dollar_volume, na.rm=TRUE)
cat(sprintf("smart-wallet share of total earnings volume: %.2f%%\n", RES$wallets$vol_share_total))
# OOS: new trades + Dome resolution
trN  <- fread(file.path(B,"build/2026.06.01_dome_trades_earnings.csv"))
res  <- fread(file.path(B,"build/2026.06.01_market_resolution.csv"))
trN[, maker_address := tolower(maker_address)]; trN[, dv := shares/1e6*price]
trN[, bf := fcase(side=="BUY"&bid_type=="yes", dv, side=="SELL"&bid_type=="no", dv,
                  side=="BUY"&bid_type=="no", -dv, side=="SELL"&bid_type=="yes", -dv)]
wmN <- trN[maker_address %in% addr, .(net=sum(bf, na.rm=TRUE)), by=.(maker_address, market_slug)]
wmN <- merge(wmN, res[, .(market_slug, resolved_beat)], by="market_slug")
wmN <- merge(wmN, panN[, .(market_slug, post_cutoff)], by="market_slug")
wmN <- wmN[net != 0 & !is.na(resolved_beat)]; wmN[, correct := (net>0) == resolved_beat]
RES$wallets$oos <- list(present=length(intersect(addr, unique(trN$maker_address))),
   calls=nrow(wmN), hit=100*mean(wmN$correct),
   calls_ho=nrow(wmN[post_cutoff==TRUE]), hit_ho=100*mean(wmN[post_cutoff==TRUE]$correct),
   base=100*mean(unique(wmN[, .(market_slug, resolved_beat)])$resolved_beat))
cat(sprintf("OOS: %d/22 present | hit %.1f%% (%d calls) | holdout %.1f%% (%d) | baseline %.1f%%\n",
    length(intersect(addr, unique(trN$maker_address))), 100*mean(wmN$correct), nrow(wmN),
    100*mean(wmN[post_cutoff==TRUE]$correct), nrow(wmN[post_cutoff==TRUE]),
    100*mean(unique(wmN[, .(market_slug, resolved_beat)])$resolved_beat)))

# ===========================================================================
# AUXILIARY DRAFT NUMBERS (data section, summary table, line verification,
#   flow-predictor outcome variants, return coverage, wallet detail)
# ===========================================================================
hdr("AUX -- data/sample counts")
RES$aux$n_trades_combined <- nrow(trP)
RES$aux$n_markets_orig    <- nrow(pan)
RES$aux$n_ibes_matched    <- nrow(pc)
RES$aux$n_tickers_orig    <- uniqueN(pan$ticker)
RES$aux$n_tickers_matched <- pc[, uniqueN(ticker)]
cat(sprintf("trades=%d | markets=%d | IBES-matched(clean-labelled)=%d | unique tickers orig=%d matched=%d\n",
    nrow(trP), nrow(pan), nrow(pc), uniqueN(pan$ticker), pc[, uniqueN(ticker)]))

hdr("AUX -- line verification (line vs consensus; clean MAE vs actual)")
lv <- paR[!is.na(eps_target) & !is.na(consensus_mean)]
mlv <- lm(eps_target ~ consensus_mean, lv); slv <- summary(mlv)$coefficients
RES$aux$line_verif <- list(n=nrow(lv), slope=slv[2,1], intercept=slv[1,1], r2=summary(mlv)$r.squared,
                           r=cor(lv$eps_target, lv$consensus_mean), median_gap=median(lv$eps_target-lv$consensus_mean))
cat(sprintf("eps_target~consensus n=%d: slope=%.3f intercept=%+.3f R2=%.4f r=%.4f median gap=%.3f\n",
    nrow(lv), slv[2,1], slv[1,1], summary(mlv)$r.squared, cor(lv$eps_target,lv$consensus_mean), median(lv$eps_target-lv$consensus_mean)))
mc <- paR[!is.na(qtr_eps) & !is.na(eps_target) & !is.na(consensus_mean)]
lmae <- mean(abs(mc$eps_target - mc$qtr_eps)); cmae <- mean(abs(mc$consensus_mean - mc$qtr_eps))
ptt0 <- t.test(abs(mc$eps_target-mc$qtr_eps), abs(mc$consensus_mean-mc$qtr_eps), paired=TRUE)
RES$aux$mae_line <- lmae; RES$aux$mae_cons <- cmae
RES$aux$mae_paired_t <- unname(ptt0$statistic); RES$aux$mae_paired_p <- ptt0$p.value
cat(sprintf("clean MAE vs qtr actual (n=%d): line=%.3f consensus=%.3f paired t=%.2f p=%.2f\n",
    nrow(mc), lmae, cmae, ptt0$statistic, ptt0$p.value))

hdr("AUX -- summary stats by accounting basis (clean beat rate)")
sb <- paR[!is.na(qtr_eps) & !is.na(eps_target)]
bystat <- sb[, .(n=.N, tickers=uniqueN(ticker), med_analysts=median(num_analysts,na.rm=TRUE),
                 med_pmvol=round(median(total_pm_volume,na.rm=TRUE)), med_trades=median(n_trades,na.rm=TRUE),
                 med_prob=round(median(beat_prob_last,na.rm=TRUE),2),
                 beat_rate=round(100*mean(qtr_eps>eps_target),1)), by=accounting_basis][order(accounting_basis)]
print(bystat); RES$aux$summary_by_basis <- bystat

hdr("AUX -- flow predictor: outcome variants (line vs consensus) + tercile detail")
pfx <- pc[!is.na(flow_imbalance) & !is.na(consensus_mean)]
m_line <- glm((qtr_eps>eps_target)   ~ flow_imbalance, pfx, family=binomial); s_line <- summary(m_line)$coefficients
m_cons <- glm((qtr_eps>consensus_mean) ~ flow_imbalance, pfx, family=binomial); s_cons <- summary(m_cons)$coefficients
RES$aux$flow_logit_line <- list(beta=s_line[2,1], z=s_line[2,3], p=s_line[2,4])
RES$aux$flow_logit_cons <- list(beta=s_cons[2,1], z=s_cons[2,3], p=s_cons[2,4])
cat(sprintf("logit (beat LINE)~flow: beta=%.3f z=%.2f p=%.2g\n", s_line[2,1], s_line[2,3], s_line[2,4]))
cat(sprintf("logit (beat CONS)~flow: beta=%.3f z=%.2f p=%.2g\n", s_cons[2,1], s_cons[2,3], s_cons[2,4]))
ptd <- pc[!is.na(flow_imbalance)]   # pc already carries re-anchored er1 + lookahead flag
ptd[, ft := cut(flow_imbalance, quantile(flow_imbalance, c(0,1/3,2/3,1), na.rm=TRUE), include.lowest=TRUE, labels=c("T1 bearish","T2","T3 bullish"))]
tdet <- ptd[!is.na(ft), .(n=.N, mean_flow=round(mean(flow_imbalance),3),
   beat_line=round(100*mean(qtr_eps>eps_target),1), beat_cons=round(100*mean(qtr_eps>consensus_mean),1),
   mean_surprise=round(mean(qtr_eps-consensus_mean,na.rm=TRUE),3),
   xs1d=round(mean(er1[lookahead==FALSE],na.rm=TRUE)*100,2)), by=ft][order(ft)]
print(tdet); RES$aux$flow_tercile_detail <- tdet

hdr("AUX -- return coverage (re-anchored, look-ahead dropped)")
RES$aux$ret_cov <- list(n_clean=nrow(clnP), d1=sum(!is.na(clnP$er1)), d5=sum(!is.na(clnP$er5)), d10=sum(!is.na(clnP$er10)))
cat(sprintf("clnP n=%d | er1=%d er5=%d er10=%d\n", nrow(clnP), sum(!is.na(clnP$er1)), sum(!is.na(clnP$er5)), sum(!is.na(clnP$er10))))

hdr("AUX -- wallet detail (primary clean): correct count + disagree-with-price")
RES$wallets$primary_clean$correct <- sum(wmP$correct)
cat(sprintf("primary clean: %d correct of %d calls = %.1f%%\n", sum(wmP$correct), nrow(wmP), 100*mean(wmP$correct)))
sc <- trPe[, .(net=sum(bf,na.rm=TRUE)), by=market_slug]
sc <- merge(sc, labP[, .(market_slug, cb)], by="market_slug"); sc <- sc[net != 0]
sc <- merge(sc, pan[, .(market_slug, beat_prob_last)], by="market_slug")
sc[, smart_beat := net>0][, price_beat := beat_prob_last>0.5][, smart_correct := smart_beat==cb][, price_correct := price_beat==cb]
dis <- sc[smart_beat != price_beat]
RES$wallets$disagree <- list(n=nrow(dis), n_total=nrow(sc), smart_hit=100*mean(dis$smart_correct), price_hit=100*mean(dis$price_correct))
cat(sprintf("smart vs price disagree: %d of %d markets | smart correct %.1f%% | price correct %.1f%%\n",
    nrow(dis), nrow(sc), 100*mean(dis$smart_correct), 100*mean(dis$price_correct)))

# ===========================================================================
saveRDS(RES, file.path(B,"analysis/2026.06.01_paper_numbers.rds"))
hdr("DONE -- saved analysis/2026.06.01_paper_numbers.rds")
