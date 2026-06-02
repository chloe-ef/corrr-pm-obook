# 2026.06.01_verify_memo.R
# Numeric-claim verifier for 2026.06.01_oos_robustness_memo.md.
# Each claim is a record {id, location, cited (as written in memo), scope, recompute()}.
# recompute() pulls the value from SOURCE data over EXACTLY the scope the memo
# names, rounds to the cited display precision, and must equal the cited value.
# Exits non-zero if any claim fails. (R, because the source data are .rds.)

suppressMessages(library(data.table))
build_dir    <- path.expand("~/Documents/data/corrr/390_paper/build")
analysis_dir <- path.expand("~/Documents/data/corrr/390_paper/analysis")

panel <- readRDS(file.path(build_dir, "2026.06.01_event_panel.rds"))
ev    <- readRDS(file.path(build_dir, "2026.06.01_dome_eps_events.rds"))
oldev <- readRDS(file.path(build_dir, "dome_eps_events.rds"))

# subsets
p_full <- panel[!is.na(beat_prob_last) & !is.na(actual_beat)]
p_ho   <- panel[post_cutoff==TRUE & !is.na(beat_prob_last) & !is.na(actual_beat)]

t_stat <- function(x){ x<-x[!is.na(x)]; mean(x)/(sd(x)/sqrt(length(x))) }
brier_bss <- function(p){ b<-mean(p$actual_beat); c<-mean((p$beat_prob_last-p$actual_beat)^2)
  n<-mean((b-p$actual_beat)^2); list(crowd=c, bss=1-c/n) }
flowQ <- function(p, q){ p<-p[!is.na(flow_imbalance)]
  br<-quantile(p$flow_imbalance, seq(0,1,.2)); p[, fq:=cut(flow_imbalance,br,include.lowest=TRUE,labels=1:5)]
  p[fq==q, mean(actual_beat)] }
shortret <- function(p, thr, col) mean(-p[beat_prob_last<thr & !is.na(get(col)), get(col)])*100
shortt   <- function(p, thr, col) t_stat(-p[beat_prob_last<thr & !is.na(get(col)), get(col)])

# wallet recompute (independent)
wallet <- function(){
  sw<-readRDS(file.path(analysis_dir,"smart_wallet_profiles.rds")); addr<-tolower(sw$addresses)
  tr<-fread(file.path(build_dir,"2026.06.01_dome_trades_earnings.csv"))
  res<-fread(file.path(build_dir,"2026.06.01_market_resolution.csv"))
  tr[,maker_address:=tolower(maker_address)]; tr[,dv:=shares/1e6*price]
  tr[,bf:=fcase(side=="BUY"&bid_type=="yes",dv, side=="SELL"&bid_type=="no",dv,
                side=="BUY"&bid_type=="no",-dv, side=="SELL"&bid_type=="yes",-dv)]
  wm<-tr[maker_address%in%addr,.(net=sum(bf)),by=.(maker_address,market_slug)]
  wm<-merge(wm,res[,.(market_slug,resolved_beat)],by="market_slug")
  wm<-merge(wm,ev[,.(market_slug,post_cutoff)],by="market_slug")
  wm<-wm[net!=0 & !is.na(resolved_beat)]; wm[,correct:=(net>0)==resolved_beat]
  list(present=length(intersect(addr,unique(tr$maker_address))),
       hit_full=100*mean(wm$correct), n_full=nrow(wm),
       hit_ho=100*mean(wm[post_cutoff==TRUE]$correct), n_ho=nrow(wm[post_cutoff==TRUE]),
       base=100*mean(unique(wm[,.(market_slug,resolved_beat)])$resolved_beat)) }
W <- wallet()

# ── extra source loads for FF3 / scaling / IBES-agreement claims ──────────
taq <- readRDS(file.path(build_dir,"2026.06.01_equity_daily_returns.rds")); taq[,date:=as.Date(date)]
ff  <- readRDS(file.path(build_dir,"2026.06.01_ff_daily.rds")); ff[,date:=as.Date(date)]
trd_days <- sort(unique(taq$date))
find_t0 <- function(ed){ pos<-findInterval(ed,trd_days); tgt<-pos+1L
  if(any(is.na(tgt))||tgt>length(trd_days)) return(as.Date(NA)); trd_days[tgt] }
ff3_alpha <- function(p, thr, horizon=10){           # pooled event-day, mirror analysis/08
  se <- p[!is.na(beat_prob_last) & beat_prob_last<thr & !is.na(excess_return_1d)]
  rows <- list()
  for(j in seq_len(nrow(se))){ ev2<-se[j]; t0<-find_t0(ev2$earnings_date); if(is.na(t0)) next
    pos<-which(trd_days==t0); if(!length(pos)) next
    days<-trd_days[pos:min(pos+horizon-1,length(trd_days))]
    sr<-taq[ticker==ev2$ticker & date %in% days, .(date, stock_ret=stock_return)]; if(!nrow(sr)) next
    mg<-merge(sr,ff,by="date"); mg<-mg[!is.na(mkt_rf)&!is.na(stock_ret)]; if(!nrow(mg)) next
    mg[,short_excess:=-(stock_ret)-rf]; rows[[length(rows)+1]]<-mg }
  dp<-rbindlist(rows); ct<-summary(lm(short_excess~mkt_rf+smb+hml,dp))$coefficients
  list(alpha=ct[1,1], t=ct[1,3]) }
A20 <- ff3_alpha(p_ho,0.20); A30 <- ff3_alpha(p_ho,0.30)

ibes_agree <- function(){
  ib<-readRDS(file.path(build_dir,"2026.06.01_ibes_data.rds"))
  m<-merge(ev[,.(market_slug,eps_target,resolved_beat)], ib[,.(market_slug,actual_eps)],by="market_slug")
  m<-m[!is.na(actual_eps)&!is.na(resolved_beat)]; 100*mean(m$resolved_beat==(m$actual_eps>m$eps_target)) }
scaling <- function(){
  tr<-fread(file.path(build_dir,"2026.06.01_dome_trades_earnings.csv"))
  res<-fread(file.path(build_dir,"2026.06.01_market_resolution.csv")); res[,volume_total:=as.numeric(volume_total)]
  comp<-tr[,.(dv=sum(shares/1e6*price)),by=market_slug]; comp<-merge(comp,res[,.(market_slug,volume_total)],by="market_slug")
  comp<-comp[volume_total>0]; list(ratio=median(comp$dv/comp$volume_total), corr=cor(comp$dv,comp$volume_total)) }
SC <- scaling()
calbin <- function(p, lo, hi) p[beat_prob_last>=lo & beat_prob_last<=hi, 100*mean(actual_beat)]

# ── claims: cited value as written in memo, scope, recompute, decimals ──────
claims <- list(
 list(id="n_markets",        loc="Data",    cited=321,   dec=0, scope="new earnings markets after liquidity filter",
      f=function() nrow(ev)),
 list(id="n_holdout",        loc="Data",    cited=115,   dec=0, scope="markets with earnings_date > 2026-02-18",
      f=function() nrow(ev[post_cutoff==TRUE])),
 list(id="overlap_original", loc="Data",    cited=0,     dec=0, scope="slugs shared with original 340",
      f=function() length(intersect(ev$market_slug, oldev$market_slug))),
 list(id="crowd_acc_full",   loc="Data",    cited=80.9,  dec=1, scope="full: crowd_predicted_beat==actual_beat",
      f=function() 100*mean(panel[!is.na(crowd_predicted_beat)&!is.na(actual_beat), crowd_predicted_beat==actual_beat])),
 # Finding 1
 list(id="brier_full",  loc="F1", cited=0.133, dec=3, scope="full: (beat_prob_last - resolved_beat)^2",
      f=function() brier_bss(p_full)$crowd),
 list(id="bss_full",    loc="F1", cited=0.326, dec=3, scope="full Brier skill vs base rate",
      f=function() brier_bss(p_full)$bss),
 list(id="brier_ho",    loc="F1", cited=0.113, dec=3, scope="holdout: (beat_prob_last - resolved_beat)^2",
      f=function() brier_bss(p_ho)$crowd),
 list(id="bss_ho",      loc="F1", cited=0.475, dec=3, scope="holdout Brier skill vs base rate",
      f=function() brier_bss(p_ho)$bss),
 # Finding 2
 list(id="flowQ1_full", loc="F2", cited=0.016, dec=3, scope="full: beat rate in flow quintile 1 (most bearish)",
      f=function() flowQ(copy(p_full),1)),
 list(id="flowQ1_ho",   loc="F2", cited=0.000, dec=3, scope="holdout: beat rate in flow quintile 1",
      f=function() flowQ(copy(p_ho),1)),
 list(id="flowZ_ho",    loc="F2", cited=3.72,  dec=2, scope="holdout logit resolved_beat~flow_imbalance z",
      f=function(){ m<-glm(actual_beat~flow_imbalance, p_ho[!is.na(flow_imbalance)], family=binomial)
                    summary(m)$coefficients["flow_imbalance","z value"] }),
 # Finding 3 — short-only (strategy return = -excess), holdout
 list(id="short20_d1_ho", loc="F3", cited=6.40, dec=2, scope="holdout P<0.20 day-1 strategy return %",
      f=function() shortret(p_ho,0.20,"excess_return_1d")),
 list(id="short20_t1_ho", loc="F3", cited=3.05, dec=2, scope="holdout P<0.20 day-1 t-stat",
      f=function() shortt(p_ho,0.20,"excess_return_1d")),
 list(id="short30_d1_ho", loc="F3", cited=3.94, dec=2, scope="holdout P<0.30 day-1 strategy return %",
      f=function() shortret(p_ho,0.30,"excess_return_1d")),
 list(id="short30_t1_ho", loc="F3", cited=2.51, dec=2, scope="holdout P<0.30 day-1 t-stat",
      f=function() shortt(p_ho,0.30,"excess_return_1d")),
 list(id="short30_d5_ho", loc="F3", cited=3.78, dec=2, scope="holdout P<0.30 day-5 strategy return %",
      f=function() shortret(p_ho,0.30,"excess_return_5d")),
 list(id="short30_t5_ho", loc="F3", cited=2.04, dec=2, scope="holdout P<0.30 day-5 t-stat",
      f=function() shortt(p_ho,0.30,"excess_return_5d")),
 list(id="short30_d10_ho",loc="F3", cited=3.44, dec=2, scope="holdout P<0.30 day-10 strategy return %",
      f=function() shortret(p_ho,0.30,"excess_return_10d")),
 list(id="short30_t10_ho",loc="F3", cited=1.61, dec=2, scope="holdout P<0.30 day-10 t-stat",
      f=function() shortt(p_ho,0.30,"excess_return_10d")),
 list(id="short30_d1_full",loc="F3",cited=1.50, dec=2, scope="full P<0.30 day-1 strategy return %",
      f=function() shortret(p_full,0.30,"excess_return_1d")),
 # Finding 4 — wallets
 list(id="wallet_present", loc="F4", cited=22,   dec=0, scope="original 22 wallets present in new markets",
      f=function() W$present),
 list(id="wallet_full",    loc="F4", cited=92.9, dec=1, scope="22 wallets pooled hit rate on full new sample",
      f=function() W$hit_full),
 list(id="wallet_n_full",  loc="F4", cited=552,  dec=0, scope="market-level calls, full",
      f=function() W$n_full),
 list(id="wallet_ho",      loc="F4", cited=80.0, dec=1, scope="22 wallets hit rate on holdout",
      f=function() W$hit_ho),
 list(id="wallet_base",    loc="F4", cited=75.1, dec=1, scope="baseline beat rate among wallet-traded markets",
      f=function() W$base)
)

fc_d  <- function(p,col){ pr<-p[!is.na(get(col))&!is.na(actual_beat)&!is.na(crowd_predicted_beat)]
  pr[!crowd_predicted_beat & !actual_beat, mean(get(col))*100] }
claims <- c(claims, list(
 list(id="base_full",   loc="Data", cited=72.8, dec=1, scope="full: resolved beat rate",
      f=function() 100*mean(p_full$actual_beat)),
 list(id="base_ho",     loc="Data", cited=68.7, dec=1, scope="holdout: resolved beat rate",
      f=function() 100*mean(p_ho$actual_beat)),
 list(id="n_gaap",      loc="Data", cited=123,  dec=0, scope="GAAP markets",     f=function() nrow(ev[eps_type=="gaap"])),
 list(id="n_nongaap",   loc="Data", cited=198,  dec=0, scope="non-GAAP markets", f=function() nrow(ev[eps_type=="nongaap"])),
 list(id="ret_cov_d1",  loc="Data", cited=319,  dec=0, scope="events with excess_return_1d", f=function() sum(!is.na(panel$excess_return_1d))),
 list(id="ret_cov_d10", loc="Data", cited=318,  dec=0, scope="events with excess_return_10d",f=function() sum(!is.na(panel$excess_return_10d))),
 list(id="scaling_ratio",loc="Data",cited=1.019,dec=3, scope="median (shares/1e6*price)/volume_total",f=function() SC$ratio),
 list(id="scaling_corr", loc="Data",cited=0.996,dec=3, scope="corr(computed vol, volume_total)",f=function() SC$corr),
 list(id="ibes_agree",   loc="Data",cited=88.8, dec=1, scope="resolved_beat == (IBES actual>line) on matched", f=function() ibes_agree()),
 list(id="cal_ho_hi",   loc="F1", cited=93.7, dec=1, scope="holdout 80-100% bin actual beat rate", f=function() p_ho[beat_prob_last>0.8, 100*mean(actual_beat)]),
 list(id="cal_ho_mid",  loc="F1", cited=66.7, dec=1, scope="holdout 60-80% bin actual beat rate", f=function() p_ho[beat_prob_last>0.6 & beat_prob_last<=0.8, 100*mean(actual_beat)]),
 list(id="cal_ho_lo",   loc="F1", cited=0.0,  dec=1, scope="holdout 0-20% bin actual beat rate",  f=function() p_ho[beat_prob_last<=0.2, 100*mean(actual_beat)]),
 list(id="flowZ_full",  loc="F2", cited=6.83, dec=2, scope="full logit resolved_beat~flow z",
      f=function(){ m<-glm(actual_beat~flow_imbalance, p_full[!is.na(flow_imbalance)], family=binomial); summary(m)$coefficients["flow_imbalance","z value"] }),
 list(id="short20_d10_ho", loc="F3", cited=7.94, dec=2, scope="holdout P<0.20 day-10 strategy return %", f=function() shortret(p_ho,0.20,"excess_return_10d")),
 list(id="short20_t10_ho", loc="F3", cited=2.67, dec=2, scope="holdout P<0.20 day-10 t-stat", f=function() shortt(p_ho,0.20,"excess_return_10d")),
 list(id="fc_miss_d1_ho",  loc="F3", cited=-1.58, dec=2, scope="holdout miss-correct cell day-1 mean %", f=function() fc_d(p_ho,"excess_return_1d")),
 list(id="fc_miss_d10_ho", loc="F3", cited=-1.30, dec=2, scope="holdout miss-correct cell day-10 mean %", f=function() fc_d(p_ho,"excess_return_10d")),
 list(id="ff3_p20_a_ho",   loc="F3", cited=0.0056, dec=4, scope="holdout FF3 P<0.20 daily alpha",  f=function() A20$alpha),
 list(id="ff3_p20_t_ho",   loc="F3", cited=0.71,   dec=2, scope="holdout FF3 P<0.20 alpha t-stat", f=function() A20$t),
 list(id="ff3_p30_a_ho",   loc="F3", cited=-0.0001,dec=4, scope="holdout FF3 P<0.30 daily alpha",  f=function() A30$alpha),
 list(id="ff3_p30_t_ho",   loc="F3", cited=-0.03,  dec=2, scope="holdout FF3 P<0.30 alpha t-stat", f=function() A30$t)
))

cat(sprintf("%-16s %-4s %10s %10s  %s\n","CLAIM","LOC","CITED","RECOMP","SCOPE"))
fail <- 0
for(c in claims){
  v <- tryCatch(c$f(), error=function(e){cat("ERROR",c$id,conditionMessage(e),"\n");NA})
  vr <- round(v, c$dec); cited <- round(c$cited, c$dec)
  ok <- !is.na(vr) && isTRUE(all.equal(vr, cited, tolerance=1e-9))
  if(!ok) fail <- fail+1
  cat(sprintf("%-16s %-4s %10s %10s  %s %s\n", c$id, c$loc,
      formatC(cited,format="f",digits=c$dec), formatC(vr,format="f",digits=c$dec),
      ifelse(ok,"PASS","*** FAIL ***"), c$scope))
}
cat(sprintf("\n%d/%d claims pass.\n", length(claims)-fail, length(claims)))
if(fail>0) quit(status=1)
