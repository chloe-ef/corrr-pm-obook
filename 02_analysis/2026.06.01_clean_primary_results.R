# 2026.06.01_clean_primary_results.R
# Corrected primary-sample (original 340) numbers using QTR-clean actuals.
# actual_beat := (qtr_eps > eps_target). Strategy/FF3 are unaffected (no actual
# EPS) and reported elsewhere. Produces the headline tables the SSRN draft cites.
suppressMessages(library(data.table))
B <- path.expand("~/Documents/data/corrr/390_paper")
ts <- function(x){x<-x[!is.na(x)]; mean(x)/(sd(x)/sqrt(length(x)))}

pan <- readRDS(file.path(B,"build/event_panel.rds"))
wc  <- readRDS(file.path(B,"build/2026.06.01_walkdown_clean.rds"))[, .(market_slug, qtr_eps)]
p   <- merge(pan, wc, by="market_slug", all.x=TRUE)
p   <- p[!is.na(qtr_eps) & !is.na(eps_target)]
p[, actual_beat := qtr_eps > eps_target]
p[, crowd_pred  := beat_prob_last > 0.5]
cat("=== PRIMARY 340 (QTR-clean), n =", nrow(p), "===\n")
cat(sprintf("beat rate: %.1f%%  (contaminated paper: 80.8%%)\n", 100*mean(p$actual_beat)))

# 1. Calibration + Brier
pc <- p[!is.na(beat_prob_last)]
base <- mean(pc$actual_beat); bc<-mean((pc$beat_prob_last-pc$actual_beat)^2); bn<-mean((base-pc$actual_beat)^2)
cat(sprintf("\n[Brier] crowd=%.3f naive=%.3f BSS=%+.3f  (paper: 0.124/0.155/+0.20)\n", bc, bn, 1-bc/bn))
pc[, bin:=cut(beat_prob_last,c(0,.2,.4,.6,.8,1),include.lowest=TRUE,labels=c("0-20","20-40","40-60","60-80","80-100"))]
cat("[Calibration]\n"); print(pc[,.(n=.N,mean_p=round(mean(beat_prob_last),3),actual=round(mean(actual_beat),3)),by=bin][order(bin)])

# 2. Flow quintiles + logit
pf <- pc[!is.na(flow_imbalance)]
pf[, fq:=cut(flow_imbalance,quantile(flow_imbalance,seq(0,1,.2),na.rm=TRUE),include.lowest=TRUE,labels=paste0("Q",1:5))]
cat("[Flow quintiles]\n"); print(pf[!is.na(fq),.(n=.N,mean_flow=round(mean(flow_imbalance),3),beat=round(mean(actual_beat),3)),by=fq][order(fq)])
m<-glm(actual_beat~flow_imbalance,pf,family=binomial); s<-summary(m)$coefficients
cat(sprintf("  logit beat~flow: beta=%.3f z=%.2f p=%.2g  (paper z=8.57)\n", s[2,1],s[2,3],s[2,4]))

# 3. Walk-down (clean): actual ~ consensus
pw <- p[!is.na(consensus_mean)]
mw<-lm(qtr_eps~consensus_mean,pw); sw<-summary(mw)$coefficients
cat(sprintf("\n[Walk-down clean] actual~consensus n=%d: alpha=%+.3f (t=%.2f) beta=%.3f R2=%.3f  median surprise=%+.3f\n",
    nrow(pw), sw[1,1], sw[1,3], sw[2,1], summary(mw)$r.squared, median(pw$qtr_eps-pw$consensus_mean)))
cat("  (contaminated paper: alpha=+1.55 t=5.88 beta=1.15 R2=0.675)\n")

# 4. Four-cell returns (clean labels)
pr <- p[!is.na(excess_return_1d)]
pr[, cell:=fcase(crowd_pred&actual_beat,"beat-correct", crowd_pred&!actual_beat,"beat-WRONG",
                 !crowd_pred&!actual_beat,"miss-correct", !crowd_pred&actual_beat,"miss-WRONG")]
cat("\n[Four-cell returns clean %]\n")
print(pr[,.(n=.N,d1=round(mean(excess_return_1d)*100,2),t1=round(ts(excess_return_1d),2),
            d5=round(mean(excess_return_5d,na.rm=T)*100,2),t5=round(ts(excess_return_5d),2)),by=cell][order(-d1)])

# 5. Implied-EPS MAE clean (vs qtr_eps), by basis
impl <- fread(file.path(B,"analysis/implied_eps_results.csv"))
impl <- merge(impl, wc, by="market_slug", all.x=TRUE)
impl <- impl[!is.na(qtr_eps) & !is.na(consensus_mean)]
mae <- function(d){
  cons<-mean(abs(d$consensus_mean-d$qtr_eps))
  a<-d[!is.na(implied_eps_t)]; ma<-mean(abs(a$implied_eps_t-a$qtr_eps)); mac<-mean(abs(a$consensus_mean-a$qtr_eps))
  cat(sprintf("    n=%d  consensus MAE=%.3f  implied(A) MAE=%.3f  improvement=%+.1f%%\n",
      nrow(a), mac, ma, (mac-ma)/mac*100)) }
cat("\n[Implied-EPS MAE clean]  (paper: non-GAAP +2.3%, GAAP -37.6%)\n")
cat("  ALL:\n"); mae(impl)
cat("  Non-GAAP:\n"); mae(impl[accounting_basis=="Non-GAAP"])
cat("  GAAP:\n"); mae(impl[accounting_basis=="GAAP"])

# 6. Waterfall clean (non-GAAP)
hist <- readRDS(file.path(B,"build/ibes_history.rds")); setDT(hist); hist<-hist[!is.na(surprise)]; hist[,fpedats:=as.Date(fpedats)]
wf <- impl[accounting_basis=="Non-GAAP" & !is.na(implied_eps_t)]
wf[, earnings_date:=as.Date(earnings_date)]
wf[, typical_beat := sapply(seq_len(.N), function(i){
  sub<-hist[oftic==ticker[i] & accounting_basis=="Non-GAAP" & fpedats<earnings_date[i]]
  sub<-tail(sub[order(fpedats)],8); if(nrow(sub)>=4) median(sub$surprise) else NA_real_})]
wf<-wf[!is.na(typical_beat)]
mc<-median(wf$consensus_mean); mwd<-median(wf$typical_beat); mim<-median(wf$implied_eps_t); mac<-median(wf$qtr_eps)
cat(sprintf("\n[Waterfall clean non-GAAP n=%d]  (paper: gap $0.53 = bias $0.05 + PM $0.10 + resid $0.38)\n", nrow(wf)))
cat(sprintf("  consensus=$%.2f  +hist bias $%.2f  +PM incr $%.2f  +residual $%.2f  = actual $%.2f   (total gap $%.2f)\n",
    mc, mwd, mim-(mc+mwd), mac-mim, mac, mac-mc))
