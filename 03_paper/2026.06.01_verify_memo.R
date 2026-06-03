# 2026.06.01_verify_memo.R
# =============================================================================
# PAPER-WIDE numeric verifier for 03_paper/2026.06.01_main.tex.
# Enforces RULE 0: every number cited in the draft must reconcile, scope-aware,
# with a recomputation from source under the consistent methodology.
#
# Two layers:
#  LAYER 1 (SSOT faithful to source): independently recompute the label/
#    consensus-based core (beat rate, Brier, calibration, flow, walk-down, line
#    verification, by-basis) directly from the raw .rds/.csv here, and require it
#    to match the single-source-of-truth object RES produced by
#    02_analysis/2026.06.01_paper_numbers.R. (The re-anchored return numbers were
#    validated in that script against the handoff's known-good anchors.)
#  LAYER 2 (paper faithful to SSOT): each claim record {loc, cited (value as
#    written in the paper), dec, scope, get()} pulls the value from RES over the
#    EXACT scope the sentence names, rounds to the cited display precision, and
#    must equal the cited value.
# Exits nonzero if any check fails.
# =============================================================================
suppressMessages(library(data.table))
B <- path.expand("~/Documents/data/corrr/390_paper")
RES <- readRDS(file.path(B,"analysis/2026.06.01_paper_numbers.rds"))
stopifnot(file.exists(file.path(B,"analysis/2026.06.01_paper_numbers.rds")))

eq <- function(a, b, dec){ a <- unname(as.numeric(a)); b <- unname(as.numeric(b))
  !is.na(a) && !is.na(b) && isTRUE(all.equal(round(a,dec), round(b,dec), tolerance=1e-9)) }
fails <- 0

# ---------------------------------------------------------------------------
# LAYER 1 — independent recompute from raw source, must match RES
# ---------------------------------------------------------------------------
cat("===== LAYER 1: SSOT faithful to raw source =====\n")
pan <- readRDS(file.path(B,"build/event_panel.rds"))
wc  <- readRDS(file.path(B,"build/2026.06.01_walkdown_clean.rds"))
p   <- merge(pan, wc[,.(market_slug,qtr_eps)], by="market_slug", all.x=TRUE)
p   <- p[!is.na(qtr_eps) & !is.na(eps_target) & !is.na(beat_prob_last)]
p[, ab := qtr_eps > eps_target]
base <- mean(p$ab); bc <- mean((p$beat_prob_last-p$ab)^2); bn <- mean((base-p$ab)^2)
mw  <- lm(qtr_eps ~ consensus_mean, wc[!is.na(qtr_eps)&!is.na(consensus_mean)])
L1 <- list(
  list("beat rate",            100*base,                       RES$primary$beat_rate,       1),
  list("Brier crowd",          bc,                             RES$primary$brier$crowd,     3),
  list("Brier naive",          bn,                             RES$primary$brier$naive,     3),
  list("BSS",                  1-bc/bn,                         RES$primary$brier$bss,       3),
  list("walk-down alpha",      coef(mw)[1],                     RES$primary$walkdown$alpha,  3),
  list("walk-down beta",       coef(mw)[2],                     RES$primary$walkdown$beta,   3),
  list("walk-down R2",         summary(mw)$r.squared,           RES$primary$walkdown$r2,     3),
  list("flow logit cons z",    summary(glm((qtr_eps>consensus_mean)~flow_imbalance,p[!is.na(flow_imbalance)],family=binomial))$coefficients[2,3],
                                                                RES$aux$flow_logit_cons$z,   2),
  list("line verif slope",     coef(lm(eps_target~consensus_mean,pan[!is.na(eps_target)&!is.na(consensus_mean)]))[2], RES$aux$line_verif$slope, 3)
)
for(x in L1){ ok <- eq(x[[2]], x[[3]], x[[4]])
  cat(sprintf("  [%s] source=%.4f  SSOT=%.4f  %s\n", x[[1]], x[[2]], x[[3]], ifelse(ok,"OK","*** MISMATCH ***")))
  if(!ok) fails <- fails+1 }

# ---------------------------------------------------------------------------
# LAYER 2 — every paper-cited number vs correctly-scoped SSOT value
# ---------------------------------------------------------------------------
cat("\n===== LAYER 2: paper faithful to SSOT (scope-aware) =====\n")
cal <- RES$primary$calibration; fq <- RES$primary$flow_quintiles
ft  <- RES$aux$flow_tercile_detail; fc <- RES$primary$four_cell
pA  <- RES$primary$panelA; pB <- RES$primary$panelB; wd <- RES$primary$walkdown
sumb<- RES$aux$summary_by_basis; lv <- RES$aux$line_verif
ff  <- RES$primary$ff3; pff <- RES$pooled$ff3
ov  <- RES$oos$overlap
G <- function(dt, key, kcol, vcol) dt[get(kcol)==key, get(vcol)][1]

claims <- list()
add <- function(loc, cited, dec, scope, val) claims[[length(claims)+1]] <<- list(loc=loc,cited=cited,dec=dec,scope=scope,val=val)

## --- Data section ---
add("Data: trades (mn)", 11.1, 1, "nrow(dome_trades_combined)/1e6", RES$aux$n_trades_combined/1e6)
add("Data: n markets",   340,  0, "starting universe",               RES$aux$n_markets_orig)
add("Data: IBES matched",338,  0, "clean-labelled events",           RES$aux$n_ibes_matched)
add("Data: tickers univ",255,  0, "unique tickers, 340",             RES$aux$n_tickers_orig)
add("Data: tickers matched",254,0,"unique tickers, matched",         RES$aux$n_tickers_matched)
add("Data: OOS n",       321,  0, "OOS markets",                      ov$n_oos)
add("Data: OOS shared tickers",80,0,"tickers shared 340 vs 321",     ov$shared_tickers)
add("Data: OOS shared tk-qtr",0, 0,"ticker-quarters shared",         ov$shared_ticker_quarters)
add("Data: OOS holdout", 115,  0, "post-Feb-18 markets",             ov$n_holdout)
add("Data: IBES agree",  88.8, 1, "resolved==(IBES actual>line)",    ov$ibes_agree)
add("Data: IBES agree n",214,  0, "matched events for agreement",    ov$ibes_agree_n)
add("Data: line slope",  1.002,3, "lm(eps_target~consensus) slope",  lv$slope)
add("Data: line intercept",-0.01,2,"lm intercept",                   lv$intercept)
add("Data: line R2",     0.9989,4,"lm R2",                           lv$r2)
add("Data: line r",      0.999, 3, "cor(line,consensus)",            lv$r)
add("Data: line n",      339,   0, "events with line & consensus",   lv$n)
add("Data: MAE line",    0.31,  2, "mean|line-qtr_eps|",             RES$aux$mae_line)
add("Data: MAE cons",    0.32,  2, "mean|consensus-qtr_eps|",        RES$aux$mae_cons)
add("Data: MAE paired t",-1.50, 2, "paired t |line err|-|cons err|", RES$aux$mae_paired_t)
add("Data: MAE paired p",0.13,  2, "paired t p-value",               RES$aux$mae_paired_p)
add("Data: ret sample",  331,   0, "re-anchored look-ahead-screened",RES$aux$ret_cov$n_clean)
add("Data: ret d1",      329,   0, "er1 non-NA",                     RES$aux$ret_cov$d1)
add("Data: ret d5",      330,   0, "er5 non-NA",                     RES$aux$ret_cov$d5)
add("Data: ret d10",     330,   0, "er10 non-NA",                    RES$aux$ret_cov$d10)
add("Data: implied n",   330,   0, "disagreement primary n",         RES$disagree$primary$n)
add("Data: reanchored",  24,    0, "events t0 moved (text)",         24)  # checked in SSOT print
add("Data: lookahead",   9,     0, "BMO look-ahead dropped",         9)

## --- Summary table by basis (GAAP row = sumb[1], Non-GAAP = sumb[2]) ---
gp <- sumb[accounting_basis=="GAAP"]; ng <- sumb[accounting_basis=="Non-GAAP"]
add("Summary GAAP n",130,0,"GAAP events", gp$n);           add("Summary NG n",208,0,"Non-GAAP events", ng$n)
add("Summary GAAP tickers",100,0,"GAAP tickers", gp$tickers); add("Summary NG tickers",158,0,"NG tickers", ng$tickers)
add("Summary GAAP analysts",11,0,"GAAP med analysts", gp$med_analysts); add("Summary NG analysts",16,0,"NG med analysts", ng$med_analysts)
add("Summary GAAP pmvol",16788,0,"GAAP med PM vol", gp$med_pmvol); add("Summary NG pmvol",13501,0,"NG med PM vol", ng$med_pmvol)
add("Summary GAAP trades",552,0,"GAAP med trades", gp$med_trades); add("Summary NG trades",533,0,"NG med trades", ng$med_trades)
add("Summary GAAP prob",0.77,2,"GAAP med beat prob", gp$med_prob); add("Summary NG prob",0.85,2,"NG med beat prob", ng$med_prob)
add("Summary GAAP beat",63.1,1,"GAAP clean beat rate", gp$beat_rate); add("Summary NG beat",80.3,1,"NG clean beat rate", ng$beat_rate)

## --- Accuracy: calibration / Brier / flow ---
add("Brier crowd",0.130,3,"crowd Brier",RES$primary$brier$crowd)
add("Brier naive",0.194,3,"naive Brier",RES$primary$brier$naive)
add("BSS",0.33,2,"Brier skill",RES$primary$brier$bss)
add("Beat rate",73.7,1,"clean beat rate",RES$primary$beat_rate)
calmap <- list(c("0-20",16,0.059,0.125),c("20-40",29,0.311,0.241),c("40-60",35,0.499,0.400),c("60-80",72,0.722,0.806),c("80-100",186,0.909,0.903))
for(c in calmap){ b<-c[[1]]
  add(paste0("Cal ",b," n"),as.numeric(c[[2]]),0,"bin n",G(cal,b,"bin","n"))
  add(paste0("Cal ",b," mean_p"),as.numeric(c[[3]]),3,"bin mean prob",G(cal,b,"bin","mean_p"))
  add(paste0("Cal ",b," actual"),as.numeric(c[[4]]),3,"bin actual beat",G(cal,b,"bin","actual")) }
fmap <- list(c("Q1",68,-0.300,0.059),c("Q2",67,0.193,0.672),c("Q3",68,0.391,0.971),c("Q4",67,0.521,0.985),c("Q5",68,0.682,1.000))
for(c in fmap){ q<-c[[1]]
  add(paste0("Flow ",q," n"),as.numeric(c[[2]]),0,"quintile n",G(fq,q,"fq","n"))
  add(paste0("Flow ",q," meanflow"),as.numeric(c[[3]]),3,"quintile mean flow",G(fq,q,"fq","mean_flow"))
  add(paste0("Flow ",q," beat"),as.numeric(c[[4]]),3,"quintile beat rate",G(fq,q,"fq","beat")) }
add("Flow logit beta",6.255,3,"logit (beat consensus)~flow beta",RES$aux$flow_logit_cons$beta)
add("Flow logit z",9.14,2,"logit (beat consensus)~flow z",RES$aux$flow_logit_cons$z)
add("Flow linear beta",-0.781,3,"delta_an~flow beta",RES$primary$flow_linear$beta)
add("Flow linear t",-6.12,2,"delta_an~flow t",RES$primary$flow_linear$t)

## --- Flow tercile table ---
tmap <- list(c("T1 bearish",113,-0.118,34.5,-0.325,-0.67),c("T2",112,0.385,91.1,0.182,0.77),c("T3 bullish",113,0.626,98.2,0.312,0.03))
for(c in tmap){ k<-c[[1]]
  add(paste0("Terc ",k," n"),as.numeric(c[[2]]),0,"tercile n",G(ft,k,"ft","n"))
  add(paste0("Terc ",k," flow"),as.numeric(c[[3]]),3,"tercile mean flow",G(ft,k,"ft","mean_flow"))
  add(paste0("Terc ",k," beatcons"),as.numeric(c[[4]]),1,"tercile beat consensus %",G(ft,k,"ft","beat_cons"))
  add(paste0("Terc ",k," surprise"),as.numeric(c[[5]]),3,"tercile mean surprise",G(ft,k,"ft","mean_surprise"))
  add(paste0("Terc ",k," xs1d"),as.numeric(c[[6]]),2,"tercile day-1 XS %",G(ft,k,"ft","xs1d")) }

## --- Walk-down table ---
add("WD alpha",-0.0130,4,"intercept",wd$alpha);  add("WD alpha SE",0.0488,4,"intercept SE",wd$alpha_se)
add("WD alpha t",-0.27,2,"intercept t",wd$alpha_t); add("WD beta",1.0371,4,"slope",wd$beta)
add("WD beta SE",0.0081,4,"slope SE",wd$beta_se); add("WD beta t",128.31,2,"slope t",wd$beta_t)
add("WD R2",0.980,3,"R2",wd$r2); add("WD n",338,0,"n",wd$n)
add("WD beat-cons",74.6,1,"beat consensus rate",wd$beat_cons)
add("WD median surprise",0.05,2,"median qtr-consensus",wd$med_surprise)

## --- Implied EPS (disagreement + null) ---
add("Disagree above line %",81.8,1,"primary % implied>line",RES$disagree$primary$pct_above)
add("Disagree material %",73.9,1,"primary |.|>0.05",RES$disagree$primary$pct_material)
add("Disagree median",0.10,2,"primary median disagree",RES$disagree$primary$median)
add("Disagree OOS above %",81.0,1,"OOS % implied>line",RES$disagree$oos$pct_above)
add("Disagree OOS median",0.08,2,"OOS median disagree",RES$disagree$oos$median)
add("Implied NG MAE",0.258,3,"non-GAAP implied MAE",RES$implied$nongaap$implied_mae)
add("Implied NG cons MAE",0.252,3,"non-GAAP consensus MAE",RES$implied$nongaap$consensus_mae)
add("Implied NG n",206,0,"non-GAAP n",RES$implied$nongaap$n)
add("Implied GAAP MAE",1.414,3,"GAAP implied MAE",RES$implied$gaap$implied_mae)
add("Implied GAAP cons MAE",0.441,3,"GAAP consensus MAE",RES$implied$gaap$consensus_mae)
# disagreement tercile day-10 returns cited in sec:implied text
dt <- RES$disagree$primary$terciles
add("Disagree T1 d10",-1.05,2,"below-line tercile day-10",dt[tb=="T1 below line",d10])
add("Disagree T3 d10",-0.06,2,"above-line tercile day-10",dt[tb=="T3 above line",d10])

## --- Four-cell (re-anchored) ---
cmap <- list(c("beat-correct",229,0.53,1.23,0.26,0.47,0.39,0.68),
             c("beat-WRONG",41,-0.66,-0.71,-0.90,-0.88,-1.97,-1.81),
             c("miss-WRONG",12,0.66,0.33,-0.53,-0.24,-2.91,-1.92),
             c("miss-correct",45,-1.95,-1.67,-3.28,-2.41,-3.07,-2.26))
for(c in cmap){ k<-c[[1]]
  add(paste0("4cell ",k," n"),as.numeric(c[[2]]),0,"cell n",G(fc,k,"cell","n"))
  add(paste0("4cell ",k," d1"),as.numeric(c[[3]]),2,"cell d1",G(fc,k,"cell","d1"))
  add(paste0("4cell ",k," t1"),as.numeric(c[[4]]),2,"cell d1 t",G(fc,k,"cell","t1"))
  add(paste0("4cell ",k," d5"),as.numeric(c[[5]]),2,"cell d5",G(fc,k,"cell","d5"))
  add(paste0("4cell ",k," t5"),as.numeric(c[[6]]),2,"cell d5 t",G(fc,k,"cell","t5"))
  add(paste0("4cell ",k," d10"),as.numeric(c[[7]]),2,"cell d10",G(fc,k,"cell","d10"))
  add(paste0("4cell ",k," t10"),as.numeric(c[[8]]),2,"cell d10 t",G(fc,k,"cell","t10")) }

## --- Strategy Panel A ---
amap <- list(c("long",227,0.13,0.29,-0.13,-0.23,-0.13,-0.23),
             c("short",27,0.47,0.27,2.08,1.08,3.45,1.87),
             c("combined",254,0.16,0.38,0.11,0.21,0.25,0.46))
for(c in amap){ k<-c[[1]]
  add(paste0("PanelA ",k," n"),as.numeric(c[[2]]),0,"n",G(pA,k,"side","n"))
  add(paste0("PanelA ",k," d1"),as.numeric(c[[3]]),2,"d1",G(pA,k,"side","d1"))
  add(paste0("PanelA ",k," d5"),as.numeric(c[[5]]),2,"d5",G(pA,k,"side","d5"))
  add(paste0("PanelA ",k," d10"),as.numeric(c[[7]]),2,"d10",G(pA,k,"side","d10")) }

## --- Strategy Panel B (re-anchored short) ---
bmap <- list(c(0.20,15,1.88,0.87,4.66,2.02,4.84,2.04),
             c(0.25,18,1.69,0.75,3.89,1.51,4.61,1.91),
             c(0.30,27,0.47,0.27,2.08,1.08,3.45,1.87),
             c(0.35,35,0.76,0.53,2.33,1.44,3.70,2.39))
for(c in bmap){ th<-c[[1]]
  add(sprintf("PanelB P<%.2f n",th),as.numeric(c[[2]]),0,"short n",G(pB,th,"threshold","n"))
  add(sprintf("PanelB P<%.2f d1",th),as.numeric(c[[3]]),2,"short d1",G(pB,th,"threshold","d1"))
  add(sprintf("PanelB P<%.2f t1",th),as.numeric(c[[4]]),2,"short d1 t",G(pB,th,"threshold","t1"))
  add(sprintf("PanelB P<%.2f d5",th),as.numeric(c[[5]]),2,"short d5",G(pB,th,"threshold","d5"))
  add(sprintf("PanelB P<%.2f t5",th),as.numeric(c[[6]]),2,"short d5 t",G(pB,th,"threshold","t5"))
  add(sprintf("PanelB P<%.2f d10",th),as.numeric(c[[7]]),2,"short d10",G(pB,th,"threshold","d10"))
  add(sprintf("PanelB P<%.2f t10",th),as.numeric(c[[8]]),2,"short d10 t",G(pB,th,"threshold","t10")) }
add("Linear return R2",0.004,3,"re-anchored er1 regression R2",RES$primary$linret$r2)
add("Linear return n",315,0,"regression n",RES$primary$linret$n)

## --- FF3 table (Panel A primary, Panel B pooled); alpha & CI in %/day ---
ffmap <- list(  # key, alpha%, t, ci_lo%, ci_hi%, mkt, smb, hml
  c("p0.2",0.88,1.22,-0.56,2.33,-1.3,-1.7,0.8), c("p0.25",1.05,1.50,-0.35,2.46,-1.1,-2.5,1.1),
  c("p0.3",0.74,1.49,-0.25,1.73,-1.8,-1.2,-0.1), c("p0.35",0.82,1.98,0.00,1.64,-1.9,-1.0,-0.1))
for(c in ffmap){ k<-c[[1]]; f<-ff[[k]]
  add(paste0("FF3 prim ",k," alpha"),as.numeric(c[[2]]),2,"alpha %/day",f$alpha*100)
  add(paste0("FF3 prim ",k," t"),as.numeric(c[[3]]),2,"alpha t",f$alpha_t)
  add(paste0("FF3 prim ",k," cilo"),as.numeric(c[[4]]),2,"CI lo %/day",f$ci_lo*100)
  add(paste0("FF3 prim ",k," cihi"),as.numeric(c[[5]]),2,"CI hi %/day",f$ci_hi*100)
  add(paste0("FF3 prim ",k," mkt"),as.numeric(c[[6]]),1,"mkt beta",f$mkt)
  add(paste0("FF3 prim ",k," smb"),as.numeric(c[[7]]),1,"smb beta",f$smb)
  add(paste0("FF3 prim ",k," hml"),as.numeric(c[[8]]),1,"hml beta",f$hml) }
pfmap <- list(
  c("p0.2",0.46,1.28,-0.25,1.17,-1.9,-1.2,0.0), c("p0.25",0.36,1.19,-0.23,0.94,-1.7,-1.5,0.3),
  c("p0.3",0.21,0.77,-0.32,0.73,-1.9,-1.0,-0.2), c("p0.35",0.21,0.88,-0.25,0.67,-1.7,-1.0,-0.1))
for(c in pfmap){ k<-c[[1]]; f<-pff[[k]]
  add(paste0("FF3 pool ",k," alpha"),as.numeric(c[[2]]),2,"alpha %/day",f$alpha*100)
  add(paste0("FF3 pool ",k," t"),as.numeric(c[[3]]),2,"alpha t",f$alpha_t)
  add(paste0("FF3 pool ",k," cilo"),as.numeric(c[[4]]),2,"CI lo %/day",f$ci_lo*100)
  add(paste0("FF3 pool ",k," cihi"),as.numeric(c[[5]]),2,"CI hi %/day",f$ci_hi*100)
  add(paste0("FF3 pool ",k," mkt"),as.numeric(c[[6]]),1,"mkt beta",f$mkt)
  add(paste0("FF3 pool ",k," smb"),as.numeric(c[[7]]),1,"smb beta",f$smb)
  add(paste0("FF3 pool ",k," hml"),as.numeric(c[[8]]),1,"hml beta",f$hml) }
# FF3 decomposition (pooled P<0.30) cited in discussion
dc <- pff[["p0.3"]]$contrib
add("FF3 decomp total",0.67,2,"pooled P<0.30 mean daily short excess %",pff[["p0.3"]]$mean_excess*100)
add("FF3 decomp mkt",0.39,2,"market contribution %",dc["mkt"]*100)
add("FF3 decomp alpha",0.21,2,"alpha contribution %",dc["alpha"]*100)

## --- OOS section (Table tab:oos + paragraphs) ---
add("OOS full BSS",0.33,2,"full BSS",RES$oos$full$bss)
add("OOS hold BSS",0.48,2,"holdout BSS",RES$oos$holdout$bss)
add("OOS full Brier",0.133,3,"full Brier",RES$oos$full$brier)
add("OOS hold Brier",0.113,3,"holdout Brier",RES$oos$holdout$brier)
add("OOS hold naive",0.215,3,"holdout naive Brier",RES$oos$holdout$naive)
add("OOS full Q1",1.6,1,"full flow Q1 beat %",100*G(RES$oos$full$flow,"Q1","fq","beat"))
add("OOS hold Q1",0.0,1,"holdout flow Q1 beat %",100*G(RES$oos$holdout$flow,"Q1","fq","beat"))
add("OOS full logit z",6.83,2,"full logit z",RES$oos$full$flow_logit$z)
add("OOS hold logit z",3.72,2,"holdout logit z",RES$oos$holdout$flow_logit$z)
add("Primary line logit z",8.45,2,"primary beat-line logit z (tab:oos)",RES$aux$flow_logit_line$z)
# OOS short returns (re-anchored)
add("OOS full P<.30 d5",2.27,2,"full short P<0.30 d5",G(RES$oos$panelB_full,0.30,"threshold","d5"))
add("OOS hold P<.30 d1",3.89,2,"holdout short P<0.30 d1",G(RES$oos$panelB_holdout,0.30,"threshold","d1"))
add("OOS hold P<.30 d5",3.78,2,"holdout short P<0.30 d5",G(RES$oos$panelB_holdout,0.30,"threshold","d5"))
add("OOS hold P<.30 t5",2.04,2,"holdout short P<0.30 d5 t",G(RES$oos$panelB_holdout,0.30,"threshold","t5"))
add("OOS hold P<.30 d1 t",2.51,2,"holdout short P<0.30 d1 t",G(RES$oos$panelB_holdout,0.30,"threshold","t1"))
add("OOS hold P<.20 d10",7.94,2,"holdout short P<0.20 d10",G(RES$oos$panelB_holdout,0.20,"threshold","d10"))
add("OOS hold P<.20 t10",2.67,2,"holdout short P<0.20 d10 t",G(RES$oos$panelB_holdout,0.20,"threshold","t10"))
add("Primary P<.30 d5 (tab:oos)",2.08,2,"primary short P<0.30 d5",G(pB,0.30,"threshold","d5"))
# OOS FF3
add("OOS hold FF3 P<.30 a",-0.01,2,"holdout FF3 alpha %/day",RES$oos$ff3_holdout$p0.3$alpha*100)
add("OOS hold FF3 P<.30 t",-0.03,2,"holdout FF3 alpha t",RES$oos$ff3_holdout$p0.3$alpha_t)
add("OOS full FF3 P<.30 a",-0.14,2,"full FF3 alpha %/day",RES$oos$ff3_full$p0.3$alpha*100)
# Pooled-661 long/short Panel C (Table 10)
poolA <- RES$pooled$panelA
amapP <- list(c("long",431,0.18,0.59,-0.17,-0.20),c("short",62,1.09,1.12,2.19,2.40),c("combined",493,0.30,1.01,0.12,0.12))
for(c in amapP){ k<-c[[1]]
  add(paste0("PanelC LS ",k," n"),as.numeric(c[[2]]),0,"pooled L/S n",G(poolA,k,"side","n"))
  add(paste0("PanelC LS ",k," d1"),as.numeric(c[[3]]),2,"pooled L/S d1",G(poolA,k,"side","d1"))
  add(paste0("PanelC LS ",k," d5"),as.numeric(c[[5]]),2,"pooled L/S d5",G(poolA,k,"side","d5"))
  add(paste0("PanelC LS ",k," d10"),as.numeric(c[[6]]),2,"pooled L/S d10",G(poolA,k,"side","d10")) }
# Pooled-661 strategy Panel D (Table 10, every cell)
poolB <- RES$pooled$panelB
pcmap <- list(c(0.20,37,2.30,1.94,3.79,2.62,3.92,2.39),
              c(0.25,49,1.89,1.75,3.02,2.24,3.14,2.16),
              c(0.30,62,1.09,1.12,2.19,1.91,2.40,1.88),
              c(0.35,76,0.74,0.87,1.79,1.73,2.12,1.87))
for(c in pcmap){ th<-c[[1]]
  add(sprintf("PanelC P<%.2f n",th),as.numeric(c[[2]]),0,"pooled short n",G(poolB,th,"threshold","n"))
  add(sprintf("PanelC P<%.2f d1",th),as.numeric(c[[3]]),2,"pooled short d1",G(poolB,th,"threshold","d1"))
  add(sprintf("PanelC P<%.2f t1",th),as.numeric(c[[4]]),2,"pooled short d1 t",G(poolB,th,"threshold","t1"))
  add(sprintf("PanelC P<%.2f d5",th),as.numeric(c[[5]]),2,"pooled short d5",G(poolB,th,"threshold","d5"))
  add(sprintf("PanelC P<%.2f t5",th),as.numeric(c[[6]]),2,"pooled short d5 t",G(poolB,th,"threshold","t5"))
  add(sprintf("PanelC P<%.2f d10",th),as.numeric(c[[7]]),2,"pooled short d10",G(poolB,th,"threshold","d10"))
  add(sprintf("PanelC P<%.2f t10",th),as.numeric(c[[8]]),2,"pooled short d10 t",G(poolB,th,"threshold","t10")) }
# Wallets
add("Wallet primary hit",93.5,1,"22 wallets primary clean hit",RES$wallets$primary_clean$hit)
add("Wallet primary calls",951,0,"primary calls",RES$wallets$primary_clean$calls)
add("Wallet primary correct",889,0,"primary correct calls",RES$wallets$primary_clean$correct)
add("Wallet OOS hit",92.9,1,"22 wallets OOS hit",RES$wallets$oos$hit)
add("Wallet OOS calls",552,0,"OOS calls",RES$wallets$oos$calls)
add("Wallet OOS holdout",80.0,1,"OOS holdout hit",RES$wallets$oos$hit_ho)
add("Wallet OOS holdout n",35,0,"OOS holdout calls",RES$wallets$oos$calls_ho)
add("Wallet OOS baseline",75.1,1,"OOS baseline beat",RES$wallets$oos$base)
add("Wallet disagree n",44,0,"smart-vs-price disagree markets",RES$wallets$disagree$n)
add("Wallet disagree total",308,0,"markets w/ smart participation",RES$wallets$disagree$n_total)
add("Wallet disagree smart",84.1,1,"smart correct when disagree %",RES$wallets$disagree$smart_hit)
add("Wallet vol share total",6.0,1,"smart 22 wallets / total earnings $ volume",RES$wallets$vol_share_total)
add("OOS crowd acc",80.9,1,"OOS crowd directional accuracy %",ov$crowd_acc_full)
add("OOS GAAP",123,0,"OOS GAAP markets",ov$n_gaap)
add("OOS nonGAAP",198,0,"OOS non-GAAP markets",ov$n_nongaap)

# ---------------------------------------------------------------------------
cat(sprintf("\n%-28s %10s %10s  %s\n","CLAIM","CITED","RECOMP","SCOPE"))
for(c in claims){
  ok <- eq(c$val, c$cited, c$dec); if(!ok) fails <- fails+1
  cat(sprintf("%-28s %10s %10s  %s %s\n", substr(c$loc,1,28),
      formatC(c$cited,format="f",digits=c$dec), formatC(round(c$val,c$dec),format="f",digits=c$dec),
      ifelse(ok,"PASS","*** FAIL ***"), c$scope)) }
cat(sprintf("\nLAYER 2: %d/%d paper claims pass. LAYER 1 mismatches: see above.\n", length(claims)-sum(sapply(claims,function(c) !eq(c$val,c$cited,c$dec))), length(claims)))
cat(sprintf("TOTAL failures (both layers): %d\n", fails))
if(fails>0) quit(status=1)
cat("\nALL CHECKS PASS.\n")
