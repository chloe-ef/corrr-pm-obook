# 2026.06.01_crowd_vs_consensus.R
# Crowd-vs-street disagreement = implied_eps - line, for ALL events (340 + 321).
# Needs only: line (slug), beat_prob (price), sigma (IBES *history*, pre-2026,
# fully loaded). No realized actual EPS required. Then sort by disagreement vs
# post-earnings returns to test whether the crowd's dollar-level disagreement
# with the street predicts the stock move.
suppressMessages(library(data.table))
B <- path.expand("~/Documents/data/corrr/390_paper")
DF <- 4

# ---- helper: rolling 12-qtr surprise sigma, shifted 1, per (oftic, basis) ----
make_sigma <- function(hist){
  setDT(hist); hist <- hist[!is.na(surprise)]; hist[, fpedats := as.Date(fpedats)]
  setorder(hist, oftic, accounting_basis, fpedats)
  hist[, sigma := shift(frollapply(surprise, 12, sd, align="right", fill=NA,
        partial=TRUE), 1), by=.(oftic, accounting_basis)]
  hist
}
sigma_for <- function(hist, tk, basis, ed){
  s <- hist[oftic==tk & accounting_basis==basis & fpedats < ed & !is.na(sigma)]
  if(!nrow(s)) return(NA_real_); s[.N, sigma]
}
clamp <- function(p) pmin(pmax(p, .001), .999)

disagreement_report <- function(ev, returns, label){
  ev <- ev[!is.na(implied_eps) & !is.na(eps_target)]
  ev[, disagree := implied_eps - eps_target]           # crowd dollar EPS minus street line
  cat(sprintf("\n===== %s  (n=%d) =====\n", label, nrow(ev)))
  cat(sprintf("disagreement (implied - line):  mean=%+.3f  median=%+.3f  sd=%.3f  mean|.|=%.3f\n",
      mean(ev$disagree), median(ev$disagree), sd(ev$disagree), mean(abs(ev$disagree))))
  cat(sprintf("crowd prices EPS ABOVE the street line: %.1f%% of events; materially (|disagree|>$0.05): %.1f%%\n",
      100*mean(ev$disagree>0), 100*mean(abs(ev$disagree)>0.05)))
  m <- merge(ev, returns, by="market_slug")
  m[, tb := cut(disagree, quantile(disagree, c(0,1/3,2/3,1), na.rm=TRUE),
                include.lowest=TRUE, labels=c("T1 below line","T2","T3 above line"))]
  cat("disagreement tercile vs SPY-excess returns (no actual EPS used):\n")
  print(m[!is.na(tb), .(n=.N, mean_disagree=round(mean(disagree),3),
      d1=round(mean(excess_return_1d,na.rm=T)*100,2),
      d5=round(mean(excess_return_5d,na.rm=T)*100,2),
      d10=round(mean(excess_return_10d,na.rm=T)*100,2)), by=tb][order(tb)])
}

# ---- ORIGINAL 340: implied_eps already computed in implied_eps_results.csv ----
impl <- fread(file.path(B,"analysis/implied_eps_results.csv"))
impl[, implied_eps := implied_eps_t]
pan  <- readRDS(file.path(B,"build/event_panel.rds"))
disagreement_report(impl[, .(market_slug, eps_target, implied_eps)],
                    pan[, .(market_slug, excess_return_1d, excess_return_5d, excess_return_10d)],
                    "ORIGINAL 340")

# ---- OOS 321: compute implied_eps from OOS history sigma ----
hist <- make_sigma(readRDS(file.path(B,"build/2026.06.01_ibes_history.rds")))
ev   <- readRDS(file.path(B,"build/2026.06.01_dome_eps_events.rds"))
ev[, sigma := mapply(function(tk,bs,ed) sigma_for(hist,tk,bs,ed),
                     ticker, accounting_basis, earnings_date)]
ev[, implied_eps := ifelse(!is.na(sigma),
     eps_target + sigma*qt(clamp(beat_prob_last), df=DF), NA_real_)]
cat(sprintf("\nOOS events with sigma (=> implied EPS): %d of %d\n", sum(!is.na(ev$implied_eps)), nrow(ev)))
pan2 <- readRDS(file.path(B,"build/2026.06.01_event_panel.rds"))
disagreement_report(ev[, .(market_slug, eps_target, implied_eps)],
                    pan2[, .(market_slug, excess_return_1d, excess_return_5d, excess_return_10d)],
                    "OOS 321")
disagreement_report(merge(ev[, .(market_slug, eps_target, implied_eps, post_cutoff)],
                          data.table(market_slug=ev$market_slug), by="market_slug")[
                          ev[,.(market_slug,post_cutoff)], on="market_slug"][post_cutoff==TRUE],
                    pan2[, .(market_slug, excess_return_1d, excess_return_5d, excess_return_10d)],
                    "OOS post-Feb-18 holdout")
