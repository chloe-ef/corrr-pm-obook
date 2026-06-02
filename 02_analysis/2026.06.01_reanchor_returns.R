# 2026.06.01_reanchor_returns.R
# Tighten the return/look-ahead handling on the primary 340:
#  (1) anchor the post-earnings return window on the actual IBES announcement
#      date (anndats), not the slug earnings_date (they differ for ~24 events);
#  (2) drop BMO events whose last prediction-market trade is not strictly before
#      the announcement (signal look-ahead). Then re-run the short/LS table.
suppressMessages(library(data.table))
B  <- path.expand("~/Documents/data/corrr/390_paper")
p  <- readRDS(file.path(B,"build/event_panel.rds"))
taq<- readRDS(file.path(B,"build/equity_daily_returns.rds")); taq[,date:=as.Date(date)]
idx<- readRDS(file.path(B,"build/index_daily_returns.rds")); idx[,date:=as.Date(date)]
rc <- intersect(c("stock_return","ret"),names(taq))[1]
ic <- intersect(c("sp500_ret","ret"),names(idx))[1]
td <- sort(unique(taq$date))
p[,`:=`(announcement_date=as.Date(announcement_date), earnings_date=as.Date(earnings_date),
        last_prob_date=as.Date(last_prob_date))]
ts <- function(x){x<-x[!is.na(x)]; mean(x)/(sd(x)/sqrt(length(x)))}
win<- function(x){q<-quantile(x,c(.02,.98),na.rm=TRUE); pmin(pmax(x,q[1]),q[2])}

# anchor on the actual announcement date (fallback to slug if anndats missing)
p[, anchor := fifelse(!is.na(announcement_date), announcement_date, earnings_date)]
t0f <- function(d){pos<-findInterval(d,td); tt<-pos+1L; if(is.na(tt)||tt>length(td)) return(as.Date(NA)); td[tt]}
p[, t0 := as.Date(sapply(anchor, function(d) as.numeric(t0f(d))), origin="1970-01-01")]
cumret <- function(tk,d0,h,dt,col){ if(is.na(d0))return(NA_real_); pos<-which(td==d0); if(!length(pos))return(NA_real_)
  days<-td[pos:min(pos+h-1,length(td))]; r<- if(is.null(tk)) dt[date%in%days,get(col)] else dt[ticker==tk&date%in%days,get(col)]
  if(!length(r))return(NA_real_); prod(1+r,na.rm=TRUE)-1 }
for(cc in c("cs1","ci1","cs5","ci5","cs10","ci10")) p[,(cc):=NA_real_]
for(i in seq_len(nrow(p))){ tk<-p$ticker[i]; d0<-p$t0[i]
  set(p,i,"cs1",cumret(tk,d0,1,taq,rc));  set(p,i,"ci1",cumret(NULL,d0,1,idx,ic))
  set(p,i,"cs5",cumret(tk,d0,5,taq,rc));  set(p,i,"ci5",cumret(NULL,d0,5,idx,ic))
  set(p,i,"cs10",cumret(tk,d0,10,taq,rc));set(p,i,"ci10",cumret(NULL,d0,10,idx,ic)) }
p[, er1:=cs1-ci1][, er5:=cs5-ci5][, er10:=cs10-ci10]
for(cc in c("er1","er5","er10")) p[!is.na(get(cc)), (cc):=win(get(cc))]

# look-ahead drop: BMO (anndats earlier than slug) with signal not strictly pre-announcement
p[, bmo := !is.na(announcement_date) & announcement_date < earnings_date]
p[, lookahead := bmo & (is.na(last_prob_date) | last_prob_date >= announcement_date)]
p[, reanchored := !is.na(t0) & t0 != as.Date(sapply(earnings_date, function(d) as.numeric(t0f(d))), origin="1970-01-01")]
cat("events re-anchored (t0 moved):", sum(p$reanchored,na.rm=TRUE), " | look-ahead events dropped:", sum(p$lookahead), "\n")
print(p[lookahead==TRUE, .(market_slug, announcement_date, earnings_date, last_prob_date, beat_prob_last=round(beat_prob_last,3))])

clean <- p[lookahead==FALSE]
strat <- function(d, thr, col) mean(-d[beat_prob_last<thr & !is.na(get(col)), get(col)])*100
strt  <- function(d, thr, col) ts(-d[beat_prob_last<thr & !is.na(get(col)), get(col)])
cat("\n=== CLEAN short table (re-anchored on anndats + look-ahead dropped), primary 340 ===\n")
cat("    [paper: P<0.30 d1 +1.23 / d5 +3.87 / d10 +5.90 (t2.88)]\n")
for(thr in c(0.20,0.25,0.30,0.35)){
  s<-clean[!is.na(beat_prob_last)&beat_prob_last<thr&!is.na(er1)]
  cat(sprintf("  P<%.2f n=%d  d1=%+.2f%%(t%.2f)  d5=%+.2f%%(t%.2f)  d10=%+.2f%%(t%.2f)\n", thr, nrow(s),
    strat(s,thr,"er1"),strt(s,thr,"er1"), strat(s,thr,"er5"),strt(s,thr,"er5"), strat(s,thr,"er10"),strt(s,thr,"er10")))
}
saveRDS(p[, .(market_slug, ticker, anchor, t0, er1, er5, er10, beat_prob_last, bmo, lookahead, reanchored)],
        file.path(B,"build/2026.06.01_reanchored_returns.rds"))
cat("\nSaved 2026.06.01_reanchored_returns.rds\n")