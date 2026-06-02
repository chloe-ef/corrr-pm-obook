# 2026.06.01_03g_walkdown_clean.R
# Quantify the annual/quarterly contamination in the ORIGINAL walk-down and give
# the corrected estimate. Re-pulls IBES actuals WITH pdicity='QTR' (the one
# filter 03_pull_ibes.R omitted), swaps them into the original ibes_data.rds
# keyed by (ibes_ticker, fpedats, basis), and re-runs actual ~ consensus.
suppressMessages({library(data.table); library(RPostgres); library(DBI)})
B <- path.expand("~/Documents/data/corrr/390_paper/build")
ib <- readRDS(file.path(B,"ibes_data.rds"))      # ORIGINAL matched IBES (contaminated actual_eps)
ib[, fpedats := as.Date(fpedats)]
con<-dbConnect(Postgres(),host="wrds-pgdata.wharton.upenn.edu",port=9737,dbname="wrds",
  user=Sys.getenv("wrds_username"),password=Sys.getenv("wrds_password"),sslmode="require",gssencmode="disable")
tk <- paste0("'", unique(ib$ibes_ticker), "'", collapse=",")
getq <- function(tbl, meas) as.data.table(dbGetQuery(con, sprintf(
  "SELECT ticker, pends AS fpedats, value AS qtr_eps, anndats FROM %s
   WHERE ticker IN (%s) AND measure='%s' AND pdicity='QTR'
     AND pends BETWEEN '2025-01-01' AND '2026-03-31'", tbl, tk, meas)))
qe <- getq("ibes.actu_epsus","EPS");  qe[, basis:="epsus"]
qx <- getq("ibes.actu_xepsus","GPS"); qx[, basis:="xepsus"]
dbDisconnect(con)
q <- rbindlist(list(qe,qx)); q[, fpedats:=as.Date(fpedats)]
# one QTR actual per (ticker,fpedats,basis): earliest announcement
setorder(q, ticker, fpedats, basis, anndats)
q <- q[, .SD[1], by=.(ticker,fpedats,basis)]

ib[, basis := source_table]                       # 'epsus' / 'xepsus'
ib <- merge(ib, q[,.(ibes_ticker=ticker,fpedats,basis,qtr_eps)],
            by=c("ibes_ticker","fpedats","basis"), all.x=TRUE)

cat("events:", nrow(ib), " with QTR actual:", sum(!is.na(ib$qtr_eps)),
    " with (contaminated) actual_eps:", sum(!is.na(ib$actual_eps)), "\n")
ib[, chg := abs(actual_eps - qtr_eps)]
cat("events where QTR actual differs from matched actual_eps by >$1:",
    sum(ib$chg>1, na.rm=TRUE), "(these are the annual-contamination cases)\n\n")

reg <- function(y, lab){ d<-ib[!is.na(get(y)) & !is.na(consensus_mean)]
  m<-lm(d[[y]] ~ d$consensus_mean); s<-summary(m)$coefficients
  cat(sprintf("%-22s n=%d  alpha=%+.3f (t=%.2f)  beta=%.3f  R2=%.3f  median surprise=%+.3f\n",
      lab, nrow(d), s[1,1], s[1,3], s[2,1], summary(m)$r.squared, median(d[[y]]-d$consensus_mean))) }
cat("=== ORIGINAL 338 walk-down: contaminated vs pdicity='QTR'-clean ===\n")
cat("--- paper reports: alpha=+1.547 (t=5.88), beta=1.153, R2=0.675 ---\n")
reg("actual_eps", "CONTAMINATED (paper)")
reg("qtr_eps",    "QTR-CLEAN (corrected)")
saveRDS(ib, file.path(B,"2026.06.01_walkdown_clean.rds"))
