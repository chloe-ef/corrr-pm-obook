# 2026.06.01_03c_pull_compustat.R
# Route-2 actuals: pull reported quarterly EPS from Compustat (comp.fundq),
# which on WRDS is current (~May 2026) while the IBES academic file lags to
# 2026-02-19. Compustat reports GAAP EPS (epsfxq diluted / epspxq primary),
# which cleanly serves the GAAP markets; non-GAAP/street actuals are filled
# separately from yfinance. Output: build/2026.06.01_compustat_eps.rds
suppressMessages({library(data.table); library(RPostgres); library(DBI)})
build_dir <- path.expand("~/Documents/data/corrr/390_paper/build")
ev  <- readRDS(file.path(build_dir,"2026.06.01_dome_eps_events.rds"))
tks <- na.omit(unique(ev$ticker))
connect <- function(tries=6){ for(i in seq_len(tries)){ con<-tryCatch(dbConnect(Postgres(),
  host="wrds-pgdata.wharton.upenn.edu",port=9737,dbname="wrds",
  user=Sys.getenv("wrds_username"),password=Sys.getenv("wrds_password"),
  sslmode="require",gssencmode="disable"),error=function(e)NULL); if(!is.null(con))return(con); Sys.sleep(2*i)}; stop("no conn")}
con <- connect()
q <- sprintf("SELECT gvkey, tic, conm, datadate, fyearq, fqtr, rdq, epsfxq, epspxq
   FROM comp.fundq
   WHERE tic IN (%s) AND indfmt='INDL' AND datafmt='STD' AND popsrc='D' AND consol='C'
     AND datadate >= '2023-06-01' AND rdq IS NOT NULL
   ORDER BY tic, datadate", paste0("'",tks,"'",collapse=","))
cs <- as.data.table(dbGetQuery(con, q))
dbDisconnect(con)
cs[, `:=`(datadate=as.Date(datadate), rdq=as.Date(rdq))]
saveRDS(cs, file.path(build_dir,"2026.06.01_compustat_eps.rds"))
cat("Compustat rows:", nrow(cs), " tickers:", uniqueN(cs$tic),
    " rdq range:", as.character(range(cs$rdq,na.rm=TRUE)), "\n")
# coverage vs holdout
ho <- ev[post_cutoff==TRUE]
cov <- sapply(seq_len(nrow(ho)), function(i){
  s <- cs[tic==ho$ticker[i] & abs(as.numeric(rdq-ho$earnings_date[i]))<=5]; nrow(s)>0 })
cat("holdout events with a Compustat report within 5d of earnings_date:", sum(cov), "of", nrow(ho), "\n")
