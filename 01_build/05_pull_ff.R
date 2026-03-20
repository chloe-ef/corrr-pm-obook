# 05_pull_ff.R — Download daily Fama-French 3-factor data
# Input:  Kenneth French's data library (public CSV)
# Output: build/ff_daily.rds

library(data.table)

data_root <- Sys.getenv("DATA_DIR", file.path(getwd(), "data"))
build_dir <- file.path(data_root, "build")

cat("=== PULLING FAMA-FRENCH 3-FACTOR DAILY DATA ===\n")

# Download from Kenneth French's website
url <- "https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Research_Data_Factors_daily_CSV.zip"
tmp_zip <- tempfile(fileext = ".zip")
tmp_dir <- tempdir()

download.file(url, tmp_zip, mode = "wb", quiet = TRUE)
unzip(tmp_zip, exdir = tmp_dir)

# Find the CSV file
csv_files <- list.files(tmp_dir, pattern = "F-F_Research_Data_Factors_daily", full.names = TRUE)
csv_file <- csv_files[grepl("\\.CSV$|\\.csv$", csv_files)][1]
cat("Downloaded:", basename(csv_file), "\n")

# Read, skipping the header description rows
# The file has a variable number of header lines; find where data starts
lines <- readLines(csv_file, n = 30)
data_start <- which(grepl("^\\s*\\d{8}", lines))[1]
cat("Data starts at line:", data_start, "\n")

ff <- fread(csv_file, skip = data_start - 1, header = FALSE,
            col.names = c("date_str", "mkt_rf", "smb", "hml", "rf"))

# Remove any footer rows (annual data section, copyright notice)
ff[, date_str := trimws(date_str)]
ff <- ff[grepl("^\\d{8}$", date_str)]

# Parse dates — date_str is YYYYMMDD as character
ff[, date := as.Date(as.character(date_str), format = "%Y%m%d")]

# French data is in percent; convert to decimal
ff[, `:=`(
  mkt_rf = as.numeric(mkt_rf) / 100,
  smb    = as.numeric(smb) / 100,
  hml    = as.numeric(hml) / 100,
  rf     = as.numeric(rf) / 100
)]

ff <- ff[!is.na(date), .(date, mkt_rf, smb, hml, rf)]
setorder(ff, date)

cat("FF daily rows:", format(nrow(ff), big.mark = ","), "\n")
cat("Date range:", as.character(range(ff$date)), "\n")
cat("Sample:\n")
print(tail(ff, 5))

saveRDS(ff, file.path(build_dir, "ff_daily.rds"))
cat("\nSaved ff_daily.rds\n")

# Clean up
unlink(tmp_zip)
