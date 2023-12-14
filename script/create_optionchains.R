rm(list=ls()); gc()
devtools::load_all()

banks <- jsonlite::fromJSON("input/banks.json")

path_in <- "/Users/belakoch/Documents/coding/ivolR output/all banks/option contracts"
path_out <- "/Users/belakoch/Documents/coding/ivolR output/all banks/option chains"
i <- 1

for (f in list.files(path_in)) {
  cat("\r", i, "of", length(list.files(path_in)))
  data <- create_optionchain(file.path(path_in, f))
  ticker <- substr(f, 17, nchar(f)-4)
  saveRDS(data, file = file.path(path_out, paste0(ticker, ".rds")))
  i <- i+1
}
