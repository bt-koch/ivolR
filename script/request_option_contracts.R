rm(list=ls()); gc()
devtools::load_all()

path_symbols <- "/Volumes/SanDisk64gb/ivolR"
from <- "2022-01-01"
to <- "2023-06-30"


for (file in list.files(path_symbols, pattern = "option_symbol_.*\\.rds")) {

  # extract ticker
  df <- readRDS(file.path(path_symbols, file))





  # read rds
  # get the tickers from rds

  # create empty list named after extracted ticker

  # loop over tickers
    # request data
    # write to list


}
