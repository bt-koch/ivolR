rm(list=ls()); gc()
devtools::load_all()

path_symbols <- "/Volumes/SanDisk64gb/ivolR"
output_path <- file.path(path_symbols, "option contracts")
from <- "2022-01-01"
to <- "2023-06-30"



for (file in list.files(path_symbols, pattern = "option_symbol_.*\\.rds")[1:2]) {

  # extract ticker
  df <- readRDS(file.path(path_symbols, file))
  if (nrow(df) == 0) next

  stock <- unique(df$stockSymbol)
  if (!length(stock) > 0) {
    warning("check '", file, "'. Something went wrong. Skip.")
  }

  tickers <- df$optionSymbol

  res <- list()

  for (tckr in tickers[1:2]) {
    cat(paste0("\nRequest ", tckr, "... "))
    req <- request(
      endpoint = "equities/eod/single-stock-option",
      symbol = tckr,
      from = from,
      to = to
    )
    res[[tckr]] <- req
    cat("finished!")
  }

  output_name <- paste0("option_contract_", stock)
  if (!is.na(output_path)) {
    saveRDS(res, file = file.path(output_path, paste0(output_name, ".rds")))
  } else {
    assign(output_name, req)
  }

}
