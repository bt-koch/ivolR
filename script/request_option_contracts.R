rm(list=ls()); gc()
devtools::load_all()

path_symbols <- "/Users/belakoch/Documents/coding/ivolR output/ad hoc cs"
output_path <- file.path(path_symbols, "option contracts")
from <- "2022-01-01"
to <- "2023-06-30"

for (file in list.files(path_symbols, pattern = "option_symbol_.*\\.rds")) {

  # extract ticker
  df <- readRDS(file.path(path_symbols, file))
  if (nrow(df) == 0) next

  stock <- unique(df$stockSymbol)
  if (!length(stock) > 0) {
    warning("check '", file, "'. Something went wrong. Skip.")
  }

  tickers <- df$optionSymbol

  res <- list()
  i <- 1

  for (tckr in tickers) {
    cat(paste0("\n", i, " of ", length(tickers), ": Request ", tckr, "... "))
    req <- request(
      endpoint = "equities/eod/single-stock-option",
      symbol = tckr,
      from = from,
      to = to
    )
    res[[tckr]] <- req
    cat("finished!")
    i <- i+1
  }

  output_name <- paste0("option_contract_", stock)
  if (!is.na(output_path)) {
    saveRDS(res, file = file.path(output_path, paste0(output_name, ".rds")))
  } else {
    assign(output_name, req)
  }

}
