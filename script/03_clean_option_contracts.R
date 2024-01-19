rm(list=ls()); gc()
devtools::load_all()

banks <- jsonlite::fromJSON("input/banks.json")
path_in <- "/Users/belakoch/Documents/coding/ivolR output/all banks/all option contracts"
path_out <- "/Users/belakoch/Documents/coding/ivolR output/all banks/clean option contracts"
i <- 1
n <- length(list.files(path_in))

for (file in list.files(path_in)) {

  cat("\r", i, "of", n)

  data <- readRDS(file.path(path_in, file))

  ticker <- lapply(data, function(x) x$data$stockSymbol)
  if (is.null(unlist(ticker))) {
    ticker <- lapply(data, function(x) x$data$symbol)
  }
  ticker <- unique(unlist(ticker))

  # problem for UBS
  if (file == "option_contract_UBS.rds") {
    ticker <- ticker[!ticker %in% c("CS", "UBSG")]
  }
  stopifnot(length(ticker) == 1)

  exchange <- get_exchange_by_ticker(banks, ticker)

  # test
  # data[[1]]$data$exchange <- "test"

  data <- lapply(data, function(x) {
    if (all(x$data$exchange %in% exchange)) return(x)
  })

  data <- Filter(Negate(is.null), data)

  saveRDS(data, file.path(path_out, file))

  i <- i+1

}
