#' get_bank_info
#'
#' to do
#'
#' @param bank_json a
#' @param bank_name a
#'
#' @return to do
#' @export
get_bank_info <- function(bank_json, bank_name) {

  bank <- subset(bank_json, name == bank_name)

  if (length(bank) > 0) {
    return(list(
      name = bank$name,
      ticker = bank$ticker[[1]],
      region = bank$region[[1]],
      category = bank$category
    ))
  }
}

get_bank_by_ticker <- function(banks, ticker) {

  return(
    banks[sapply(banks$ticker, function(x) ticker %in% x), ]
  )

}

get_exchange_by_ticker <- function(banks, ticker) {

  df <- banks[sapply(banks$ticker, function(x) ticker %in% x), ]

  idx <- which(df$ticker[[1]] == ticker)
  exchange <- df$exchange[[1]][idx]

  # somewhat stupid solution to make following condition work
  # --> following solution is when multimatches of ticker in same region
  # this will past together multimatches for different regions and separates
  # them later, not elegant but works
  if (length(exchange) > 1) exchange <- paste(exchange, collapse = "|")

  if (grepl("\\|", exchange)) {
    exchange <- unlist(strsplit(exchange, "\\|"))
  }

  return(exchange)
}
