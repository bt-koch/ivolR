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

get_bank_by_ticker <- function(bank_json, ticker) {

  return(
    banks[sapply(banks$ticker, function(x) ticker %in% x), ]
  )

}
