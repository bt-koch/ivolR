base_url <- function() "http://restapi.ivolatility.com/"

get_endpoints <- function() {

  url <- "https://redocly.github.io/redoc/?nocors&url=https://restapi.ivolatility.com/api-docs#section/Endpoints-Access"
  # url |>
  #   rvest::read_html() |>
  #   # rvest::html_nodes("#section\\/Endpoints-Access")
  #   rvest::html_nodes("div[id='section/Endpoints-Access']")

}

request <- function(endpoint, symbol) {

  filter_symbol <- paste("symbol", symbol, sep = "=") # für hier braucht es elegantere lösung

  filters <- ""
  endpoint <- "equities/eod/option-series-on-date"

  url <- paste0(base_url(), endpoint, "?apiKey=", get_apikey())

}
