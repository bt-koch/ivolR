#' base_url
#'
#' Returns base URL of ivolatility API
#'
#' @return Base URL as string
#'
#' @noRd
base_url <- function() "http://restapi.ivolatility.com/"

get_endpoints <- function() {

  url <- "https://redocly.github.io/redoc/?nocors&url=https://restapi.ivolatility.com/api-docs#section/Endpoints-Access"
  # url |>
  #   rvest::read_html() |>
  #   # rvest::html_nodes("#section\\/Endpoints-Access")
  #   rvest::html_nodes("div[id='section/Endpoints-Access']")

}

#' request
#'
#' Request given endpoint using parameters provided by funtion.
#' List of endpoints available here \url{https://redocly.github.io/redoc/?nocors&url=https://restapi.ivolatility.com/api-docs#section/Endpoints-Access}.
#'
#' @param endpoint Endpoint (character)
#' @param symbol Ticker of equity or option (character)
#' @param date Date in format "YYYY-MM-DD" (character)
#' @param from Date in format "YYYY-MM-DD" (character)
#' @param to Date in format "YYYY-MM-DD" (character)
#' @param region Equity region. Options: "USA", "EUROPE", "ASIA", "CANADA", "RUSSIA", "CRYPTO" (character)
#' @param callPut "C" for Call or "P" for Put (character)
#'
#' @return data.frame of API response
#' @export
request <- function(endpoint, symbol=NA_character_, date=NA_character_,
                    from=NA_character_, to=NA_character_, region=NA_character_,
                    callPut=NA_character_) {

  params <- c(symbol, date, from, to, region, callPut)
  names(params) <- c("symbol", "date", "from", "to", "region", "callPut")
  params <- params[!is.na(params)]
  params <- paste(names(params), params, sep = "=", collapse = "&")


  url <- paste0(base_url(), endpoint, "?apiKey=", get_apikey(), "&", params)
  url <- gsub(" ", "%20", url)

  response <- tryCatch(jsonlite::fromJSON(url),
                       error = function(e) return(e),
                       warning = function(w) w)

  # if (class(response) != "data.frame" & !is.na(response)) {
  #   response <- response$data
  # }

  return(response)

}

# request_alltickers <- function(symbol, from, to, callPut=NA_character_) {
#   response <- data.table::data.table()
#   for (r in get_regions()) {
#     cat("\nRetrieve Tickers for Region", r, "\n")
#     timespan <- seq(as.Date(from), as.Date(to), by = "days")
#     i <- 1
#     for (d in as.character(timespan)) {
#       cat(" Request", i, "of", length(timespan), "\r")
#       temp <- request(endpoint = "equities/eod/option-series-on-date",
#                       symbol = symbol,
#                       date = d,
#                       region = r,
#                       callPut = callPut)
#       temp <- data.table::as.data.table(temp)
#       temp <- temp[, region = r]
#       response <- rbind(response, temp, fill = T)
#       response <- unique(response)
#       i <- i+1
#     }
#   }
#   return(response)
# }

# request_alloptions <- function(symbols, from, to) {
#   response <- list()
#   i <- 1
#   for (s in symbols) {
#     cat("Request", i, "of", length(symbols), "\r")
#     temp <- request(endpoint = "equities/eod/single-stock-option",
#                     symbol = s,
#                     from = from,
#                     to = to)
#     response[[s]] <- temp
#     i <- i+1
#   }
#   return(response)
# }

get_regions <- function() return(c("USA", "EUROPE", "ASIA", "CANADA"))
