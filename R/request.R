base_url <- function() "http://restapi.ivolatility.com/"

get_endpoints <- function() {

  url <- "https://redocly.github.io/redoc/?nocors&url=https://restapi.ivolatility.com/api-docs#section/Endpoints-Access"
  # url |>
  #   rvest::read_html() |>
  #   # rvest::html_nodes("#section\\/Endpoints-Access")
  #   rvest::html_nodes("div[id='section/Endpoints-Access']")

}

request <- function(endpoint, symbol=NA, date=NA) {

  params <- c(symbol, date)
  names(params) <- c("symbol", "date")
  params <- params[!is.na(params)]
  params <- paste(names(params), params, sep = "=", collapse = "&")


  url <- paste0(base_url(), endpoint, "?apiKey=", get_apikey(), "&", params)

  response <- jsonlite::fromJSON(url)

  if (class(response) != "data.frame") {
    response <- response$data
  }

  return(response)

}
