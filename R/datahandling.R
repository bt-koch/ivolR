manipulate <- function(file) {

  lst <- readRDS(file)

  data <- lapply(lst, function(x) x$data)
  names(data) <- sub("^(.*?\\d{6}[CP]).*", "\\1", names(data))

  option_chains <- list()

  for (chain_id in unique(names(data))) {

    subset_option_chain <- data[names(data) == chain_id]

    if (length(subset_option_chain) > 1) {
      df_option_chain <- do.call(rbind, subset_option_chain)
      option_chains[[chain_id]] <- df_option_chain
    } else {
      option_chains[[chain_id]] <- df_option_chain[[1]]
    }
  }

  return(option_chains)

}

#' options_csv_to_list
#'
#' Convert the CSV obtained from WRDS to same structure as dataset obtained from
#' ivolatility. Addditionally, add adjusted close price.
#'
#' @param csv_file
#' @param to_ivol_format
#'
#' @return
#' @export
#'
#' @examples
options_csv_to_list <- function(csv_file, to_ivol_format = T, add_stockprice = T) {

  df <- read.csv(csv_file)
  option_identifier <- "symbol"
  stock_identifier <- "ticker"

  if (to_ivol_format) {
    df <- data.frame(
      symbol = df$ticker,
      exchange = df$exchange_d,
      date = df$date,
      `Adjusted close` = NA_real_,
      `option symbol` = df$symbol,
      expiration = df$exdate,
      strike = df$strike_price,
      `Call/Put` = df$cp_flag,
      style = df$exercise_style,
      ask = df$best_offer,
      bid = df$best_bid,
      volume = df$volume,
      `open interest` = df$open_interest,
      Unadjusted = NA_real_,
      check.names = F
    )
    option_identifier <- "option symbol"
    stock_identifier <- "symbol"
  }

  if (add_stockprice) {
    # TODO
    # get stock price, join to df by date
  }

  df$stock_identifier <- df[[stock_identifier]]

  output <- list()

  for (tckr in unique(df[[stock_identifier]])) {

    lst <- list()

    df_tckr_subset <- subset(df, stock_identifier == tckr)
    df_tckr_subset$chain_id <- sub("^(.*?\\d{6}[CP]).*", "\\1", df_tckr_subset[[option_identifier]])

    for (id in unique(df_tckr_subset$chain_id)) {

      temp <- subset(df_tckr_subset, chain_id == id)
      temp$chain_id <- NULL
      temp$stock_identifier <- NULL
      lst[[id]] <- temp
    }

    output[[tckr]] <- lst

  }

  return(output)

}

#' Title
#'
#' Convert a option chain data.frame into format suitable for ipod estimation
#' as well as enrich option chain with information relevant for estimation.
#'
#' Finished data.frame should have following structure:
#' - date
#' - time to maturity
#' - price (bid+ask/2, stock price for strike price=0)
#' - risk free rate
#' - open interest
#' - weight (open interest_i / sum(open interest))
#'
#' @return
#' @export
#'
#' @examples
enrich_optionchains <- function(data.frame) {

  # TODO
  # 1. calculate time to maturity (expiration - date)
  # 2. define price (bid+ask/2 if regular option, stock price for strike price=0)
  # 3. add risk free rate (join by date)
  # 4. calculate weight (open interest_i / sum(open interest))

}


