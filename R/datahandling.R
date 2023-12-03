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
