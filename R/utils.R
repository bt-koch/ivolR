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
  print("sali")
}
