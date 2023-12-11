rm(list = ls()); gc()
devtools::load_all()

output_path <- "/Users/belakoch/Documents/coding/ivolR output/SSM/"  # if NA, keep in Global Environment

from <- "2022-01-01"
to <- "2023-06-30"
banks <- jsonlite::fromJSON("input/banks.json")

# banks <- banks[banks$name %in% c("Silvergate", "Signature Bank"),]
# from <- "2022-02-01"
# to <- "2022-06-30"

banks <- banks[banks$category == "SSM",]
# Banco Bilbao Vizcaya Argentaria, S.A.
banks <- banks[11:nrow(banks),]


for (b in 1:nrow(banks)) {

  bank <- get_bank_info(banks, banks[b,]$name)
  cat("\nRetrieve data for", bank$name)

  stopifnot(length(bank$ticker) == length(bank$region))

  for (i in 1:length(bank$ticker)) {

    df <- data.frame()
    sym <- bank$ticker[i]
    reg <- bank$region[i]

    # to do: skip if NA (e.g. groupe BPCE)
    if (is.na(sym) | is.na(reg)) {
      warning("NA for Bank ")
    }

    count <- 1
    cat("\n")
    for (d in as.character(seq(as.Date(from), as.Date(to), by = "days"))) {
      cat("\r", count, "of", length(seq(as.Date(from), as.Date(to), by = "days")))
      req <- request(
        endpoint = "equities/eod/option-series-on-date",
        symbol = sym,
        region = reg,
        date = d,
        callPut = "C"
      )
      count <- count+1

      if (length(req) > 0) {
        res <- data.frame(
          stockSymbol = req$stockSymbol,
          optionSymbol = req$optionSymbol,
          expirationDate = req$expirationDate,
          strike = req$strike,
          callPut = req$callPut
        )
        df <- rbind(df, res)
        df <- unique(df)
      }
    }

    output_name <- paste0("option_symbol_", sym, ".", reg)
    if (!is.na(output_path)) {
      saveRDS(df, file = file.path(output_path, paste0(output_name, ".rds")))
    } else {
      assign(output_name, df)
    }
  }
}

if (is.na(output_path)) rm(list = ls()[-grep("option_symbol_*", ls())])

