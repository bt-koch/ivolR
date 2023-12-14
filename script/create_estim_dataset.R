rm(list = ls()); gc()
devtools::load_all()

path_in <- "/Users/belakoch/Documents/coding/ivolR output/all banks/option chains"
path_out <- "/Users/belakoch/Documents/coding/ivolR output/all banks/estimation input"

files <- list.files(path_in)
i <- 1

for (f in files) {
  cat("\r", i, "of", length(files))
  data <- readRDS(file.path(path_in, f))
  data <- lapply(data, FUN = enrich_optionchains)
  saveRDS(data, file.path(path_out, f))
  i <- i+1
}
