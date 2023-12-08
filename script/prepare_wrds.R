rm(list = ls()); gc()
devtools::load_all()

path_out <- "/Users/belakoch/Documents/coding/ivolR output/option chains"

data <- options_csv_to_list("/Users/belakoch/Documents/coding/ivolR output/wrds/jetdbqpn98tcerko.csv")

for (d in names(data)) {
  saveRDS(data[[d]], file = file.path(path_out, paste0("wrds_", d, ".rds")))
}
