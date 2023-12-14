rm(list = ls()); gc()

setwd("/Users/belakoch/Documents/coding/ivolR output/all banks/estimation input")
files <- list.files()
files <- split(files, ceiling(seq_along(files)/10))
for (i in 1:length(files)) zip(paste0("part_", i, ".zip"), files[[i]])
