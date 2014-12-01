# SE_to_export_03142014.csv
CO2.df <- read.csv("SE_to_export_03142014.csv")
save(list = ls(all=TRUE), file="phreeqc/data/csv.rda")
rm(list = ls(all=TRUE))
