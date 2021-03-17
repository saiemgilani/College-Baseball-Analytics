# devtools::install_github("BillPetti/baseballr")

library(baseballr)


park_factors <- get_ncaa_park_factor(234,c(2015:2019), type = "conference")
write.csv('park_factors_FSU.csv',park_factors)