load("//galen.storage.virginia.edu/ivy-hip-chru/ekd6bx/Carilion_ind_weather_merged")
carilion <- weather_merged_car

load("//galen.storage.virginia.edu/ivy-hip-chru/ekd6bx/UVA_ind_weather_merged")
uva <- weather_merged

library(Amelia)
# missmap(as.data.frame(carilion[,1:23]))
missmap(as.data.frame(uva[,1:23]))

library(rlist)
library(tidyverse)
library(tibble)
columns <- (colnames(carilion))
missingness <- tibble(columns)
missingness$prop <- 0
for (i in 1:115){
  missingness$prop[i] <-sum(is.na(carilion[i]))/1388129
}

uva_columns <- colnames(uva)
missingness_uva <- tibble(uva_columns)
missingness_uva$prop <- 0
for (i in 1:113){
  missingness_uva$prop[i] <- sum(is.na(uva[i]))/468757
}
