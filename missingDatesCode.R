library(tidyverse)
library(dplyr)
library(testit)


## Load in UVA Data ##
load("//galen.storage.virginia.edu/ivy-hip-chru/ekd6bx/UVA_ind_weather_merged")
uva <- weather_merged

# Choose a weather variable that will be used to identify missing weather variables  #
count_for_missingness <- uva %>% group_by(visitdate, Station, Max.T.Â.F.) %>% summarize(n = sum(is.na(uva$Max.T.Â.F.)))

missing <- subset(visitcount, is.na(Max.T.Â.F.))
missing <- missing[,-c(3,4)]

