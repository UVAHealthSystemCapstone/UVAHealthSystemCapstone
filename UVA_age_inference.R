library(tidyverse)
library(dplyr)
library(ggplot2)

# Loading UVA data
load("//galen.storage.virginia.edu/ivy-hip-chru/ekd6bx/UVA_ind_weather_merged")

# There are 14086 observations that have 0 in the dx column - ignore *for now
missing <- subset(weather_merged, dx == '0')
UVA_individuals <- subset(weather_merged, dx != '0')

UVA_individuals$Principal.Diagnosis4 <- substr(UVA_individuals$dx, 2, 5)
UVA_individuals$Principal.Diagnosis3 <- substr(UVA_individuals$dx, 2, 4)
all_diabetes <- c(icd9_main, icd9_list, icd9_list2, icd9_3, icd10)

UVA_individuals$diabetic <- ifelse(UVA_individuals$dx %in% all_diabetes |UVA_individuals$Principal.Diagnosis4 %in% all_diabetes | UVA_individuals$Principal.Diagnosis3 %in% all_diabetes,1,0)

UVA_individuals$weekdays <- weekdays(as.Date(UVA_individuals$visitdate))
UVA_individuals$diabetic <- as.factor(UVA_individuals$diabetic)
UVA_individuals <- UVA_individuals %>% mutate(diabetes = ifelse(UVA_individuals$diabetic==1, 'yes', 'no'))
#age <- UVA_individuals %>% select(age, diabetic)
ggplot(data = UVA_individuals) +
  geom_histogram(aes(x = age, y = ..density.., fill = diabetes), position = 'dodge', binwidth = 5, alpha= 0.8)+lims(x=c(0,100), y = c(0,0.03))

