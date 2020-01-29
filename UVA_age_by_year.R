library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggpubr)

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

# Statistic inference on days of week (diabetic v.s. non-diabetic)
age <- UVA_individuals %>% select(age, diabetic, visitdate)
age$year<- as.factor(substr(age$visitdate, 1,4))
age <- age %>% select(age, diabetic, year) %>% filter(year!="2009")

ggplot(data= age, aes(x = age, y = ..density.., fill = diabetic)) +
  geom_histogram(position = 'dodge', binwidth = 5, alpha= 0.8) +
  facet_wrap(~ year, nrow = 2)
