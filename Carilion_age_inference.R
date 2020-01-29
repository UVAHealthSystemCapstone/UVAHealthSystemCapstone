library(tidyverse)
library(dplyr)
library(ggplot2)

#load data
load("//galen.storage.virginia.edu/ivy-hip-chru/ekd6bx/Carilion_ind_weather_merged")
individuals <- weather_merged_car

# Creating data frame with just diabetes patients (based on ICD codes)
library(stringr)
# ICD 9 Codes
icd9_main <- as.character(c(249,250))
icd9_list <- as.character(c(2493,2498,7751,6480,2500,2535,5881,2491,2490,2492,7902,2503,7517,2499,
                            3572,3620,2509,7756,2502,3371,3535,5363,7750,6488,2714,2504,
                            2507,2506,2508,2505,2497,7318,2494,2750,2510,2501,2512,2496,
                            2495,2511,7071,2496,2495,2508,2497,2494,2498,2503,2491,2493,
                            3620,7750,2504,2507,7800,2505,2510,2501,2762,2511,7130,7135))
icd9_list2 <- as.character(c(36641,44381,58181,58381,79029,36201,36207,36202,36203,36204,36205,36206))

icd9_list3 <- 'V180|V771|V653'
icd10_codes <- "E23.2|O24.92|E10.618|E11.618|E13.36|E13.44|Z13.1|E13.42|E10.41|E10.44|E11.41|E11.44|E13.40|E13.41|E13.618|E13.21|E09.21|E10.42|E09.36|E10.36|E10.620|E11.36|E11.620|E13.620|E08.36|E08.41|E08.44|E08.61|E09.61|E09.620|E10.31|E10.35|E10.610|E11.31|E11.35|E13.31|E13.35|E11.40|E08.42|E08.620|E11.21|E13.610|N25.1|O24.4|E10.21|E11.610|O24|E08.355|E10.40|E11.42|E11|E10|E08.21|E13|E10.65|E11.65|E13.65|Z83.3|E10.43|E13.9|O24.439|E09.41|E09.44|E11.49|E13.319|E13.359|E13.49|E08.3553|E09.35|E09.355|E09.42|E09.52|E09.610|E10.355|E11.355|E13.29|E13.33|E13.34|E13.355|E13.3553|E13.3559|E13.43|E13.52|O24.93|E10.29|E11.29|Z86.32|E08.610|E09.29|E10.32|E11.32|E11.51|E13.32|E10.311|E10.51|E10.52|E11.311|E11.39|E11.52|E13.321|E09.9|O24.419|E11.43|E09.311|E09.351|E09.40|E09.51|E10.351|E10.359|E13.311|E13.341|E13.351|E13.39|E10.9|O24.32|O24.33|E10.39|E10.649|E13.3|E13.4|E13.5|O24.414|O24.424|O24.430|O24.434|E08.9|E10.10|E11.8|E13.01|E13.641|E13.649|E13.8|O24.429|O24.919|E08.31|E08.33|E08.34|E08.35|E08.40|E08.49|E08.618|E09.31|E09.33|E09.34|E09.3553|E09.618|E10.22|E10.319|E10.33|E10.34|E10.3551|E10.3552|E10.3553|E10.3559|E11.319|E11.321|E11.33|E11.34|E11.351|E11.3551|E11.3552|E11.3553|E11.3559|E13.3551|E13.3552|E13.37|E13.51|E08.29|E08.32|E09.32|E10.49|E11.359|E13.10|E09|E13.22|E10.69|E09.22|E11.22|E08.3213|E08.3219|E08.3313|E08.3319|E08.3413|E08.3419|E08.3513|E08.3519|E08.352|E08.354|E08.3543|E08.3551|E08.3552|E08.3559|E08.37|E13.354|E11.9|E10.321|E10.331|E10.341|E11.331|E11.341|E13.331|E11.649|E09.01|E09.11|E09.8|E10.11|E10.641|E11.01|E13.11|P70.0|E08.0|E08.1|E08.2|E08.3|E08.4|E08.5|E08.62|E08.63|E08.630|E08.64|E08.65|E08.8|E09.0|E09.2|E09.3|E09.4|E09.5|E09.62|E09.63|E09.65|E10.628|E13.6|E13.628|E13.638|E08|E08.39|E08.43|E09.39|O24.319|E13.329|E11.59|E10.630|E10.8|E11.630|E11.641|E11.628|E13.59|E09.319|E09.359|E08.3549|E08.22|E08.52|E11.69|E08.3211|E08.3212|E08.3291|E08.3293|E08.3299|E08.3311|E08.3393|E08.3411|E08.3412|E08.3491|E08.3493|E08.3499|E08.353|E08.37X3|E09.321|E09.329|E09.3551|E09.3559|E10.3213|E10.354|E11.3213|E11.354|E13.3211|E13.3213|E13.3219|E13.3293|E13.3313|E13.3413|E13.3419|E13.3543|E10.59|E08.01|E08.11|E08.641|E13.621|E13.69|E08.6|E09.6|O24.311|O24.312|O24.313|O24.811|O24.812|O24.813|O24.819|E09.621|E09.630|E13.622|E10.329|E11.329|O24.415|O24.435|E09.43|E11.349|E09.331|E09.341|E10.339|E10.349|E11.339|E13.339|E13.349|E08.3541|E09.49|E09.641|E09.10|E10.622|E08.621|O24.9|E08.10|E08.59|E08.628|E08.638|E08.649|E08.69|E09.59|E09.628|E09.638|E09.69|O24.011|O24.012|O24.013|O24.019|O24.111|O24.112|O24.113|O24.119|E09.339|E09.349|E11.621|E10.621|E11.622|P70.1|O24.0|E09.649|E13.00|E08.622|O24.425|O24.1|E09.622|E09.00|E11.00|O24.3|O24.8|E08-E13|R73.03|E10.61|E11.61|E13.61|O24.41|O24.42|O24.43|O24.91|P70.2|E08.311|E08.351|E08.51|E10.3513|E10.37|E11.3513|E11.37|E13.3513|E10.1|E10.64|E11.0|E11.1|E11.64|E13.0|E13.1|E13.64|E08.319|E08.321|E08.329|E08.3292|E08.331|E08.3312|E08.339|E08.3391|E08.3392|E08.3399|E08.341|E08.349|E08.3492|E08.3511|E08.3512|E08.3521|E08.3522|E08.3523|E08.3529|E08.3531|E08.3532|E08.3533|E08.3539|E08.359|E08.3591|E08.3592|E08.3593|E08.3599|E08.37X1|E08.37X2|E08.37X9|E09.3211|E09.3212|E09.3213|E09.3219|E09.3291|E09.3292|E09.3293|E09.3299|E09.3311|E09.3312|E09.3313|E09.3319|E09.3391|E09.3392|E09.3393|E09.3399|E09.3411|E09.3412|E09.3413|E09.3419|E09.3491|E09.3492|E09.3493|E09.3499|E09.3511|E09.3512|E09.3513|E09.3519|E09.352|E09.3521|E09.3522|E09.3523|E09.3529|E09.353|E09.3531|E09.3532|E09.3533|E09.3539|E09.354|E09.3543|E09.3552|E09.3591|E09.3592|E09.3593|E09.3599|E09.37|E09.37X1|E09.37X2|E09.37X3|E09.37X9|E10.3211|E10.3212|E10.3219|E10.3291|E10.3292|E10.3293|E10.3299|E10.3311|E10.3312|E10.3313|E10.3319|E10.3391|E10.3392|E10.3393|E10.3399|E10.3411|E10.3412|E10.3413|E10.3419|E10.3491|E10.3492|E10.3493|E10.3499|E10.3511|E10.3512|E10.3519|E10.352|E10.3521|E10.3522|E10.3523|E10.3529|E10.353|E10.3531|E10.3532|E10.3533|E10.3539|E10.3541"

icd10 <- unlist(strsplit(icd10_codes, "\\|"))
icd9_3 <- unlist(strsplit(icd9_list3, "\\|"))

individuals$Principal.Diagnosis4 <- substr(individuals$Principal.Diagnosis, 2, 5)
individuals$Principal.Diagnosis3 <- substr(individuals$Principal.Diagnosis, 2, 4)


all_diabetes <- c(icd9_main, icd9_list, icd9_list2, icd9_3, icd10)
individuals$diabetic <- ifelse(individuals$Principal.Diagnosis %in% all_diabetes | individuals$Principal.Diagnosis4 %in% all_diabetes | individuals$Principal.Diagnosis3 %in% all_diabetes,1,0)

individuals$weekdays <- weekdays(as.Date(individuals$visitdate))
individuals$diabetic <- as.factor(individuals$diabetic)
individuals <- individuals %>% mutate(diabetes = ifelse(individuals$diabetic==1, 'yes', 'no'))

# summary(individuals)

# Statistic inference on days of week (diabetic v.s. non-diabetic)
library(dplyr)
library(ggplot2)
# age <- individuals %>% select(Age, diabetic)
ggplot(data = individuals) +
  stat_summary(
    mapping = aes(x= diabetic, y = Age),
    fun.ymin = min,
    fun.ymax = max,
    fun.y = mean
  )
ggplot(data = individuals) +
  geom_histogram(aes(x = Age, y = ..density.., fill = diabetes), position = 'dodge', binwidth = 5, alpha= 0.8)+lims(x=c(0,100), y=c(0,0.03))

age1 <- individuals %>% select(Age, diabetic) %>% 
  filter(diabetic == 1)
labels <- cut(age1$Age, breaks = c(-1, 17, 44, 64, max(age1$Age)), labels = c("0-17", "18-44", "45-64", "65+"))
er_all <- age1 %>% 
  mutate(group = labels) 
er_adult <- er_all %>% 
  filter(group != "0-17") %>% 
  group_by(group) %>%
  summarise(n = n()) %>% 
  mutate(prop = n/sum(n), from = "ER") %>%
  select(group, prop, from)
# age_prop%>%filter(is.na(group))


VAsum <- 2.9+15.1+20.9
VA_adult <- data.frame(group = c("18-44", "45-64", "65+"), prop = c(2.9/VAsum, 15.1/VAsum, 20.9/VAsum), from = c("VIRGINIAN","VIRGINIAN","VIRGINIAN"))

USAsum <- 2.9+14.5+22.6
USA_adult <- data.frame(group = c("18-44", "45-64", "65+"), prop = c(2.9/USAsum, 14.5/USAsum, 22.6/USAsum), from = c("USA","USA","USA"))

age_prop <- rbind(er_adult, VA_adult, USA_adult)

ggplot(data = age_prop) +
  geom_col(aes(x = group, y = prop, fill = from), alpha = 0.8, position = 'dodge') +
  xlab("Age group") +
  ylab("Percentage in Adults")

