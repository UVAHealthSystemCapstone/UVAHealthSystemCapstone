# UVA Data
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


library(forcats)
prop_avg = mean(UVA_individuals$diabetic == 1)
UVA_individuals <- UVA_individuals %>% 
  mutate(weekdays = fct_relevel(weekdays,"Monday",'Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday'))
# proportion of ER visit due to Diabetes between weekdays
prop_by_wkd <- UVA_individuals %>% group_by(weekdays) %>% summarise(p = mean(diabetic == 1))
ggplot() +
  geom_point(data = prop_by_wkd, mapping = aes(x = weekdays, y = p)) + 
  geom_hline(yintercept = prop_avg, col = 'blue') +
  ylab("Proportion of Diabetes-Related ED Visits") +
  ggtitle("Diabetes-Related ED Visits at UVA Between Weekdays")
test <- UVA_individuals %>% group_by(weekdays) %>% summarize(prop = mean(diabetic == 1), n = n()) %>%
  mutate(diff = abs(prop - prop_avg)) %>%
  mutate(se = sqrt(prop_avg*(1-prop_avg)/n)) %>%
  mutate(p_value = pnorm(diff, 0, se, lower.tail = FALSE))

# Statistic inference on days of week (diabetic v.s. non-diabetic)
wkd <- UVA_individuals %>% 
  select(weekdays, diabetic) %>%
  count(diabetic, weekdays) %>% 
  group_by(diabetic) %>% 
  mutate(prop = prop.table(n))
ggplot(data = wkd) +
  geom_col(mapping = aes(x = weekdays, y = prop, fill = diabetic), alpha = 0.8, position = 'dodge') +
  ylab('proportion of patients who visit ER')


# between diabetic and non-diabetic (2 population)
#-- random variables :1. the day diabetric patients visit ER; 2. the day non-diabetric visit ER
#-- test statistic: |p1 - p0| (using proportion from sample to estimate porbability from population)
#---- population: the difference between the probability a diabetric patient visit ER on a certain day (p1)
#             and the probability a non-diabetric patient visit ER on that day(p0)
#---- sample: the difference between the proportion of diabatic patients who visit ER on a certain day (p1_hat)
#         and the proportion of non-diabetic patients who visit ER on that day(p0s_hat)

#-- hypothesis: |p1-p0| = 0

test_dt <- merge(wkd%>%filter(diabetic==0),wkd %>%filter(diabetic==1), by = 'weekdays', suffixes = c("0","1")) %>%
  mutate(diff = abs(prop0 - prop1)) %>%
  mutate(p_pool = (prop0*n0 + prop1*n1)/(n0+n1)) %>%
  mutate(se = sqrt(p_pool*(1-p_pool)*(1/n0 + 1/n1))) %>%
  mutate(p_value = pnorm(diff, 0, se, lower.tail = FALSE))
test_result <- test_dt %>% select(weekdays, diff, p_value)
test_result

# within diabetic (1 population)
#-- random variable: the day diabetic patient visit ER
#-- test statistic: (baseline : Wednesday)
#---- population: the change(difference) of the probability diabatic patients visit ER on not-Wed days
#                 and the probability diabetic patients visit ER on Wed.
#---- sample: proportion estimates probability

dia_wkd <- UVA_individuals %>% select(weekdays, diabetic) %>% filter(diabetic == 1) %>% 
  group_by(weekdays) %>% 
  summarise(n = n()) %>%
  mutate(prop = prop.table(n))
base <- dia_wkd %>% filter(weekdays=='Wednesday') %>% select(prop,n)
wed_prop <- base $prop
wed_n <- base$n 
dia_wkd %>% mutate(diff = abs(prop - wed_prop)) %>%
  mutate(p_pool = (prop * n + wed_prop * wed_n)/(n+wed_n)) %>%
  mutate(se = sqrt(p_pool*(1-p_pool)*(1/n+1/wed_n))) %>%
  mutate(p_value = pnorm(diff, 0, se, lower.tail = FALSE))
