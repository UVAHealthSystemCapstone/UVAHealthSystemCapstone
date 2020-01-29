uva$ageRange <- ifelse(uva$age < 11, "0-10",ifelse(uva$age > 10 & uva$age < 21, "11-20", 
                      ifelse(uva$age > 20 & uva$age < 31, "21-30", ifelse(uva$age > 30 & uva$age < 41, "31-40",
                      ifelse(uva$age > 40 & uva$age < 51, "41-50", ifelse(uva$age > 50 & uva$age < 61, "51-60",
                      ifelse(uva$age > 60 & uva$age < 71, "61-70", ifelse(uva$age > 70 & uva$age < 81, "71-80",
                      ifelse(uva$age > 80, "81+", "ERROR")))))))))

individuals$ageRange <- ifelse(individuals$Age < 11, "0-10",ifelse(individuals$Age > 10 & individuals$Age < 21, "11-20", 
                                                   ifelse(individuals$Age > 20 & individuals$Age < 31, "21-30", ifelse(individuals$Age > 30 & individuals$Age < 41, "31-40",
                                                   ifelse(individuals$Age > 40 & individuals$Age < 51, "41-50", ifelse(individuals$Age > 50 & individuals$Age < 61, "51-60",
                                                   ifelse(individuals$Age > 60 & individuals$Age < 71, "61-70", ifelse(individuals$Age > 70 & individuals$Age < 81, "71-80",
                                                   ifelse(individuals$Age > 80, "81+", "ERROR")))))))))


# Proportion of Individuals with Diabetes by Hospital
diabetic_proportions <- individuals %>% group_by(ageRange, year) %>% summarize(mean(diabetic))

#add UVA
uva_diabetic <- uva %>% group_by(year, ageRange) %>% summarize(mean(diabetic))

uva_diabetic <- data.frame(uva_diabetic)
diabetic_proportions <- data.frame(diabetic_proportions)

diabetic_proportions <- rbind(uva_diabetic, diabetic_proportions)

diabetic_proportions <- subset(diabetic_proportions, year > 2009)
diabetic_proportions$ageRange <- factor(diabetic_proportions$ageRange)


ggplot(diabetic_proportions, aes(x = year, y = mean.diabetic., color = ageRange, group = ageRange)) +
  geom_smooth(se = FALSE) + xlab("Year") + ylab('Proportions of ER visits due to Diabetes')
