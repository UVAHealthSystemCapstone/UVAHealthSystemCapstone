
# Proportion of Individuals with Diabetes by Hospital
diabetic_proportions <- individuals %>% group_by(Gender, year) %>% summarize(mean(diabetic))

#add UVA
uva_diabetic <- uva %>% group_by(year, sex) %>% summarize(mean(diabetic))
names(uva_diabetic)[2] <- 'Gender'

uva_diabetic <- data.frame(uva_diabetic)
diabetic_proportions <- data.frame(diabetic_proportions)

diabetic_proportions <- rbind(uva_diabetic, diabetic_proportions)

diabetic_proportions <- subset(diabetic_proportions, year > 2009 & Gender != 'U')
diabetic_proportions$Gender <- factor(diabetic_proportions$Gender)


ggplot(diabetic_proportions, aes(x = year, y = mean.diabetic., color = Gender, group = Gender)) +
  geom_smooth(se = FALSE) + xlab("Year") + ylab('Proportions of ER visits due to Diabetes')