uva$month <- substr(uva$visitdate, 6,7)
individuals$month <- substr(individuals$visitdate, 6,7)


# Proportion of Individuals with Diabetes by Hospital
diabetic_proportions <- individuals %>% group_by(month) %>% summarize(mean(diabetic))
diabetic_proportions$system <- 'Carilion'

#add UVA
uva_diabetic <- uva %>% group_by(month) %>% summarize(mean(diabetic))
uva_diabetic$system <- 'UVA'

uva_diabetic <- data.frame(uva_diabetic)
diabetic_proportions <- data.frame(diabetic_proportions)

diabetic_proportions <- rbind(uva_diabetic, diabetic_proportions)

diabetic_proportions <- subset(diabetic_proportions)
diabetic_proportions$month <- factor(diabetic_proportions$month)


ggplot(diabetic_proportions, aes(x = month, y = mean.diabetic., color = system, group = system)) +
  geom_smooth(se = FALSE) + xlab("Month") + ylab('Proportion of ER visits due to Diabetes')