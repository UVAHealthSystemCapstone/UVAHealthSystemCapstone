# Proportion of Individuals with Diabetes by Hospital
individuals$diabetic <- as.numeric(individuals$diabetic)
individuals$year<- as.integer(substr(individuals$visitdate, 1,4))
individuals$month<- substr(individuals$visitdate, 6,7)
diabetic_proportions <- individuals %>% group_by(ageRange, year, month, Facility) %>% summarize(mean(diabetic))

#add UVA
uva$diabetic <- as.numeric(uva$diabetic)
uva$year<- as.integer(substr(uva$visitdate, 1,4))
uva$month<- substr(uva$visitdate, 6,7)
uva_diabetic <- uva %>% group_by(year, month, ageRange) %>% summarize(mean(diabetic))
uva_diabetic$Facility <- 'UVA'

uva_diabetic <- data.frame(uva_diabetic)
diabetic_proportions <- data.frame(diabetic_proportions)

diabetic_proportions <- rbind(uva_diabetic, diabetic_proportions)
diabetic_proportions <- subset(diabetic_proportions, year > 2009)
diabetic_proportions <- subset(diabetic_proportions, Facility != "")
diabetic_proportions$ageRange <- factor(diabetic_proportions$ageRange)

# Grouping by Month, Year, and Age Range
ggplot(diabetic_proportions, aes(x = year, y = mean.diabetic., color = ageRange, group = ageRange)) +
  geom_smooth(se = FALSE) + xlab("Year") + ylab('Proportions of ER visits due to Diabetes')

ggplot(diabetic_proportions, aes(x = year, y = mean.diabetic., color = Facility, group = Facility)) +
  geom_smooth(se = FALSE) + xlab("Year") + ylab('Proportions of ER visits due to Diabetes')

ggplot(diabetic_proportions, aes(x = month, y = mean.diabetic., color = Facility, group = Facility)) +
  geom_smooth(se = FALSE) + xlab("Month") + ylab('Proportions of ER visits due to Diabetes')

# Grouping by Race
diabetic_race_proportions <- individuals %>% group_by(Race, year, Facility) %>% summarize(mean(diabetic))
uva_diabetic_race_proportions <- uva %>% group_by(race, year) %>% summarize(mean(diabetic))
uva_diabetic_race_proportions$Facility <- "UVA"
colnames(uva_diabetic_race_proportions)[colnames(uva_diabetic_race_proportions)=="race"] <- "Race"

diabetic_race_proportions <- data.frame(diabetic_race_proportions)
uva_diabetic_race_proportions <- data.frame(uva_diabetic_race_proportions)

diabetic_race_proportions <- rbind(uva_diabetic_race_proportions, diabetic_race_proportions)
diabetic_race_proportions <- subset(diabetic_race_proportions, year > 2009)
diabetic_race_proportions <- subset(diabetic_race_proportions, Facility != "")
diabetic_race_proportions <- subset(diabetic_race_proportions, Race != "")

for (i in 1:492){
  if (diabetic_race_proportions$Race[i] == "B"){
  diabetic_race_proportions$Race[i] <- "Black"
  }
}
for (i in 1:492){
  if (diabetic_race_proportions$Race[i] == "W"){
    diabetic_race_proportions$Race[i] <- "White"
  }
}
for (i in 1:492){
  if (diabetic_race_proportions$Race[i] == "H"){
    diabetic_race_proportions$Race[i] <- "HISPANIC"
  }
}
for (i in 1:492){
  if (diabetic_race_proportions$Race[i] == "N"){
    diabetic_race_proportions$Race[i] <- "Am Indian"
  }
}
for (i in 1:492){
  if (diabetic_race_proportions$Race[i] == "O"){
    diabetic_race_proportions$Race[i] <- "Other"
  }
}
for (i in 1:492){
  if (diabetic_race_proportions$Race[i] == "U"){
    diabetic_race_proportions$Race[i] <- "Unknown"
  }
}
for (i in 1:492){
  if (diabetic_race_proportions$Race[i] == "A"){
    diabetic_race_proportions$Race[i] <- "Asian"
  }
}

# Limiting the number of races included
diabetic_race_proportions <- subset(diabetic_race_proportions, Race == "White" | Race == "Black" | Race == "HISPANIC" | Race == "BIRACIAL" | Race == "Asian" | Race == "Am Indian")

ggplot(diabetic_race_proportions, aes(x = year, y = mean.diabetic., color = Race, group = Race)) +
  geom_smooth(se = FALSE) + xlab("Year") + ylab('Proportions of ER visits due to Diabetes')

# Grouping by Ethnicity
diabetic_ethnicity_proportions <- individuals %>% group_by(Ethnicity, year, Facility) %>% summarize(mean(diabetic))
uva_diabetic_ethnicity_proportions <- uva %>% group_by(ethnicity, year) %>% summarize(mean(diabetic))
uva_diabetic_ethnicity_proportions$Facility <- "UVA"
colnames(uva_diabetic_ethnicity_proportions)[colnames(uva_diabetic_ethnicity_proportions)=="ethnicity"] <- "Ethnicity"

diabetic_ethnicity_proportions <- data.frame(diabetic_ethnicity_proportions)
uva_diabetic_ethnicity_proportions <- data.frame(uva_diabetic_ethnicity_proportions)

diabetic_ethnicity_proportions <- rbind(uva_diabetic_ethnicity_proportions, diabetic_ethnicity_proportions)
diabetic_ethnicity_proportions <- subset(diabetic_ethnicity_proportions, year > 2009)
diabetic_ethnicity_proportions <- subset(diabetic_ethnicity_proportions, Facility != "")
diabetic_ethnicity_proportions <- subset(diabetic_ethnicity_proportions, Ethnicity != "")

unique(diabetic_ethnicity_proportions$Ethnicity)
for (i in 1:219){
  if (diabetic_ethnicity_proportions$Ethnicity[i] == "N"){
    diabetic_ethnicity_proportions$Ethnicity[i] <- "Non-Hispanic"
  }
}
for (i in 1:219){
  if (diabetic_ethnicity_proportions$Ethnicity[i] == "H"){
    diabetic_ethnicity_proportions$Ethnicity[i] <- "Hispanic"
  }
}
for (i in 1:219){
  if (diabetic_ethnicity_proportions$Ethnicity[i] == 0){
    diabetic_ethnicity_proportions$Ethnicity[i] <- "Unknown"
  }
}
for (i in 1:219){
  if (diabetic_ethnicity_proportions$Ethnicity[i] == "R"){
    diabetic_ethnicity_proportions$Ethnicity[i] <- "Pt Refused"
  }
}
for (i in 1:219){
  if (diabetic_ethnicity_proportions$Ethnicity[i] == "Patient Refused"){
    diabetic_ethnicity_proportions$Ethnicity[i] <- "Pt Refused"
  }
}
for (i in 1:219){
  if (diabetic_ethnicity_proportions$Ethnicity[i] == "U"){
    diabetic_ethnicity_proportions$Ethnicity[i] <- "Patient Unavailable"
  }
}
unique(diabetic_ethnicity_proportions$Ethnicity)
ggplot(diabetic_ethnicity_proportions, aes(x = year, y = mean.diabetic., color = Ethnicity, group = Ethnicity)) +
  geom_smooth(se = FALSE) + xlab("Year") + ylab('Proportions of ER visits due to Diabetes')

diabetic_ethnicity_proportions <- subset(diabetic_ethnicity_proportions, Ethnicity == "Hispanic" | Ethnicity == "Non-Hispanic" | Ethnicity == "Unknown")
ggplot(diabetic_ethnicity_proportions, aes(x = year, y = mean.diabetic., color = Ethnicity, group = Ethnicity)) +
  geom_smooth(se = FALSE) + xlab("Year") + ylab('Proportions of ER visits due to Diabetes')
