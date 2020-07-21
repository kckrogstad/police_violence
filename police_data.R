#Police Data
#Import Data

#install.packages("Hmisc")
#install.packages("readxl")
#install.packages("dplyr")
#install.packages("tidyr")
#install.packages("usmap")
#install.packages("viridisLite")

library(lmerTest)
library(readxl)
library(Hmisc)
library(dplyr)
library(ggplot2)
library(tidyr)
library(usmap)
library(viridisLite)
library(viridis)

setwd("C:/Users/kirby/Documents/Police violence")

####By police department####

setwd("C:/Users/kirby/Documents/Police violence")
a <- read_xlsx("C:/Users/kirby/Documents/Police violence/MPVDatasetDownload_police.xlsx", sheet = "2013-2019 Killings by PD", na = "")
na.action(a, na.omit)
str(a)

####By State####
b <- read_xlsx("C:/Users/kirby/Documents/Police violence/MPVDatasetDownload_police.xlsx", sheet = "2013-2019 Killings by State", na = "")
na.action(b, na.omit)
str(b)

rate_all <- plot_usmap(data = b, values = "Rate (All People)", color = "Black") + 
  ggtitle("Police Violence by State") + 
  labs(caption = "Source: Mapping Police Violence") +
  scale_fill_viridis(name = "Rate, Death/100,000 People") + 
  theme(plot.title = element_text(hjust = 0.5, size = 20, family = "Times New Roman"), 
        legend.text = element_text(size = 12, family = "Times New Roman"),
        legend.title = element_text(hjust = 0.5, size = 14, family = "Times New Roman"),
        plot.caption = element_text(size = 10, family = "Times New Roman", face = "italic"),
        legend.position = "bottom",
        legend.key.size = unit(1, "cm"))

jpeg(filename = "rate_all.jpeg", units = 'in', width = 8.0, height = 5.5, res = 1200, type = 'cairo')
plot(rate_all)
dev.off()

rate_black <- plot_usmap(data = b, values = "Rate (Black People)", color = "Black") + 
  ggtitle("Police Violence by State Against") + 
  labs(caption = "Source: Mapping Police Violence") +
  scale_fill_viridis(name = "Rate, Death/100,000 Black People") + 
  theme(plot.title = element_text(hjust = 0.5, size = 20, family = "Times New Roman"), 
        legend.text = element_text(size = 12, family = "Times New Roman"),
        legend.title = element_text(hjust = 0.5, size = 14, family = "Times New Roman"),
        plot.caption = element_text(size = 10, family = "Times New Roman", face = "italic"),
        legend.position = "bottom",
        legend.key.size = unit(1, "cm"))

jpeg(filename = "rate_black.jpeg", units = 'in', width = 8.0, height = 5.5, res = 1200, type = 'cairo')
plot(rate_black)
dev.off()

rate_disparity <- plot_usmap(data = b, values = "Disparity in Rate" , color = "Black") + 
  ggtitle("Disparity in Rate of Police Violence") + 
  labs(caption = "Source: Mapping Police Violence") +
  scale_fill_viridis(name = "Disparity (Black Rate - All Rate)") + 
  theme(plot.title = element_text(hjust = 0.5, size = 20, family = "Times New Roman"), 
        legend.text = element_text(size = 12, family = "Times New Roman"),
        legend.title = element_text(hjust = 0.5, size = 14, family = "Times New Roman"),
        plot.caption = element_text(size = 10, family = "Times New Roman", face = "italic"),
        legend.position = "bottom",
        legend.key.size = unit(1, "cm"))

jpeg(filename = "rate_disparity.jpeg", units = 'in', width = 8.0, height = 5.5, res = 1200, type = 'cairo')
plot(rate_disparity)
dev.off()

####All Police killings####
c <- read_xlsx("C:/Users/kirby/Documents/Police violence/MPVDatasetDownload_police.xlsx", sheet = "2013-2019 Police Killings", na = "")
na.action(c, na.omit)
str(c)

#Gender

Gender <- as.data.frame(c %>% count(`Victim's gender`))
Gender$percent <- Gender$n/sum(Gender$n)

gender_plot <- Gender %>% filter(percent > 0.01) %>%
  ggplot(aes(y = percent, x=`Victim's gender`)) +
  geom_col(aes(fill = `Victim's gender`), show.legend = FALSE) + 
  scale_y_continuous(labels = scales::percent, breaks = seq(0,1,.1)) + 
  ggtitle("Police Violence by Gender") + 
  labs( x = NULL, y = "Percent of Victims", caption = "Source: Mapping Police Violence") + 
  scale_fill_viridis_d() + 
  theme(panel.background = element_rect("white"),
        axis.line = element_line(color = "black"),
        panel.grid.major.y = element_blank(),
        plot.title = element_text(size = 18, hjust = 0.5),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14), 
        plot.caption = element_text(face = "italic"))

jpeg(filename = "gender.jpeg", units = 'in', width = 8.0, height = 5.5, res = 1200, type = 'cairo')
plot(gender_plot)
dev.off()

#Race

Race <- as.data.frame(c %>% count(`Victim's race`))
Race$percent <- Race$n/sum(Race$n)
Race$total_pop <- c(14465124, 37685848, 50477594, 2247098, 481576, 0, 0,196817552)
Race$Rate <- Race$n/Race$total_pop*1000000/7

race_plot <- Race %>% filter(total_pop > 0) %>%
  ggplot(aes(y = Rate, x=`Victim's race`)) +
  geom_col(aes(fill = `Victim's race`), show.legend = FALSE) + 
  scale_y_continuous(breaks = seq(0,12,2)) + 
  ggtitle("Police Violence by Race") + 
  labs( x = NULL, y = "Rate of Violence, Deaths/100,000 People", caption = "Source: Mapping Police Violence") + 
  scale_fill_viridis_d() + 
  theme(panel.background = element_rect("white"),
        axis.line = element_line(color = "black"),
        panel.grid.major.y = element_blank(),
        plot.title = element_text(size = 18, hjust = 0.5),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14), 
        axis.text.x = element_text(angle = 30, vjust = 0.5),
        plot.caption = element_text(face = "italic"))

jpeg(filename = "race.jpeg", units = 'in', width = 8.0, height = 5.5, res = 1200, type = 'cairo')
plot(race_plot)
dev.off()

#Cause of Death

Death <- as.data.frame(c %>% count(`Cause of death`))
Death$percent <- Death$n/sum(Death$n)
sum(Death$n)

cause_death <- Death %>% filter(percent >= 0.01) %>%
  ggplot(aes(y = percent, x=`Cause of death`)) +
  geom_col(aes(fill = `Cause of death`), show.legend = FALSE) + 
  scale_y_continuous(labels = scales::percent, breaks = seq(0,1,.1)) + 
  ggtitle("Cause of Death of Police Violence") + 
  labs( x = NULL, y = "Percent of Victims", caption = "Source: Mapping Police Violence") + 
  scale_fill_viridis_d() + 
  theme(panel.background = element_rect("white"),
        axis.line = element_line(color = "black"),
        panel.grid.major.y = element_blank(),
        plot.title = element_text(size = 18, hjust = 0.5),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14), 
        plot.caption = element_text(face = "italic"))

jpeg(filename = "causeofdeath.jpeg", units = 'in', width = 8.0, height = 5.5, res = 1200, type = 'cairo')
plot(cause_death)
dev.off()

#Mental Health

Mental_health <- as.data.frame(c %>% count(`Symptoms of mental illness?`))
Mental_health$percent <- Mental_health$n/sum(Mental_health$n)
Mental_health$`Symptoms of mental illness?` <- factor(Mental_health$`Symptoms of mental illness?`, levels = c("Yes", "No", "Drug or alcohol use", "Unknown", "unknown", "Unkown"))

mental_health_plot <- Mental_health %>% filter(percent >= 0.01) %>%
  ggplot(aes(y = percent, x=`Symptoms of mental illness?`)) +
  geom_col(aes(fill = `Symptoms of mental illness?`), show.legend = FALSE) + 
  scale_y_continuous(labels = scales::percent) +
  ggtitle("Mental Health and Police Violence") + 
  labs( x = "Symptoms of mental illness?", y = "Percent of Victims", caption = "Source: Mapping Police Violence") + 
  scale_fill_viridis_d() + 
  theme(panel.background = element_rect("white"),
        axis.line = element_line(color = "black"),
        panel.grid.major.y = element_blank(),
        plot.title = element_text(size = 18, hjust = 0.5),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14), 
        plot.caption = element_text(face = "italic"))

jpeg(filename = "mentalhealth.jpeg", units = 'in', width = 8.0, height = 5.5, res = 1200, type = 'cairo')
plot(mental_health_plot)
dev.off()

#geography by year
geo_total <- as.data.frame(c %>% count(Geography))

year_2013 <- c %>% filter(`Date of Incident (month/day/year)`<= '2013-12-31') 
geo_2013 <- as.data.frame(year_2013 %>% count(Geography))
colnames(geo_2013) = c("Geography", "2013")

year_2014 <- c %>% filter(`Date of Incident (month/day/year)`<= '2014-12-31' & `Date of Incident (month/day/year)`>= "2014-1-1")
geo_2014 <- as.data.frame(year_2014 %>% count(Geography))
colnames(geo_2014) = c("Geography", "2014")


year_2015<- c %>% filter(`Date of Incident (month/day/year)`<= '2015-12-31' & `Date of Incident (month/day/year)`>= "2015-1-1")
geo_2015 <- as.data.frame(year_2015 %>% count(Geography))
colnames(geo_2015) = c("Geography", "2015")


year_2016<- c %>% filter(`Date of Incident (month/day/year)`<= '2016-12-31' & `Date of Incident (month/day/year)`>= "2016-1-1")
geo_2016 <- as.data.frame(year_2016 %>% count(Geography))
colnames(geo_2016) = c("Geography", "2016")


year_2017<- c %>% filter(`Date of Incident (month/day/year)`<= '2017-12-31' & `Date of Incident (month/day/year)`>= "2017-1-1")
geo_2017 <- as.data.frame(year_2017 %>% count(Geography))
colnames(geo_2017) = c("Geography", "2017")


year_2018<- c %>% filter(`Date of Incident (month/day/year)`<= '2018-12-31' & `Date of Incident (month/day/year)`>= "2018-1-1")
geo_2018 <- as.data.frame(year_2018 %>% count(Geography))
colnames(geo_2018) = c("Geography", "2018")


year_2019<- c %>% filter(`Date of Incident (month/day/year)`<= '2019-12-31' & `Date of Incident (month/day/year)`>= "2019-1-1")
geo_2019 <- as.data.frame(year_2019 %>% count(Geography))
colnames(geo_2019) = c("Geography", "2019")


geo_by_year <- Reduce(inner_join, list(geo_2013, geo_2014, geo_2015, geo_2016, geo_2017, geo_2018, geo_2019))
geo_year_long <- gather(geo_by_year, year, deaths, 2:8, factor_key = TRUE) 

geo_year_long$year <- as.numeric(as.character(geo_year_long$year))
geo_year_long$Geography <- as.factor(geo_year_long$Geograp)


geo_year_graph <- geo_year_long %>% filter(deaths > 100) %>% ggplot(aes(y = deaths, x = year, color = Geography)) + 
  geom_point() + 
  geom_line() + 
  scale_x_continuous(breaks = seq(2012, 2019, 1)) + 
  scale_y_continuous(limits = c(0,600), breaks = seq(0,600,100)) + 
  ggtitle("Police Violence broken down by Geography") + 
  labs( x = "Year", y = "Number of Victims", caption = "Source: Mapping Police Violence") + 
  theme(panel.background = element_rect("white"),
        axis.line = element_line(color = "black"),
        panel.grid.major.y = element_blank(),
        plot.title = element_text(size = 18, hjust = 0.5),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14), 
        plot.caption = element_text(face = "italic"))

jpeg(filename = "geogrpahy_year.jpeg", units = 'in', width = 8.0, height = 5.5, res = 1200, type = 'cairo')
plot(geo_year_graph)
dev.off()

#Total by Year
years <- as.list(c(nrow(year_2013), nrow(year_2014), nrow(year_2015), nrow(year_2016), nrow(year_2017), nrow(year_2018), nrow(year_2019)))
years

total_by_year <- data.frame(Year = c(2013:2019), Deaths = c(1106, 1048, 1102, 1070, 1087, 1139, 1096))

total_by_year_graph <- total_by_year %>% ggplot(aes(y = Deaths, x = Year)) + 
  geom_point() + 
  geom_line() + 
  scale_x_continuous(breaks = seq(2012, 2019, 1)) + 
  scale_y_continuous(limits = c(0,1200), breaks = seq(0,1200,100)) + 
  ggtitle("Police Violence by Year") + 
  labs( x = "Year", y = "Number of Victims", caption = "Source: Mapping Police Violence") + 
  theme(panel.background = element_rect("white"),
        axis.line = element_line(color = "black"),
        panel.grid.major.y = element_blank(),
        plot.title = element_text(size = 18, hjust = 0.5),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14), 
        plot.caption = element_text(face = "italic"))

jpeg(filename = "total_year.jpeg", units = 'in', width = 8.0, height = 5.5, res = 1200, type = 'cairo')
plot(total_by_year_graph)
dev.off()

#Proportion of Geography by year

year_2013 <- c %>% filter(`Date of Incident (month/day/year)`<= '2013-12-31') 
prop_2013 <- as.data.frame(year_2013 %>% count(Geography))
prop_2013$"2013" <- prop_2013$n/sum(prop_2013$n) 
prop_2013_sel <- prop_2013 %>% select(Geography, "2013")

year_2014 <- c %>% filter(`Date of Incident (month/day/year)`<= '2014-12-31' & `Date of Incident (month/day/year)`>= "2014-1-1")
prop_2014 <- as.data.frame(year_2014 %>% count(Geography))
prop_2014$"2014" <- prop_2014$n/sum(prop_2014$n) 
prop_2014_sel <- prop_2014 %>% select(Geography, "2014")

year_2015<- c %>% filter(`Date of Incident (month/day/year)`<= '2015-12-31' & `Date of Incident (month/day/year)`>= "2015-1-1")
prop_2015 <- as.data.frame(year_2015 %>% count(Geography))
prop_2015$"2015" <- prop_2015$n/sum(prop_2015$n) 
prop_2015_sel <- prop_2015 %>% select(Geography, "2015")

year_2016<- c %>% filter(`Date of Incident (month/day/year)`<= '2016-12-31' & `Date of Incident (month/day/year)`>= "2016-1-1")
prop_2016 <- as.data.frame(year_2016 %>% count(Geography))
prop_2016$"2016" <- prop_2016$n/sum(prop_2016$n) 
prop_2016_sel <- prop_2016 %>% select(Geography, "2016")

year_2017<- c %>% filter(`Date of Incident (month/day/year)`<= '2017-12-31' & `Date of Incident (month/day/year)`>= "2017-1-1")
prop_2017 <- as.data.frame(year_2017 %>% count(Geography))
prop_2017$"2017" <- prop_2017$n/sum(prop_2017$n) 
prop_2017_sel <- prop_2017 %>% select(Geography, "2017")

year_2018<- c %>% filter(`Date of Incident (month/day/year)`<= '2018-12-31' & `Date of Incident (month/day/year)`>= "2018-1-1")
prop_2018 <- as.data.frame(year_2018 %>% count(Geography))
prop_2018$"2018" <- prop_2018$n/sum(prop_2018$n) 
prop_2018_sel <- prop_2018 %>% select(Geography, "2018")

year_2019<- c %>% filter(`Date of Incident (month/day/year)`<= '2019-12-31' & `Date of Incident (month/day/year)`>= "2019-1-1")
prop_2019 <- as.data.frame(year_2019 %>% count(Geography))
prop_2019$"2019" <- prop_2019$n/sum(prop_2019$n) 
prop_2019_sel <- prop_2019 %>% select(Geography, "2019")

geoprop_by_year <- Reduce(inner_join, list(prop_2013_sel, prop_2014_sel, prop_2015_sel, prop_2016_sel, 
                                           prop_2017_sel, prop_2018_sel, prop_2019_sel))
geoprop_year_long <- gather(geoprop_by_year, year, proportion, 2:8, factor_key = TRUE)

geoprop_year_long$year <- as.numeric(as.character(geo_year_long$year))
geoprop_year_long$Geography <- as.factor(geo_year_long$Geograp)


geoprop_year_graph <- geoprop_year_long %>% filter(proportion > 0.05) %>% ggplot(aes(y = proportion, x = year, color = Geography)) + 
  geom_point() + 
  geom_line() + 
  scale_x_continuous(breaks = seq(2012, 2019, 1)) + 
  scale_y_continuous(labels = scales::percent, limits = c(0, .55), breaks = seq(0,0.55, .1)) + 
  ggtitle("Police Violence broken down by Geography") + 
  labs( x = "Year", y = "Percent of Annual Victims", caption = "Source: Mapping Police Violence") + 
  theme(panel.background = element_rect("white"),
        axis.line = element_line(color = "black"),
        panel.grid.major.y = element_blank(),
        plot.title = element_text(size = 18, hjust = 0.5),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14), 
        plot.caption = element_text(face = "italic"))

jpeg(filename = "geogrpahy_proportion_year.jpeg", units = 'in', width = 8.0, height = 5.5, res = 1200, type = 'cairo')
plot(geoprop_year_graph)
dev.off()


mental_filtered_race <- c %>% filter(`Symptoms of mental illness?`== "Yes") %>%count(`Victim's race`)
sum(mental_filtered_race$n)
243/1467 #16% of mental health issue victims were blacka
841/1467 #57% of mental health issue victims were white

race_filtered_mental <- c %>% filter(`Victim's race`== "Black") %>%count(`Symptoms of mental illness?`) 
243/sum(race_filtered_mental$n) #13% of black victims had mental health issues 


#Armed Status
armed <- as.data.frame(c %>% count(Unarmed))
armed$proportion <- armed$n/sum(armed$n)
armed_status <- c("Armed", "Unarmed", "Unlcear", "Vehicle")
armed$Unarmed <- as.character(armed_status)


armed_plot <- armed %>% 
  ggplot(aes(y = n, x= Unarmed)) +
  geom_col(aes(fill = Unarmed), show.legend = FALSE) + 
  scale_y_continuous(breaks = seq(0,6000, 1000)) +
  ggtitle("Weapon possesion of Victim") + 
  labs( x = NULL, y = "Number of Victims", caption = "Source: Mapping Police Violence") + 
  scale_fill_viridis_d() + 
  theme(panel.background = element_rect("white"),
        axis.line = element_line(color = "black"),
        panel.grid.major.y = element_blank(),
        plot.title = element_text(size = 18, hjust = 0.5),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14), 
        plot.caption = element_text(face = "italic"))

jpeg(filename = "armed_totals.jpeg", units = 'in', width = 8.0, height = 5.5, res = 1200, type = 'cairo')
plot(armed_plot)
dev.off()

armed_plot_prop <- armed %>% 
  ggplot(aes(y = proportion, x= Unarmed)) +
  geom_col(aes(fill = Unarmed), show.legend = FALSE) + 
  scale_y_continuous(labels = scales::percent, breaks = seq(0,1, 0.1)) +
  ggtitle("Weapon possesion of Victim") + 
  labs( x = NULL, y = "Proportion of Victims", caption = "Source: Mapping Police Violence") + 
  scale_fill_viridis_d() + 
  theme(panel.background = element_rect("white"),
        axis.line = element_line(color = "black"),
        panel.grid.major.y = element_blank(),
        plot.title = element_text(size = 18, hjust = 0.5),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14), 
        plot.caption = element_text(face = "italic"))

jpeg(filename = "armed_totals_prop.jpeg", units = 'in', width = 8.0, height = 5.5, res = 1200, type = 'cairo')
plot(armed_plot_prop)
dev.off()

#Weapon Type
weapon <- as.data.frame(c %>% count(`Alleged Weapon (Source: WaPo)`))
weapon$proportion <- weapon$n/sum(weapon$n)
weapon_filtered <- weapon %>% filter(proportion >= 0.01)
sum(weapon_filtered$n)/sum(weapon$n)
weapon_edited <- data.frame(Weapon = c("Gun", "Knife", "Toy", "Vehicle", "Unknown Weapon", "Unarmed", "Undetermined"), 
                            Deaths = c(3961, 1002, 209, 515, 91, 822, 491))
weapon_edited$Proportion <- weapon_edited$Deaths/sum(weapon_edited$Deaths)

weapon_plot <- weapon_edited %>%
  ggplot(aes(y = Deaths, x= Weapon)) +
  geom_col(aes(fill = Weapon), show.legend = FALSE) + 
  scale_y_continuous(breaks = seq(0,4000, 500)) +
  ggtitle("Weapon possesion of Victim") + 
  labs( x = NULL, y = "Number of Weapons Possessed by Victims", caption = "Source: Mapping Police Violence") + 
  scale_fill_viridis_d() + 
  theme(panel.background = element_rect("white"),
        axis.line = element_line(color = "black"),
        panel.grid.major.y = element_blank(),
        plot.title = element_text(size = 18, hjust = 0.5),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14), 
        axis.text.x = element_text(angle = 30, vjust = 0.5),
        plot.caption = element_text(face = "italic"))

jpeg(filename = "type_weapon.jpeg", units = 'in', width = 8.0, height = 5.5, res = 1200, type = 'cairo')
plot(weapon_plot)
dev.off()

weapon_prop_plot <- weapon_edited %>% 
  ggplot(aes(y = Proportion, x= Weapon)) +
  geom_col(aes(fill = Weapon), show.legend = FALSE) + 
  scale_y_continuous(labels = scales::percent, breaks = seq(0,1, 0.1)) +
  ggtitle("Weapon possesion of Victim") + 
  labs( x = NULL, y = "Proportion of Weapon Type Posessed by Victims", caption = "Source: Mapping Police Violence") + 
  scale_fill_viridis_d() + 
  theme(panel.background = element_rect("white"),
        axis.line = element_line(color = "black"),
        panel.grid.major.y = element_blank(),
        plot.title = element_text(size = 18, hjust = 0.5),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        axis.text.x = element_text(angle = 30, vjust = 0.5),
        plot.caption = element_text(face = "italic"))

jpeg(filename = "type_weapon_prop.jpeg", units = 'in', width = 8.0, height = 5.5, res = 1200, type = 'cairo')
plot(weapon_prop_plot)
dev.off()

#Threat Assesment

Threat <- as.data.frame(c %>% count(`Alleged Threat Level (Source: WaPo)`))
threat_list <- c("Attacked", "Other", "Undetermined")
threat_edited <- data.frame("Threat Level" = c("Attacked", "Other", "Undetermined"), 
                            Incidence = c(3394, 1596, 291))
threat_edited$proportion <- threat_edited$Incidence/sum(threat_edited$Incidence)

threat_plot <- threat_edited %>%
  ggplot(aes(y = Incidence, x= Threat.Level)) +
  geom_col(aes(fill = Threat.Level), show.legend = FALSE) + 
  scale_y_continuous(breaks = seq(0,3500, 500)) +
  ggtitle("Threat Level of the Victim") + 
  labs( x = NULL, y = "Number of Incidence for Each Threat Level", caption = "Source: Mapping Police Violence") + 
  scale_fill_viridis_d() + 
  theme(panel.background = element_rect("white"),
        axis.line = element_line(color = "black"),
        panel.grid.major.y = element_blank(),
        plot.title = element_text(size = 18, hjust = 0.5),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14), 
        plot.caption = element_text(face = "italic"))

jpeg(filename = "threat_totals.jpeg", units = 'in', width = 8.0, height = 5.5, res = 1200, type = 'cairo')
plot(threat_plot)
dev.off()

threat_plot_prop <- threat_edited %>%
  ggplot(aes(y = proportion, x= Threat.Level)) +
  geom_col(aes(fill = Threat.Level), show.legend = FALSE) + 
  scale_y_continuous(breaks = seq(0,1, 0.1), labels = scales::percent) +
  ggtitle("Threat Level Displayed by the Victim") + 
  labs( x = NULL, y = "Number of Incidence for Each Threat Level", caption = "Source: Mapping Police Violence") + 
  scale_fill_viridis_d() + 
  theme(panel.background = element_rect("white"),
        axis.line = element_line(color = "black"),
        panel.grid.major.y = element_blank(),
        plot.title = element_text(size = 18, hjust = 0.5),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14), 
        plot.caption = element_text(face = "italic"))

jpeg(filename = "threat_totals.jpeg", units = 'in', width = 8.0, height = 5.5, res = 1200, type = 'cairo')
plot(threat_plot_prop)
dev.off()
