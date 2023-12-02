# Data Analysis for Insects manuscript 
#Winter rains support butterfly diversity, but summer monsoon rainfall drives 
#fall post monsoon butterfly abundance in the arid southwest US 
# 12/1/2023 
# Jennifer Broatch

# LOAD LIBRARIES ---
library(tidyverse)
library(lubridate)
library(dplyr)
library(zoo)
library(lme4)
library(glme)
library(nlme)
library(regclass)
library(performance) #for VIF
library(patchwork)


# LOAD DATA ---
bfly_spring <- read.csv(file = "DataSets/butterfly_analysis_spring.csv")
bfly_fall <-read.csv(file = "DataSets/butterfly_analysis_fall.csv")


# MINOR DATA CORRECTION FOR TYPO ---

# Correcting the reporting mistake for the 2021 north rim sampling event of 126,
# party hours should be 26
bfly_fall["PartyHours"][bfly_fall["PartyHours"] == 126] <- 26

# Removing the grand canyon sampling events from the spring data set noted in Section 2.5
# For comparison purposes only 
bfly_spring2 <- subset(bfly_spring, Site!= 'GrandCanyonDesertView')
bfly_spring2 <- subset(bfly_spring2, Site!= 'GrandCanyonSouthRim')

bfly_fall2 <- subset(bfly_fall, Site!= 'GrandCanyonNorthRim')
bfly_fall3 <- subset(bfly_fall, Site!= 'McDowellSonoranPreserve')


## S5. Linear mixed model output table for fall survey butterfly abundance

# Fall model for total butterflies
model_fall1 = lmer(log(total_butterfly_count) ~ + year + 
                     tmin_previous30 +
                     tmax_previous30+
                     Mseason_precip +
                     Wseason_precip +
                     PartyHours +
                     (1|Site)  ,
                   data=bfly_fall,
                   REML = TRUE)

options(scipen=999)
summary(model_fall1)
ranef(model_fall1)
plot(model_fall1)
anova(model_fall1)

fallran <- ranef(model_fall1)


##  S6. General linear model output table for spring survey butterfly abundance
# Spring model for total butterflies
model_spring1 = lm(log(total_butterfly_count) ~ year + 
                     tmin_previous30 + 
                     tmax_previous30 +
                     Mseason_precip +
                     Wseason_precip +
                     PartyHours,
                   data=bfly_spring
)

summary(model_spring1) 
anova(model_spring1)

##  S7. General mixed  model output table for spring survey butterfly richness
# Spring model for unique butterflies

model_spring2 = lm(Unique_butterflies ~ year + 
                     tmin_previous30 + 
                     tmax_previous30 +
                     Mseason_precip +
                     Wseason_precip +
                     PartyHours,
                   data=bfly_spring)

summary(model_spring2) 


## S8. Linear mixed model output table for summer/fall survey butterfly richness. 
# Fall model for unique butterflies
model_fall2 = lmer(sqrt(Unique_butterflies) ~ + year + 
                     tmin_previous30 +
                     tmax_previous30+
                     Mseason_precip +
                     Wseason_precip +
                     PartyHours +
                     (1|Site),
                   data=bfly_fall,
                   REML = TRUE)

summary(model_fall2) 
plot(model_fall2)

## S9. Variance Inflation Factor (VIF) Output for each model. 
check_collinearity(model_fall1)
check_collinearity(model_fall2)
check_collinearity(model_spring1)
check_collinearity(model_spring2)

## MS Figures 3/4

fallab <- bfly_fall %>% 
  select(year, total_butterfly_count) %>% 
  group_by(year) %>% 
  summarise(fall_avg_abundance = mean(total_butterfly_count))

springab <- bfly_spring %>% 
  select(year, total_butterfly_count) %>% 
  group_by(year) %>% 
  summarise(spring_avg_abundance = mean(total_butterfly_count))

fallri <- bfly_fall %>% 
  select(year, Unique_butterflies) %>% 
  group_by(year) %>% 
  summarise(fall_avg_richness = mean(Unique_butterflies))

springri <- bfly_spring %>% 
  select(year, Unique_butterflies) %>% 
  group_by(year) %>% 
  summarise(spring_avg_richness = mean(Unique_butterflies))

# Merging the abundance data frames 
total_ab <- left_join(fallab, springab, by="year")

# Merging the richness data frames
totalri <- left_join(fallri, springri, by="year")

# Creating long data frames for plotting
long_abun <- gather(total_ab, Group, myValue, -1)
long_rich <- gather(totalri, Group, myValue, -1)



# Graphing the average abundance of the years
ggplot(data=long_abun, aes(x=year, y=myValue, color=Group))+
  geom_point() +
  geom_smooth(method = "lm", se =TRUE) +
  labs(x="Year", y="Average butterfly abundance", title="") +
  scale_color_discrete(
    labels= c("fall_avg_abundance" = "Summer/Fall **",
              "spring_avg_abundance" = "Spring ^"))

# Graphing the average  richness of the years
ggplot(data=long_rich, aes(x=year, y=myValue, color=Group))+
  geom_point() +
  geom_smooth(method = "lm", se =TRUE) +
  labs(x="Year", y="Average butterfly richness", title="") +
  scale_color_discrete(
    labels= c("fall_avg_richness" = "Summer/Fall",
              "spring_avg_richness" = "Spring **"))


## Figure S2. Distribution of mean butterfly abundance, richness, and party hours 
# Creating data frames for spring and fall dot plots showing abundance and richness for each site
falldot <- bfly_fall %>% 
  select(Site, total_butterfly_count, Unique_butterflies)

springdot <- bfly_spring %>% 
  select(Site, total_butterfly_count, Unique_butterflies)

# Renaming sites for easy reading
falldot$Site[falldot$Site=="McDowellSonoranPreserve"]<-"McDowell Sonoran Preserve"
falldot$Site[falldot$Site=="AtascosaHighlandsAZ"]<-"Atascosa Highlands"
falldot$Site[falldot$Site=="BoyceThompsonArboretum"]<-"Boyce Thompson Arboretum"
falldot$Site[falldot$Site=="GrandCanyonNorthRim"]<-"Grand Canyon North Rim"
falldot$Site[falldot$Site=="GrandCanyonSouthRim"]<-"Grand Canyon South Rim"
falldot$Site[falldot$Site=="PatagoniaAZ"]<-"Patagonia"
falldot$Site[falldot$Site=="PortalAZ"]<-"Portal"
falldot$Site[falldot$Site=="RamseyCanyonAZ"]<-"Ramsey Canyon"
falldot$Site[falldot$Site=="SabinoCanyonAZ"]<-"Sabino Canyon"
falldot$Site[falldot$Site=="SantaRitaMountains"]<-"Santa Rita Mountains"
falldot$Site[falldot$Site=="SycamoreCreekAZ"]<-"Sycamore Creek"

# Renaming spring sites for reading
springdot$Site[springdot$Site=="McDowellSonoranPreserve"]<-"McDowell Sonoran Preserve"
springdot$Site[springdot$Site=="GrandCanyonSouthRim"]<-"Grand Canyon South Rim"
springdot$Site[springdot$Site=="GrandCanyonDesertView"]<-"Grand Canyon Desert View"
springdot$Site[springdot$Site=="SabinoCanyonAZ"]<-"Sabino Canyon"

EnglishSites<- c("Cottonwood","McDowell Sonoran Preserve","Grand Canyon Desert View", "Grand Canyon South Rim","Sycamore Creek",
                 "Patagonia", "Ramsey Canyon","Boyce Thompson Arboretum","Grand Canyon North Rim", "Atacosa Highlands", 
                 "Sabino Canyon", "Portal","Santa Rita Mountains")


# CREATING ORGANIZED BOX PLOTS FOR FALL ---

# Fall abundance
f1 <- falldot %>% 
  mutate(Site = fct_relevel(
    Site,"Cottonwood","McDowell Sonoran Preserve", "Grand Canyon South Rim","Sycamore Creek",
    "Patagonia", "Ramsey Canyon","Boyce Thompson Arboretum","Grand Canyon North Rim", "Atascosa Highlands", 
    "Sabino Canyon", "Portal","Santa Rita Mountains" )) %>% 
  ggplot(aes(x=Site, y=total_butterfly_count))+
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 90))+
  labs(x="Site",y="Abundance")
f1

# Fall richness
f2 <- falldot %>% 
  mutate(Site = fct_relevel(
    Site,"Cottonwood","McDowell Sonoran Preserve", "Grand Canyon South Rim","Sycamore Creek",
    "Patagonia", "Ramsey Canyon","Boyce Thompson Arboretum","Grand Canyon North Rim", "Atascosa Highlands", 
    "Sabino Canyon", "Portal","Santa Rita Mountains" )) %>% 
  ggplot(aes(x=Site, y=Unique_butterflies))+
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 90))+
  labs(x="Site",y="Richness")
f2


# CREATING ORGANIZED BOX PLOTS FOR SPRING ---

# Spring abundance
s1 <- springdot %>% 
  mutate(Site = fct_relevel(
    Site,"McDowell Sonoran Preserve","Grand Canyon Desert View", "Grand Canyon South Rim",
    "Sabino Canyon" )) %>%
  ggplot(aes(x=Site, y=total_butterfly_count))+
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 90))+
  labs(x="Site",y="Abundance")

# Spring richness
s2 <- springdot %>% 
  mutate(Site = fct_relevel(
    Site,"McDowell Sonoran Preserve","Grand Canyon Desert View", "Grand Canyon South Rim",
    "Sabino Canyon" )) %>%
  ggplot(aes(x=Site, y=Unique_butterflies))+
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 90))+
  labs(x="Site",y="Richness")



# CREATING GRAPHS OF PARTY HOURS FOR EACH SITE ---
fallparty <- bfly_fall %>% 
  select(year, Site, PartyHours)

springparty <- bfly_spring %>% 
  select(year, Site, PartyHours)

fallparty$Site[fallparty$Site=="McDowellSonoranPreserve"]<-"McDowell Sonoran Preserve"
fallparty$Site[fallparty$Site=="AtascosaHighlandsAZ"]<-"Atascosa Highlands"
fallparty$Site[fallparty$Site=="BoyceThompsonArboretum"]<-"Boyce Thompson Arboretum"
fallparty$Site[fallparty$Site=="GrandCanyonNorthRim"]<-"Grand Canyon North Rim"
fallparty$Site[fallparty$Site=="GrandCanyonSouthRim"]<-"Grand Canyon South Rim"
fallparty$Site[fallparty$Site=="PatagoniaAZ"]<-"Patagonia"
fallparty$Site[fallparty$Site=="PortalAZ"]<-"Portal"
fallparty$Site[fallparty$Site=="RamseyCanyonAZ"]<-"Ramsey Canyon"
fallparty$Site[fallparty$Site=="SabinoCanyonAZ"]<-"Sabino Canyon"
fallparty$Site[fallparty$Site=="SantaRitaMountains"]<-"Santa Rita Mountains"
fallparty$Site[fallparty$Site=="SycamoreCreekAZ"]<-"Sycamore Creek"

springparty$Site[springparty$Site=="McDowellSonoranPreserve"]<-"McDowell Sonoran Preserve"
springparty$Site[springparty$Site=="GrandCanyonSouthRim"]<-"Grand Canyon South Rim"
springparty$Site[springparty$Site=="GrandCanyonDesertView"]<-"Grand Canyon Desert View"
springparty$Site[springparty$Site=="SabinoCanyonAZ"]<-"Sabino Canyon"

# Creating fall party hour box plot
fallpartyplot <- fallparty %>% 
  mutate(Site = fct_relevel(
    Site,"Cottonwood","McDowell Sonoran Preserve", "Grand Canyon South Rim","Sycamore Creek",
    "Patagonia", "Ramsey Canyon","Boyce Thompson Arboretum","Grand Canyon North Rim", "Atascosa Highlands", 
    "Sabino Canyon", "Portal","Santa Rita Mountains" )) %>% 
  ggplot(aes(x=Site, y=PartyHours))+
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 90))+
  labs(x="Site",y="Party hours")

# Creating spring party hour box plot
springpartyplot <- springparty %>% 
  mutate(Site = fct_relevel(
    Site,"McDowell Sonoran Preserve","Grand Canyon Desert View", "Grand Canyon South Rim",
    "Sabino Canyon" )) %>%
  ggplot(aes(x=Site, y=PartyHours))+
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 90))+
  labs(x="Site",y="Party hours")

## Supplementary Fig S1: 
f1 
f2 
fallpartyplot
s1
s2
springpartyplot









# Fall abundance boxplot
ggplot(falldot, aes(x=Site, y=total_butterfly_count))+
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 90))+
  labs(x="Site",y="Abundance")

# Fall richness boxplot
ggplot(falldot, aes(x=Site, y=Unique_butterflies))+
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 90))+
  labs(x="Site",y="Richness")

# Spring abundance boxplot  
ggplot(springdot, aes(x=Site, y=total_butterfly_count))+
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 90))+
  labs(x="Site",y="Abundance")

# Spring richness boxplot
ggplot(springdot, aes(x=Site, y=Unique_butterflies))+
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 90))+
  labs(x="Site",y="Richness")



# Spring model for total butterflies with no GC
model_springGC = lm(log(total_butterfly_count) ~ year + 
                      tmin_previous30 + 
                      tmax_previous30 +
                      Mseason_precip +
                      Wseason_precip +
                      PartyHours,
                    data=bfly_spring2
)
summary(model_springGC) 

# Spring model for unique butterflies with no GC
model_springGC2 = lm(log(Unique_butterflies) ~ year + 
                       tmin_previous30 + 
                       tmax_previous30 +
                       Mseason_precip +
                       Wseason_precip +
                       PartyHours,
                     data=bfly_spring2
)

summary(model_springGC2) 




