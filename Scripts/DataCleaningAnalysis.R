# 12/1/2023 
# Jennifer Broatch
# Data Cleaning from Site Sampling Data - updated data with family names 

# Creates: 
# butterfly_analysis_spring.csv
# butterfly_analysis_fall.csv 

#Creates Intermediate data sets with climate: 
# sites_winter_precipitation.csv
# sites_monsoon_precipitation.csv


# LOAD LIBRARIES ---
library(tidyverse)
library(lubridate)
library(dplyr)
library(zoo)

# LOAD DATA ---
#Updated 12/1 - Master file updated/cleaned names to prevent duplicates in analysis 
bfly<- read.csv(file = "DataSets/TotalButterflyWithFamily.csv")  

bfly <- bfly %>% 
  group_by(Year, Month, Day, Site) %>% 
filter (! duplicated(LatinAnalysisName))

bfly_analysis1 <- bfly %>% 
  group_by(Year, Month, Day, Site) %>% 
  summarize(total_butterfly_count = sum(ButterflyCount), 
            Unique_butterflies = n_distinct(NABAEnglishName)) 


#Add in party hours and number of parties to total and unique counts

bflyparty<- bfly %>% 
  select(Year, Month, Day, Site, PartyHours, X.Parties)
bfly_analysis <- left_join(bfly_analysis1, bflyparty, 
                            by =c('Year', 'Month', 'Day', 'Site'), multiple="first", keep=F)


#PRECIPITATION DATA ---

# Reading in the daily weather and butterfly summary csv
daily_weather <- read.csv("DataSets/sites_daily_weather.csv")


# Create a new data frame with all the rows from daily_weather and Butterfly Summary
Butterfly_daily_weather <- full_join(daily_weather, bfly_analysis,
                                    by =c("year"="Year", "month"="Month", "day"="Day", "Site"="Site"))

# Adding the previous 30, 90, and 365 day high/low/mean temp, and adding sum of the last 30/90/365 day precipitation 
Final_Butterly<- Butterfly_daily_weather %>% 
  group_by(Site) %>% 
  arrange(Site) %>%
  mutate(tmean_previous30=rollmean(tmean,30, na.pad = TRUE, align = "right")) %>% 
  mutate(tmax_previous30=rollmax(tmax,30, na.pad = TRUE, align = "right")) %>% 
  mutate(tmin_previous30=-rollmax(-tmin,30, na.pad = TRUE, align = "right")) %>%
  mutate(PrecipSum_previous30=rollsum(Precip,30, na.pad = TRUE, align = "right")) %>% 
  mutate(tmean_previous90=rollmean(tmean,90, na.pad = TRUE, align = "right")) %>% 
  mutate(tmax_previous90=rollmax(tmax,90, na.pad = TRUE, align = "right")) %>% 
  mutate(tmin_previous90=-rollmax(-tmin,90, na.pad = TRUE, align = "right")) %>%
  mutate(tmean_previous365=rollmean(tmean,365, na.pad = TRUE, align = "right")) %>% 
  mutate(tmax_previous365=rollmax(tmax,365, na.pad = TRUE, align = "right")) %>% 
  mutate(tmin_previous365=-rollmax(-tmin,365, na.pad = TRUE, align = "right")) %>%
  mutate(PrecipSum_previous90=rollsum(Precip,90, na.pad = TRUE, align = "right")) %>% 
  mutate(PrecipSum_previous365=rollsum(Precip,365, na.pad = TRUE, align = "right"))


# Creating winter precip data
initial_winter_precip <- Final_Butterly %>% 
  select(year, month, day, Site, Precip, tmean, tmax, tmin) %>% 
  group_by(Site, year, month) %>% 
  summarise(monthly_precip = sum(Precip), monthly_tmean = mean(tmean), monthly_tmax = max(tmax), 
            monthly_tmin = min(tmin))

# Deleting months that are not in winter season
winter_precip<- subset(initial_winter_precip, month!="5" & month!="6" & month!="7" & month!="8" & month!="9")


# Creating winter months of 10-12
winter_precip_firsthalf <- subset(
  winter_precip, month!="5" & month!="6" & month!="7" & month!="8" & month!="9" & month!="1" & month!="2"
  & month!="3" & month!="4")

# Combining months 10-12
winter_precip_firsthalf<- winter_precip_firsthalf %>% 
  mutate(PrecipSum_previous3=rollsum(monthly_precip,3, na.pad = TRUE, align = "right"))

# Adding 1 to each year to align with the second half of winter season
winter_precip_firsthalf$year<- winter_precip_firsthalf$year +1

# Creating winter months of 1-4
winter_precip_secondhalf<- subset(
  winter_precip, month!="5" & month!="6" & month!="7" & month!="8" & month!="9" & month!="10" & month!="11"
  & month!="12")

# Combining months 1-4
winter_precip_secondhalf<- winter_precip_secondhalf %>% 
  mutate(PrecipSum_previous4=rollsum(monthly_precip,4, na.pad = TRUE, align = "right"))

# Joining the two winter halves
Wseason_precip<- merge(x=winter_precip_firsthalf, y=winter_precip_secondhalf, 
                       by=c( "Site", "year", "month", "monthly_precip", "monthly_tmax", "monthly_tmin",
                             "monthly_tmean"), all = TRUE)

# Replacing NAs with 0 so rows can be added
Wseason_precip[is.na(Wseason_precip)]<-0

# Adding the two rows
Wseason_precip$Precip_total<- Wseason_precip$PrecipSum_previous3 + Wseason_precip$PrecipSum_previous4

# Removing unneeded columns
Wseason_precip <- subset(Wseason_precip, select = -c(PrecipSum_previous3, PrecipSum_previous4))

# Combining the two halves into 1 season precip
total_Wseason_precip <- Wseason_precip %>% 
  select(Site, year, Precip_total, monthly_tmean, monthly_tmax, monthly_tmin) %>% 
  group_by(Site, year) %>% 
  summarise(Wseason_precip = sum(Precip_total), Wseason_tmean = mean(monthly_tmean),
            Wseason_tmax = max(monthly_tmax), Wseason_tmin = min(monthly_tmin))

# Creating a csv of the yearly winter data
write_csv(x = total_Wseason_precip, 
          file = "DataSets/sites_winter_precipitation.csv")



# MONSOON PRECIPITATION DATA ---

# Creating monsoon precip data
monsoon_precip <- Final_Butterly %>% 
  select(year, month, day, Site, Precip) %>% 
  group_by(Site, year, month) %>% 
  summarise(monthly_precip = sum(Precip))

# Deleting months that are not in monsoon season
monsoon_precip<- subset(monsoon_precip, month!="1" & month!="2" & month!="3" & month!="4" & month!="5" & 
                          month!="6" & month!="10" & month!="11" & month!="12" )

# Adding the monsoon season months up for the year/site
monsoon_precip <- monsoon_precip %>% 
  select(Site, year, monthly_precip) %>% 
  group_by(Site, year) %>% 
  summarise(Mseason_precip = sum(monthly_precip))

# Creating the previous year monsoon precip
monsoon_precip<- monsoon_precip %>% 
  dplyr::mutate(previous_Mseason_precip = dplyr::lag(Mseason_precip, n = 1, default = NA))

# Creating monsoon temperature data
monsoon_temp <- Final_Butterly %>% 
  select(year, month, day, Site, tmin, tmean, tmax) %>% 
  group_by(Site, year, month) %>% 
  summarise(monthly_tmean = mean(tmean), monthly_tmin = min(tmin), monthly_tmax = max(tmax))

# Removing months not in monsoon season
monsoon_temp <- subset(monsoon_temp, month!="1" & month!="2" & month!="3" & month!="4" & month!="5" & 
                         month!="6" & month!="10" & month!="11" & month!="12" )

# Combing all the months for one monsoon season
monsoon_temp <- monsoon_temp %>% 
  select(year, month, Site, monthly_tmean, monthly_tmin, monthly_tmax) %>% 
  group_by(Site, year) %>% 
  summarise(Mseason_tmean = mean(monthly_tmean), Mseason_tmin = min(monthly_tmin), Mseason_tmax = max(monthly_tmax))

# Combining the monsoon temp and precip data
monsoon_all <- merge(x=monsoon_temp, y=monsoon_precip, by=c("Site", "year"), all = TRUE)

# WRriting the monsoon data to csv
write_csv(x = monsoon_all, 
          file = "DataSets/sites_monsoon_precipitation.csv")

# Joining the monsoon and winter data
Seasonal <- left_join(total_Wseason_precip, monsoon_all, by=c("Site", "year"))

# Joining the seasonal data to the butterfly data
seasonal_butterfly <- left_join(Final_Butterly, Seasonal, by=c("Site", "year"))


# Dropping days without a sampling event
sampling_events <-seasonal_butterfly %>% 
  drop_na(Unique_butterflies)

# Dropping sample events that do not have weather data i.e. prior to 1981
sampling_events <- sampling_events %>% 
  drop_na(tmin)

# Adding recent precip option 1
sampling_events <- mutate(sampling_events, recent_precip1 = 
                            ifelse(month%in% 3:7, sampling_events$Wseason_precip,
                                   ifelse(month%in% 8:9, sampling_events$PrecipSum_previous90,
                                          sampling_events$Mseason_precip)))

# Adding recent precip option 2
sampling_events <- mutate(sampling_events, recent_precip2 = 
                            ifelse(month%in% 3:8, sampling_events$Wseason_precip,
                                   ifelse(month%in% 9, sampling_events$PrecipSum_previous90,
                                          sampling_events$Mseason_precip)))

# Removing unneeded columns
sampling_events<- subset(sampling_events, select = -c(previous_Mseason_precip))

# Creating a csv of the sampling events
write_csv(x = sampling_events, 
          file = "DataSets/ButterFlyAnalysis.csv")


# SEPARATE DATA INTO FALL AND SPRING DATA ---

# Creating the fall DF
bfly_fall <- sampling_events %>% 
  filter(month > 6)

# Removing second sampling of Santa Rita Mountains
bfly_fall <- bfly_fall[!(bfly_fall$Site == 'SantaRitaMountains' & bfly_fall$month == 9),]
bfly_fall <- bfly_fall[!(bfly_fall$Site == 'SantaRitaMountains' & bfly_fall$month == 10),]

# Removing pre 7/15 sampling
bfly_fall <- bfly_fall[!(bfly_fall$Site == 'GrandCanyonNorthRim' & bfly_fall$day == 5),]
bfly_fall <- bfly_fall[!(bfly_fall$Site == 'SycamoreCreekAZ' & bfly_fall$day == 7),]

# Creating the spring data observations
bfly_spring <-sampling_events %>% 
  filter(month < 7)

# Removing post 6/15 sampling  
bfly_spring <- bfly_spring[!(bfly_spring$Site == 'RamseyCanyonAZ' & bfly_spring$month == 6),]
bfly_spring <- bfly_spring[!(bfly_spring$Site == 'AtascosaHighlandsAZ' & bfly_spring$month == 6),]

# Write files
write_csv(x = bfly_spring, 
          file = "DataSets/butterfly_analysis_spring.csv")

write_csv(x = bfly_fall, 
          file = "DataSets/butterfly_analysis_fall.csv")



