# Maxine Cruz
# tmcruz@arizona.edu
# Created: 11 December 2023
# Last modified: 11 December 2023




# ----- ABOUT -----

# Creates Table S3




# ----- LOAD LIBRARIES -----

library(dplyr)




# ----- LOAD DATA -----

# For final Table S3
names <- read.csv("DataSets/NameList.csv")

# ---

# For additional columns of S3

# For all butterfly data
data <- read.csv("DataSets/TotalButterflyWithFamily.csv") # 13378 observations

# Fall samples
fall <- read.csv("DataSets/butterfly_analysis_fall.csv")
fall$Season_Sampled <- "Summer/Fall"

# Spring samples
spring <- read.csv("DataSets/butterfly_analysis_spring.csv")
spring$Season_Sampled <- "Spring"

# ---

# For filtering fall /spring samples
seasons <- rbind(fall, spring) %>%
  select(1, 2, 3, 4, 41) %>%
  rename(Year = year,
         Month = month,
         Day = day) %>%
  arrange(Site, Year, Month, Day)

# Match and attach season designation to main data
data <- merge(data, seasons, all.x = TRUE) %>%
  arrange(Site, Year, Month, Day) %>%
  mutate(Season_Sampled = ifelse(Site == "SantaRitaMountains",
                                 "Summer/Fall",
                                 Season_Sampled))




# ----- FALL DROPPED SPECIES -----

# 2002-2006 v. most recent 3
# Leave out Sycamore Creek

# First years
F_spp <- data %>%
  filter(Site != "SycamoreCreekAZ") %>%
  filter(Season_Sampled == "Summer/Fall") %>%
  filter(Year %in% c(2002, 2003, 2004, 2005, 2006)) %>%
  select(Year, LatinAnalysisName, NABAEnglishName, Family, Season_Sampled) %>%
  arrange(Year)

# Last years
L_spp <- data %>%
  filter(Site != "SycamoreCreekAZ") %>%
  filter(Season_Sampled == "Summer/Fall") %>%
  filter(Year %in% c(2019, 2020, 2021)) %>%
  select(Year, LatinAnalysisName, NABAEnglishName, Family, Season_Sampled) %>%
  arrange(Year)

# COMPARE LAST YEARS V. X RECENT YEAR, 
# AND FIND WHICH SPECIES ARE GONE ALL RECENT YEARS --

# All species in first five years v. Species from year x in last three years
# Which species was not recorded in year x, but was during first five years?

# List years in last 3 years for loop
year_list <- sort(unique(L_spp$Year))

# New data frame to store results
fa_lost_species <- data.frame()

# L o o p
for (i in 1:3) {
  
  # Select year to compare
  year <- year_list[i]
  
  # Filter that year from last five years data frame
  select_year <- L_spp %>%
    filter(Year == year)
  
  # Get unique species from that year
  spp <- distinct(select_year, NABAEnglishName, .keep_all = TRUE)
  
  # Compare to first five years and return lost species
  # (Values in x that don't have a match in y are returned)
  lost_spp <- anti_join(F_spp, spp, by = "NABAEnglishName")
  
  # Store those results in a data frame
  df <- lost_spp %>%
    mutate(Lost_year = year) %>%
    distinct()
  
  # Append to main data frame
  fa_lost_species <- rbind(fa_lost_species, df)
  
}

# Which species are missing all recent years?
fa_dropped_all_recent <- fa_lost_species %>%
  select(-1) %>%
  distinct() %>%
  group_by(NABAEnglishName) %>%
  filter(n() == 3) %>%
  distinct(NABAEnglishName, .keep_all = TRUE) %>%
  select(-5) %>%
  arrange(Family, LatinAnalysisName, NABAEnglishName)




# ----- SPRING DROPPED SPECIES -----

# First years
F_spp <- data %>%
  filter(Season_Sampled == "Spring") %>%
  filter(Year %in% c(2011, 2012, 2013)) %>%
  select(Year, LatinAnalysisName, NABAEnglishName, Family, Season_Sampled) %>%
  arrange(Year)

# Last years
L_spp <- data %>%
  filter(Season_Sampled == "Spring") %>%
  filter(Year %in% c(2019, 2020, 2021)) %>%
  select(Year, LatinAnalysisName, NABAEnglishName, Family, Season_Sampled) %>%
  arrange(Year)

# COMPARE LAST YEARS V. X RECENT YEAR, 
# AND FIND WHICH SPECIES ARE GONE ALL RECENT YEARS --

# List years in last 3 years for loop
year_list <- sort(unique(L_spp$Year))

# New data frame to store results
sp_lost_species <- data.frame()

# All species in first five years v. Species from year x in last five years
# Which species was not recorded in year x, but was during first five years?
for (i in 1:3) {
  
  # Select year to compare
  year <- year_list[i]
  
  # Filter that year from last five years data frame
  select_year <- L_spp %>%
    filter(Year == year)
  
  # Get unique species from that year
  spp <- distinct(select_year, NABAEnglishName, .keep_all = TRUE)
  
  # Compare to first five years and return lost species
  # (Values in x that don't have a match in y are returned)
  lost_spp <- anti_join(F_spp, spp, by = "NABAEnglishName")
  
  # Store those results in a data frame
  df <- lost_spp %>%
    mutate(Lost_year = year) %>%
    distinct()
  
  # Append to main data frame
  sp_lost_species <- rbind(sp_lost_species, df)
  
}

# Which species are missing all recent years?
sp_dropped_all_recent <- sp_lost_species %>%
  select(-1) %>%
  distinct() %>%
  group_by(NABAEnglishName) %>%
  filter(n() == 3) %>%
  distinct(NABAEnglishName, .keep_all = TRUE) %>%
  select(-5) %>%
  arrange(Family, LatinAnalysisName, NABAEnglishName)




# ----- FORM TABLE S3 -----

# Combine all dropped species into one table
all_dropped <- rbind(fa_dropped_all_recent,
                     sp_dropped_all_recent)

# Start creating Table S3
s3 <- mutate(names,
             Missing2019to2021 = "",
             Season = "")

# If species was missing all three years between 2019-2021,
# List as "Yes" under Missing 2019to2021
s3$Missing2019to2021 <- 
  ifelse(names$NABAEnglishName %in% all_dropped$NABAEnglishName, "Yes", "")

# Attach which season that species is associated with
s3$Season <- ifelse(s3$Missing2019to2021 == "Yes",
                    all_dropped$Season_Sampled[match(names$NABAEnglishName, all_dropped$NABAEnglishName)],
                    "")
  
# Save file
write.csv(s3, "Outputs/table3_revision.csv", row.names = FALSE)  
  
  
  
  
  