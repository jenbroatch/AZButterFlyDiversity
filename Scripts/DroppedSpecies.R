# Maxine Cruz
# tmcruz@arizona.edu
# Created: 1 December 2023
# Last modified: 5 December 2023




# ----- ABOUT -----

# Figure for Discussions

# Make list of all species in first five years of study time frame
  # Similarly, make list for last five years
  # Find which species are no longer in list during last five years
  # Which species drop off each year within those last five years




# ----- LOAD LIBRARIES -----

library(dplyr)
library(gt)




# ----- LOAD DATA -----

# For all butterfly data
data <- read.csv("DataSets/TotalButterflyWithFamily.csv") # 13378 observations

# Fall samples
fall <- read.csv("DataSets/butterfly_analysis_fall.csv")
fall$Season_Sampled <- "Summer/Fall"

# Spring samples
spring <- read.csv("DataSets/butterfly_analysis_spring.csv")
spring$Season_Sampled <- "Spring"

# For filtering fall /spring samples
seasons <- rbind(fall, spring) %>%
  select(1, 2, 3, 4, 41) %>%
  rename(Year = year,
         Month = month,
         Day = day)

# Match and attach season designation to main data
data <- merge(data, seasons, all.x = TRUE)




# ----- FALL DROPPED SPECIES -----

# 2002-2006 v. most recent 3
# Leave out Sycamore Creek

# First years
F_spp <- data %>%
  filter(Site != "SycamoreCreekAZ") %>%
  filter(Season_Sampled == "Summer/Fall") %>%
  filter(Year == c(2002, 2003, 2004, 2005, 2006)) %>%
  select(Year, LatinAnalysisName, NABAEnglishName, Family, Season_Sampled) %>%
  arrange(Year)

# Last years
L_spp <- data %>%
  filter(Site != "SycamoreCreekAZ") %>%
  filter(Season_Sampled == "Summer/Fall") %>%
  filter(Year == c(2019, 2020, 2021)) %>%
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
  df <- data.frame(Year = year,
                   Species_Lost = lost_spp)
  
  # Append to main data frame
  fa_lost_species <- rbind(fa_lost_species, df)
  
}

# Save as csv
write.csv(fa_lost_species, "fall_dropped_spp_3excSycamore.csv")

# Fix data frame
fa_lost_species <- fa_lost_species %>%
  select(-2) %>%
  rename(LatinAnalysisName = Species_Lost.LatinAnalysisName,
         NABAEnglishName = Species_Lost.NABAEnglishName,
         Family = Species_Lost.Family,
         Season_Sampled = Species_Lost.Season_Sampled) %>%
  arrange(Year, Family, LatinAnalysisName, NABAEnglishName)

# Which species are missing all recent years?
dropped_all_recent <- fa_lost_species %>%
  distinct() %>%
  group_by(NABAEnglishName) %>%
  filter(n() == 3) %>%
  distinct(NABAEnglishName, .keep_all = TRUE) %>%
  select(-1) %>%
  arrange(Family, LatinAnalysisName, NABAEnglishName)

# PLOT RESULTS --

# Having issues with gtsave() and gtsave_extra(), so I opened the html file
# and manually saved the webpage as an image using Web Capture (Edge)

dropped_all_recent |>
  group_by(Season_Sampled) |>
  gt() |>
  tab_style(style = list(cell_text(weight = "bold"),
                         cell_fill(color = "grey95")),
            locations = cells_row_groups()) |>
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_column_labels()) |>
  tab_style(style = cell_text(style = "italic"),
            locations = cells_body(columns = LatinAnalysisName)) |>
  cols_width(LatinAnalysisName ~ px(280),
             NABAEnglishName ~ px(250),
             Family ~ px(150)) |>
  cols_label(LatinAnalysisName = "Scientific Name",
             NABAEnglishName = "Common Name") |>
  gtsave("fa_spp_drops_3excSycamore.html")




# ----- SPRING DROPPED SPECIES -----

# First years
F_spp <- data %>%
  filter(Season_Sampled == "Spring") %>%
  filter(Year == c(2011, 2012, 2013)) %>%
  select(Year, LatinAnalysisName, NABAEnglishName, Family, Season_Sampled) %>%
  arrange(Year)

# Last years
L_spp <- data %>%
  filter(Season_Sampled == "Spring") %>%
  filter(Year == c(2019, 2020, 2021)) %>%
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
  df <- data.frame(Year = year,
                   Species_Lost = lost_spp)
  
  # Append to main data frame
  sp_lost_species <- rbind(sp_lost_species, df)
  
}

# Save as csv
write.csv(sp_lost_species, "spring_dropped_spp.csv")

# Re-organize
sp_lost_species <- sp_lost_species %>%
  select(-2) %>%
  rename(LatinAnalysisName = Species_Lost.LatinAnalysisName,
         NABAEnglishName = Species_Lost.NABAEnglishName,
         Family = Species_Lost.Family,
         Season_Sampled = Species_Lost.Season_Sampled) %>%
  arrange(Year, Family, LatinAnalysisName) %>%
  distinct()

# Which species are missing all recent years?
dropped_all_recent <- sp_lost_species %>%
  distinct() %>%
  group_by(NABAEnglishName) %>%
  filter(n() == 3) %>%
  distinct(NABAEnglishName, .keep_all = TRUE) %>%
  select(-1) %>%
  arrange(Family, LatinAnalysisName, NABAEnglishName)

# PLOT RESULTS --

# Having issues with gtsave() and gtsave_extra(), so I opened the html file
# and manually saved the webpage as an image using Web Capture (Edge)

sp_lost_species |>
  group_by(Season_Sampled) |>
  gt() |>
  tab_style(style = list(cell_text(weight = "bold"),
                         cell_fill(color = "grey95")),
            locations = cells_row_groups()) |>
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_column_labels()) |>
  tab_style(style = cell_text(style = "italic"),
            locations = cells_body(columns = LatinAnalysisName)) |>
  tab_style(style = cell_text(align = "left"),
            locations = list(cells_body(columns = Year),
                             cells_column_labels(columns = Year))) |>
  cols_width(Year ~ px(90),
             LatinAnalysisName ~ px(280),
             NABAEnglishName ~ px(250),
             Family ~ px(150)) |>
  cols_label(LatinAnalysisName = "Scientific Name",
             NABAEnglishName = "Common Name") |>
  gtsave("sp_yearly_spp_drops.html")




