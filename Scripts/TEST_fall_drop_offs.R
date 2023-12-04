# Maxine Cruz
# tmcruz@arizona.edu
# Created: 4 December 2023
# Last modified: 4 December 2023




# ----- ABOUT -----

# For testing summer / fall species drops
  
  # First 5 v. recent 5 years, excluding Atascosa
    # fa_spp_drops_5excAtascosa.jpeg

  # First 5 v. recent 3 years, excluding Atascosa
    # fa_spp_drops_3excAtascosa.jpeg

  # First 5 v. recent 5 years, excluding Sycamore
    # fa_spp_drops_5excSycamore.jpeg

  # First 5 v. recent 3 years, excluding Sycamore
    # fa_spp_drops_3excSycamore.jpeg




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




# ----- 1986-1990 v. MOST RECENT 5 -----

# PREP DATA --

# Leave out Atascosa Highlands

# First years
F_spp <- data %>%
  filter(Site != "AtascosaHighlandsAZ") %>%
  filter(Season_Sampled == "Summer/Fall") %>%
  filter(Year == c(1986, 1987, 1988, 1989, 1990)) %>%
  select(Year, LatinAnalysisName, NABAEnglishName, Family, Season_Sampled) %>%
  arrange(Year)

# Last years
L_spp <- data %>%
  filter(Site != "AtascosaHighlandsAZ") %>%
  filter(Season_Sampled == "Summer/Fall") %>%
  filter(Year == c(2017, 2018, 2019, 2020, 2021)) %>%
  select(Year, LatinAnalysisName, NABAEnglishName, Family, Season_Sampled) %>%
  arrange(Year)

# COMPARE RICHNESS --

# All species in first 5 years to run comparison against
F_spp <- F_spp %>% 
  distinct(NABAEnglishName, .keep_all = TRUE)

# All species in last 5 years
L_spp <- L_spp %>% 
  distinct(NABAEnglishName, .keep_all = TRUE)

# Find species that drop off in all the last 5 years
fa_lost_spp <- anti_join(F_spp, L_spp, by = "NABAEnglishName")

# Remove year column
fa_lost_spp <- select(fa_lost_spp, -1)

# Organize table
fa_lost_spp <- arrange(fa_lost_spp,
                       Family, LatinAnalysisName, NABAEnglishName)

# PLOT RESULTS --

fa_lost_spp |>
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
  gtsave("fa_spp_drops_5excAtascosa.html")

# COMPARE LAST YEARS V. X RECENT YEAR, 
# AND FIND WHICH SPECIES ARE GONE ALL RECENT YEARS --

# All species in first five years v. Species from year x in last five years
# Which species was not recorded in year x, but was during first five years?

# List years in last 5 years for loop
year_list <- sort(unique(L_spp$Year))

# New data frame to store results
fa_lost_species <- data.frame()

# L o o p
for (i in 1:5) {
  
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
  filter(n() == 5) %>%
  distinct(NABAEnglishName, .keep_all = TRUE) %>%
  select(-1)

# PLOT RESULTS --

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
  gtsave("fa_spp_drops_5excAtascosa.html")




# ----- 1986-1990 v. MOST RECENT 3 -----

# PREP DATA --

# Leave out Atascosa Highlands

# First years
F_spp <- data %>%
  filter(Site != "AtascosaHighlandsAZ") %>%
  filter(Season_Sampled == "Summer/Fall") %>%
  filter(Year == c(1986, 1987, 1988, 1989, 1990)) %>%
  select(Year, LatinAnalysisName, NABAEnglishName, Family, Season_Sampled) %>%
  arrange(Year)

# Last years
L_spp <- data %>%
  filter(Site != "AtascosaHighlandsAZ") %>%
  filter(Season_Sampled == "Summer/Fall") %>%
  filter(Year == c(2019, 2020, 2021)) %>%
  select(Year, LatinAnalysisName, NABAEnglishName, Family, Season_Sampled) %>%
  arrange(Year)

# COMPARE RICHNESS

# All species in first 5 years to run comparison against
F_spp <- F_spp %>% 
  distinct(NABAEnglishName, .keep_all = TRUE)

# All species in last 5 years
L_spp <- L_spp %>% 
  distinct(NABAEnglishName, .keep_all = TRUE)

# Find species that drop off in all the last 3 years
fa_lost_spp <- anti_join(F_spp, L_spp, by = "NABAEnglishName")

# Remove year column
fa_lost_spp <- select(fa_lost_spp, -1)

# Organize table
fa_lost_spp <- arrange(fa_lost_spp,
                       Family, LatinAnalysisName, NABAEnglishName)

# PLOT RESULTS --

fa_lost_spp |>
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
  gtsave("fa_spp_drops_3excAtascosa.html")

# COMPARE LAST YEARS V. X RECENT YEAR, 
# AND FIND WHICH SPECIES ARE GONE ALL RECENT YEARS --

# All species in first five years v. Species from year x in last three years
# Which species was not recorded in year x, but was during first five years?

# List years in last 5 years for loop
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
  select(-1)

# PLOT RESULTS --

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
  gtsave("fa_spp_drops_3excAtascosa.html")




# ----- 2002-2006 v. MOST RECENT 5 -----

# PREP DATA --

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
  filter(Year == c(2017, 2018, 2019, 2020, 2021)) %>%
  select(Year, LatinAnalysisName, NABAEnglishName, Family, Season_Sampled) %>%
  arrange(Year)

# COMPARE RICHNESS

# All species in first 5 years to run comparison against
F_spp <- F_spp %>% 
  distinct(NABAEnglishName, .keep_all = TRUE)

# All species in last 5 years
L_spp <- L_spp %>% 
  distinct(NABAEnglishName, .keep_all = TRUE)

# Find species that drop off in all the last 5 years
fa_lost_spp <- anti_join(F_spp, L_spp, by = "NABAEnglishName")

# Remove year column
fa_lost_spp <- select(fa_lost_spp, -1)

# Organize table
fa_lost_spp <- arrange(fa_lost_spp,
                       Family, LatinAnalysisName, NABAEnglishName)

# PLOT RESULTS --

fa_lost_spp |>
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
  gtsave("fa_spp_drops_5excSycamore.html")

# COMPARE LAST YEARS V. X RECENT YEAR, 
# AND FIND WHICH SPECIES ARE GONE ALL RECENT YEARS --

# All species in first five years v. Species from year x in last five years
# Which species was not recorded in year x, but was during first five years?

# List years in last 5 years for loop
year_list <- sort(unique(L_spp$Year))

# New data frame to store results
fa_lost_species <- data.frame()

# L o o p
for (i in 1:5) {
  
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
  filter(n() == 5) %>%
  distinct(NABAEnglishName, .keep_all = TRUE) %>%
  select(-1)

# PLOT RESULTS --

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
  gtsave("fa_spp_drops_5excSycamore.html")



# ----- 2002-2006 v. MOST RECENT 3 -----

# PREP DATA --

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

# ------------------- probably delete

# COMPARE RICHNESS

# All species in first 5 years to run comparison against
F_spp <- F_spp %>% 
  distinct(NABAEnglishName, .keep_all = TRUE)

# All species in last 3 years
L_spp <- L_spp %>% 
  distinct(NABAEnglishName, .keep_all = TRUE)

# Find species that drop off in all the last 5 years
fa_lost_spp <- anti_join(F_spp, L_spp, by = "NABAEnglishName")

# Remove year column
fa_lost_spp <- select(fa_lost_spp, -1)

# Organize table
fa_lost_spp <- arrange(fa_lost_spp,
                       Family, LatinAnalysisName, NABAEnglishName)

# PLOT RESULTS --

fa_lost_spp |>
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

# -------------------

# COMPARE LAST YEARS V. X RECENT YEAR, 
# AND FIND WHICH SPECIES ARE GONE ALL RECENT YEARS --

# All species in first five years v. Species from year x in last three years
# Which species was not recorded in year x, but was during first five years?

# List years in last 5 years for loop
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
  select(-1)

# PLOT RESULTS --

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



