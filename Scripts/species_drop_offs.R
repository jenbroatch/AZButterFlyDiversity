# Maxine Cruz
# tmcruz@arizona.edu
# Created: 1 December 2023
# Last modified: 1 December 2023




# ----- ABOUT -----

# Figure for Discussions

# Make list of all species in first five years of study time frame
  # Similarly, make list for last five years
  # Find which species are no longer in list during last five years
  # Which species drop off each year within those last five years




# ----- LOAD LIBRARIES -----

library(dplyr)




# ----- LOAD DATA -----

data <- read.csv("data/TotalButterflyWithFamily.csv")




# ----- ORGANIZE DATA -----

# Earliest year with spring samples: 2011
# Latest year: 2021

# New object with data from first 2 or 3 years
F_five <- data %>%
  filter(Year == c(2011, 2012, 2013)) %>%
  select(Year, LatinAnalysisName, NABAEnglishName) %>%
  arrange(Year)

# Another new object with data from last 2 or 3 years
L_five <- data %>%
  filter(Year == c(2019, 2020, 2021)) %>%
  select(Year, LatinAnalysisName, NABAEnglishName) %>%
  arrange(Year)




# ----- COMPARE RICHNESS: FIRST 5 YEARS V. LAST 5 YEARS -----

# All species in first 5 years to run comparison against
F5_spp <- unique(F_five$NABAEnglishName)

# All species in last 5 years
L5_spp <- unique(L_five$NABAEnglishName)

# Find species that drop off in all the last 5 years
total_lost_spp <- as.data.frame(setdiff(F5_spp, L5_spp))

# Rename column just because
colnames(total_lost_spp)[1] <- "Species_Lost"




# ----- COMPARE RICHNESS: FIRST 5 YEARS V. YEAR (X) IN LAST 5 YEARS -----

# List years in last 5 years for loop
year_list <- sort(unique(L_five$Year))

# New data frame to store results
lost_species <- data.frame()

# All species in first five years v. Species from year x in last five years
# Which species was not recorded in year x, but was during first five years?
for (i in 1:5) {
  
  # Select year to compare
  year <- year_list[i]
  
  # Filter that year from last five years data frame
  select_year <- L_five %>%
    filter(Year == year)
  
  # Get unique species from that year
  spp <- unique(select_year$NABAEnglishName)
  
  # Compare to first five years and return lost species
  # (Values in x that don't have a match in y are returned)
  lost_spp <- setdiff(F5_spp, spp)
  
  # Store those results in a data frame
  df <- data.frame(Year = year,
                   Species_Lost = lost_spp)
  
  # Append to main data frame
  lost_species <- rbind(lost_species, df)
  
}

# Which species are lost for more than 1 year in the last 5 years?
lost_lost <- lost_species %>%
  group_by(Species_Lost) %>%
  filter(n() > 1) %>%
  distinct(Species_Lost)




# ----- COMPARE RICHNESS: YEAR (X) V. YEAR (X+1) IN LAST 5 YEARS -----

# Data frame to store these results
lost_species_L5 <- data.frame()

# Find species that drop off each year in last 5 years
# E.g. 2017 species v. 2018 species, which were lost? Repeat through remaining.
for (i in 1:4) {
  
  # Select first year
  year1 <- year_list[i]
  
  # Select second year
  year2 <- year_list[i+1]
  
  # Filter for year1
  select_year_1 <- L_five %>%
    filter(Year == year1)
  
  # Filter for year2
  select_year_2 <- L_five %>%
    filter(Year == year2)
  
  # Get species from first year
  spp1 <- unique(select_year_1$LatinAnalysisName)
  
  # Get species from second year
  spp2 <- unique(select_year_2$LatinAnalysisName)
  
  # Compare species between years and return lost species
  lost_spp <- setdiff(spp1, spp2)
  
  # Create name for year comparison (for data frame)
  comp_name <- paste(as.character(year1), "_", as.character(year2), sep = "")
  
  # Store those results in a data frame
  df <- data.frame(Year = comp_name,
                   Species_Lost = lost_spp)
  
  # Append to main data frame
  lost_species_L5 <- rbind(lost_species_L5, df)
  
}

# Which species are lost for more than 1 year in the last 5 years?
lost_lost2 <- lost_species_L5 %>%
  group_by(Species_Lost) %>%
  filter(n() > 1) %>%
  distinct(Species_Lost)



