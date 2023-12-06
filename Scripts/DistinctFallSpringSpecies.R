# Maxine Cruz
# tmcruz@arizona.edu
# Created: 5 December 2023
# Last modified: 5 December 2023




# ----- ABOUT -----

# For finding species that are exclusive to summer/fall
  # And species exclusive to spring




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




# ----- FIND SPECIES ONLY IN FALL V. ONLY IN SPRING -----

# Separate fall and spring again
fall <- data %>%
  filter(Season_Sampled == "Summer/Fall") %>%
  filter(Year %in% c(2019, 2020, 2021)) %>%
  select(6, 12, 13, 14)

spring <- data %>%
  filter(Season_Sampled == "Spring") %>%
  filter(Year %in% c(2019, 2020, 2021)) %>%
  select(6, 12, 13, 14)

# Find distinct fall species
dist_fall <- anti_join(fall, spring, by = "LatinAnalysisName")

# Organize
dist_fall <- dist_fall %>% 
  arrange(Family, LatinAnalysisName, NABAEnglishName) %>%
  distinct()

# Plot table of distinct fall species
dist_fall |>
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
  gtsave("distinct_fall_spp.html")

# Find distinct spring species
dist_spring <- anti_join(spring, fall, by = "LatinAnalysisName")

# Organize
dist_spring <- dist_spring %>% 
  arrange(Family, LatinAnalysisName, NABAEnglishName) %>%
  distinct()

# Plot table of distinct fall species
dist_spring |>
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
  gtsave("distinct_spring_spp.html")




