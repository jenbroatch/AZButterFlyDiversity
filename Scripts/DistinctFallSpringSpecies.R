# Maxine Cruz
# tmcruz@arizona.edu
# Created: 5 December 2023
# Last modified: 7 December 2023




# ----- ABOUT -----

# For finding species that are exclusive to summer/fall
  # And species exclusive to spring




# ----- LOAD LIBRARIES -----

library(dplyr)
library(gt)




# ----- LOAD DATA -----

# For all butterfly data
data <- read.csv("DataSets/TotalButterflyWithFamily.csv") # 13378 observations
data <- data %>%
  filter(Year %in% c(2019, 2020, 2021)) %>%
  distinct() %>%
  arrange(Site, Year, Month, Day) # 1484 observations

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
         Day = day) %>%
  filter(Year %in% c(2019, 2020, 2021)) %>%
  arrange(Site, Year, Month, Day)

# Match and attach season designation to main data
data <- merge(data, seasons, all.x = TRUE) %>%
  arrange(Site, Year, Month, Day) %>%
  mutate(Season_Sampled = ifelse(Site == "SantaRitaMountains",
                                 "Summer/Fall",
                                 Season_Sampled))




# ----- FIND SPECIES ONLY IN FALL V. ONLY IN SPRING -----

# Separate fall data
fall <- data %>%
  filter(Season_Sampled == "Summer/Fall") %>%
  filter(Year %in% c(2019, 2020, 2021)) %>%
  select(6, 12, 13, 14) %>%
  distinct()

# Separate spring data
spring <- data %>%
  filter(Season_Sampled == "Spring") %>%
  filter(Year %in% c(2019, 2020, 2021)) %>%
  select(6, 12, 13, 14) %>%
  distinct()

# Find distinct fall species
dist_fall <- anti_join(fall, spring, by = "LatinAnalysisName")

# Find distinct spring species
dist_spring <- anti_join(spring, fall, by = "LatinAnalysisName")

# FALL PLOTS --

# Plot table of ALL fall species
fall |>
  group_by(Season_Sampled) |>
  arrange(Family, LatinAnalysisName, NABAEnglishName) |>
  distinct() |>
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
  gtsave("all_fall_species.html")

# Save as csv too
table <- fall %>%
  select(-4) %>%
  arrange(Family, LatinAnalysisName, NABAEnglishName) %>%
  rename("Scientific Name" = LatinAnalysisName,
         "Common Name" = NABAEnglishName) 

write.csv(table, "all_fall_species.csv", row.names = FALSE)

# Plot table of DISTINCT fall species (only found in fall)
dist_fall |>
  group_by(Season_Sampled) |>
  arrange(Family, LatinAnalysisName, NABAEnglishName) |>
  distinct() |>
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

# Save as csv too
table <- dist_fall %>%
  select(-4) %>%
  arrange(Family, LatinAnalysisName, NABAEnglishName) %>%
  rename("Scientific Name" = LatinAnalysisName,
         "Common Name" = NABAEnglishName)

write.csv(table, "distinct_fall_spp.csv", row.names = FALSE)

# SPRING PLOTS --

# Plot table of ALL spring species
spring |>
  group_by(Season_Sampled) |>
  arrange(Family, LatinAnalysisName, NABAEnglishName) |>
  distinct() |>
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
  gtsave("all_spring_species.html")

# Save as csv too
table <- spring %>%
  select(-4) %>%
  arrange(Family, LatinAnalysisName, NABAEnglishName) %>%
  rename("Scientific Name" = LatinAnalysisName,
         "Common Name" = NABAEnglishName)

write.csv(table, "all_spring_species.csv", row.names = FALSE)

# Plot table of DISTINCT spring species (only found in spring)
dist_spring |>
  group_by(Season_Sampled) |>
  arrange(Family, LatinAnalysisName, NABAEnglishName) |>
  distinct() |>
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

# Save as csv too
table <- dist_spring %>%
  select(-4) %>%
  arrange(Family, LatinAnalysisName, NABAEnglishName) %>%
  rename("Scientific Name" = LatinAnalysisName,
         "Common Name" = NABAEnglishName)

write.csv(table, "distinct_spring_spp.csv", row.names = FALSE)
