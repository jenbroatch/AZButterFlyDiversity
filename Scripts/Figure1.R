# Maxine Cruz
# tmcruz@arizona.edu
# Created: 21 September 2023
# Last modified: 4 December 2023




# ----- ABOUT THE SCRIPT -----

# Generates Figure 1 for AZNABA

# NOTES:
  # Make black and white
  # Have 3 shapes for season sampled (Fall, Spring, Both)
    # Black circle, white circle, black and white circle
  # Data frame headers:
    # Site no. | Site name | Lat | Long | Elevation | Season | Years | Times




# ----- LOAD LIBRARIES -----

library(ggplot2)
library(tidyverse)
library(cowplot)

# Using archived packages

# 1) ggsn (adds symbols and scale bars to ggplot)

# install.packages("https://cran.r-project.org/src/contrib/Archive/ggsn/ggsn_0.5.0.tar.gz", 
#                  type = "source", 
#                  repos = NULL)

library(ggsn)




# ----- LOAD DATA -----

# Coordinates and names of sample sites
bfly_data <- read.csv("DataSets/ButterflyAnalysis.csv")

# For filtering spring sample sites
spring_df <- read.csv("DataSets/butterfly_analysis_spring.csv")

# For filtering fall sample sites
fall_df <- read.csv("DataSets/butterfly_analysis_fall.csv")




# ----- DEAL WITH FALL SITES -----

# Subset columns from fall_data
fall_df <- select(fall_df, 
                  Site, Latitude, Longitude, Elevation, 
                  Year = year, 
                  Month = month, 
                  Day = day)

# Add column designating that these are summer/fall samples
fall_df$Season_Sampled <- "Summer / Fall"




# ----- DEAL WITH SPRING SITES -----

# Subset columns from spring_data
spring_df <- select(spring_df, 
                  Site, Latitude, Longitude, Elevation, 
                  Year = year, 
                  Month = month, 
                  Day = day)

# Add column designating that these are spring samples
spring_df$Season_Sampled <- "Spring"




# ----- ORGANIZE DATA FRAME FOR FIGURE -----

# Combine filtered fall and spring data into one data frame
both_szn <- rbind(fall_df, spring_df)

# GrandCanyonSouthRim, McDowellSonoranPreserve, and SabinoCanyonAZ have samples 
# from summer / fall and spring. 
# We want to change their Season_Sampled column to Both Seasons.
both_szn <- both_szn %>%
  mutate(Season_Sampled = 
           ifelse(Site == "GrandCanyonSouthRim" |
                    Site == "McDowellSonoranPreserve" |
                    Site == "SabinoCanyonAZ", 
                  "Both Seasons", # If Site is named one of the above, then this
                  Season_Sampled)) # Otherwise, keep original season

# Create data frame for assigning site numbers to the sites
nums <- data.frame(Site_Number = cbind(seq(1, 13)),
                   Site = c("Cottonwood",
                            "McDowellSonoranPreserve",
                            "GrandCanyonDesertView",
                            "GrandCanyonSouthRim",
                            "SycamoreCreekAZ",
                            "PatagoniaAZ",
                            "RamseyCanyonAZ",
                            "BoyceThompsonArboretum",
                            "GrandCanyonNorthRim",
                            "AtascosaHighlandsAZ",
                            "SabinoCanyonAZ",
                            "PortalAZ",
                            "SantaRitaMountains"))

# Merge site numbers to season sampled data
both_szn <- merge(both_szn, nums, all.y = TRUE) 

# Reorganize
both_szn <- both_szn %>%
  select(9, 1, 5, 6, 7, 2, 3, 4, 8) %>%
  arrange(Site_Number, Year, Month, Day)



# ----- PLOT US MAP -----

# Get state border lines
all_state <- map_data("state")

# Get Arizona border lines
az_lines <- filter(all_state, region == "arizona")

# Plot US map with Arizona highlighted
us_map <- ggplot(all_state, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "white", 
               color = "black") +
  geom_polygon(fill = "cornflowerblue", 
               color = "black",
               data = az_lines) +
  xlab("Longitude") +
  ylab("Latitude") +
  theme(axis.title.x = element_text(margin = margin(t = 10), size = 12),
        axis.title.y = element_text(margin = margin(r = 10), size = 12),
        panel.background = element_rect(fill = "grey95"),
        panel.grid.major = element_line(color = "black",
                                        size = 0.5,
                                        linetype = 2)) +
  scalebar(data = all_state,
           location = "bottomright",
           dist = 500,
           dist_unit = "km",
           transform = TRUE,
           model = "WGS84",
           st.size = 3,
           border.size = 0.5)

# Save as shown on Plots
ggsave(file = "fig_1_us_map.png",
       plot = us_map,
       width = 25,
       height = 15,
       units = "cm")




# ----- PLOT ARIZONA MAP -----

# Add coordinates for major cities (Flagstaff, Phoenix, and Tucson)
# Coordinates determined by Google search
az_cities <- data.frame(City = c("Flagstaff", "Phoenix", "Tucson"),
                        Longitude = c(-111.651299, -112.074036, -110.911789),
                        Latitude = c(35.198284, 33.448376, 32.253460))

# Plot Arizona map
az_map <- ggplot() +
  geom_point(data = both_szn,
             aes(x = Longitude, y = Latitude, color = Site)) +
  geom_polygon(data = az_lines,
               aes(x = long, y = lat, group = group),
               fill = "white",
               color = "black",
               linewidth = 0.5) + 
  geom_point(data = az_cities,
             aes(x = Longitude, y = Latitude),
             size = 2) + 
  geom_point(data = both_szn,
             aes(x = Longitude, y = Latitude, 
                 shape = Season_Sampled),
             size = 6) + 
  geom_text(data = both_szn,
            aes(x = Longitude, y = Latitude, label = Site_Number),
            color = "white",
            size = 3) + 
  geom_text(data = az_cities,
            aes(x = Longitude, y = Latitude, label = City),
            vjust = 0,
            nudge_y = -0.2,
            size = 3.5) + 
  xlab("Longitude") +
  ylab("Latitude") +
  scale_color_manual(
    name = "Site",
    breaks = c("Cottonwood",
               "McDowellSonoranPreserve",
               "GrandCanyonDesertView",
               "GrandCanyonSouthRim",
               "SycamoreCreekAZ",
               "PatagoniaAZ",
               "RamseyCanyonAZ",
               "BoyceThompsonArboretum",
               "GrandCanyonNorthRim",
               "AtascosaHighlandsAZ",
               "SabinoCanyonAZ",
               "PortalAZ",
               "SantaRitaMountains"),
    values = c("Cottonwood" = "grey13", 
               "McDowellSonoranPreserve" = "grey12",
               "GrandCanyonDesertView" = "grey11", 
               "GrandCanyonSouthRim" = "grey10",
               "SycamoreCreekAZ" = "grey9", 
               "PatagoniaAZ" = "grey8",
               "RamseyCanyonAZ" = "grey7", 
               "BoyceThompsonArboretum" = "grey6", 
               "GrandCanyonNorthRim" = "grey5",
               "AtascosaHighlandsAZ" = "grey4", 
               "SabinoCanyonAZ" = "grey3",
               "PortalAZ" = "grey2", 
               "SantaRitaMountains" = "grey1"),
    labels = c("1 - Cottonwood", 
               "2 - McDowell Sonoran Preserve",
               "3 - Grand Canyon Desert View", 
               "4 - Grand Canyon South Rim",
               "5 - Sycamore Creek", 
               "6 - Patagonia",
               "7 - Ramsey Canyon", 
               "8 - Boyce Thompson Arboretum", 
               "9 - Grand Canyon North Rim",
               "10 - Atascosa Highlands", 
               "11 - Sabino Canyon",
               "12 - Portal", 
               "13 - Santa Rita Mountains") 
  ) +
  guides(shape = guide_legend(title = "Season Sampled",
                              override.aes = list(size = 4)),
         color = guide_legend(title = "Site Name",
                              override.aes = list(shape = NA,
                                                  size = 3))) +
  theme(axis.title.x = element_text(margin = margin(t = 10), size = 12),
        axis.title.y = element_text(margin = margin(r = 10), size = 12),
        legend.position = "right",
        legend.key = element_rect(fill = "white", color = "transparent"),
        legend.title = element_text(face = "bold"),
        panel.background = element_rect(fill = "grey95"),
        panel.grid.major = element_line(color = "black",
                                        size = 0.5,
                                        linetype = 2)) +
  scalebar(data = az_lines,
           location = "bottomleft",
           dist = 100,
           dist_unit = "km",
           transform = TRUE,
           model = "WGS84",
           st.size = 3,
           border.size = 0.5)

# Save as shown on Plots
ggsave(file = "fig_1_az_map.png",
       plot = az_map,
       width = 20, 
       height = 16,
       units = "cm")




# ----- COMBINE MAPS -----

# Combine US and AZ maps
plot_row <- cowplot::plot_grid(us_map, az_map)

# Save as shown on Plots
ggsave(file = "fig_1_us_az_map.png",
       plot = plot_row,
       width = 40,
       height = 15,
       units = "cm")

# The overlay was achieved by arranging the individual plots on Microsoft
# Powerpoint and then screenshotting the final product. The save above gives
# the US and Arizona maps plotted side-by-side.



