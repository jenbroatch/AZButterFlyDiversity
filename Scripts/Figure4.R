# Maxine Cruz
# tmcruz@arizona.edu
# Created: 21 September 2023
# Last modified: 3 December 2023




# ----- ABOUT THE SCRIPT -----

# Generates Figure 4 for AZNABA

# FIGURE NOTES:
  # 8 panels of regressions
  # 2 rows (4 panels each row)
    # One row is abundance and the other is richness
    # 4 panels are abundance/richness v.
      # Monsoon precipitation
      # Winter precipitation
      # 30-day Maximum Temperature (prior to sampling event)
      # 30-day Minimum Temperature (prior to sampling event)
  # Data frame headers:
  # Site no. | Predictor | Response | Predictor Value | Response Value | Season
  # facet_wrap(response ~ predictor)

# Also faceted further to group by season sampled

# NOTES FOR MYSELF:
  # Richness: Number of unique species within study area
  # Abundance: Number of individuals for each species within study area




# ----- LOAD LIBRARIES -----

library(tidyverse)
library(ggplot2)

# Extension to create nested facets
library(ggh4x)




# ----- LOAD DATA -----

# Butterfly, monsoons, and temperature data
bfly_df <- read.csv("DataSets/ButterflyAnalysis.csv")

# For filtering spring sample sites
spring <- read.csv("DataSets/butterfly_analysis_spring.csv")

# For filtering fall sample sites
fall <- read.csv("DataSets/butterfly_analysis_fall.csv")




# ----- ORGANIZE DATA FRAME FOR FIGURE -----

# Subset columns from fall
fall_df <- select(fall, 
                  Site, Latitude, Longitude, Elevation, 
                  Year = year, 
                  Month = month, 
                  Day = day)

# Add column designating that these are summer/fall samples
fall_df$Season_Sampled <- "Summer / Fall"

# Subset columns from spring
spring_df <- select(spring, 
                    Site, Latitude, Longitude, Elevation, 
                    Year = year, 
                    Month = month, 
                    Day = day)

# Add column designating that these are spring samples
spring_df$Season_Sampled <- "Spring"

# Combine filtered fall and spring data into one data frame
both_szn <- rbind(fall_df, spring_df)

# Sort both_szn
both_szn <- arrange(both_szn, Site, Year, Month, Day)

# Combine number assignment column to both_szn
both_szn <- merge(both_szn, site_coords, all.y = TRUE)

# Reorganize and select column order in data frame
both_szn <- select(both_szn,
                   9, 1, 5, 6, 7, 8)

# Keep necessary variables from ButterflyAnalysis.csv
bfly_df <- select(bfly_df,
                  4, 1, 2, 3, 15, 16, 20, 21, 31, 38)

# NOTES:
  # 4 = Site
  # 1 = Sample year
  # 2 = Sample month
  # 3 = Sample day
  # 15 = total_butterfly_count = Abundance
  # 16 = Unique_butterflies = Richness
  # 20 = tmax_previous30 = 30-day max. temperature prior to sampling event
  # 21 = tmin_previous30 = 30-day min. temperature prior to sampling event
  # 31 = Wseason_precip = Winter season precipitation
  # 38 = Mseason_precip = Monsoon (Summer) season precipitation

# Rename some columns in both_szn so that date merging happens properly
both_szn <- rename(both_szn, 
                   year = Year,
                   month = Month,
                   day = Day)

# Merge Season_Sampled with the bfly_df
bfly_df <- merge(bfly_df, both_szn, all.y = TRUE)

# Reorganize and rename columns
bfly_df <- bfly_df %>%
  select(-11, -12, -13) %>%
  rename(Year = year,
         Month = month,
         Day = day,
         Abundance = total_butterfly_count,
         Richness = Unique_butterflies,
         Tmax = tmax_previous30,
         Tmin = tmin_previous30,
         Winter = Wseason_precip,
         Monsoon = Mseason_precip)




# ----- TURN PREDICTOR COLUMNS INTO ROWS -----

# Predictor: Tmax, Tmin, Winter, Monsoon

# Need to make it so that the column header for predictors is in another column.
# And all predictor values are in one column. Same with richness and abundance.

# Create empty data frame to store all the new data frames
bfly_predictors_df <- data.frame()

# Loop for creating new data frames (df) for PREDICTOR variables (columns 7-10)

# For i values 7 to 10, starting with 7...
for (i in 7:10) {
  
  # Select these columns from bfly_predictors_df and store it in a new df
  new_df <- data.frame(select(bfly_df, 1, 2, 3, 4, 11, i))
  
  # Get the name of that column i and store it in a new variable
  new_df_name <- paste(colnames(bfly_df)[i])
  
  # Add another column to the new df containing the name of those selected variables 
  new_df <- mutate(new_df, Predictor = new_df_name)
  
  # Rename i column (should be 6th one in new_df) as Value
  new_df <- rename(new_df, Predictor_Value = 6)
  
  # Add new_df to another new df that will contain all the new dfs
  bfly_predictors_df <- rbind(bfly_predictors_df, new_df)
  
  # Then the loop repeats with the next i value (starts with 8, stops after 11)
}

# Order predictors data frame
bfly_predictors_df <- arrange(bfly_predictors_df,
                              Site, Year, Month, Day, Predictor)




# ----- TURN RESPONSE COLUMNS INTO ROWS -----

# Response: Abundance, Richness

# Loop for creating new data frames (df) for RESPONSE variables (columns 6-7)
# (Same as for the predictors)

bfly_responses_df <- data.frame()

for (i in 5:6) {
  new_df <- data.frame(select(bfly_df, 1, 2, 3, 4, 11, i))
  new_df_name <- paste(colnames(bfly_df)[i])
  new_df <- mutate(new_df, Response = new_df_name)
  new_df <- rename(new_df, Response_Value = 6)
  bfly_responses_df <- rbind(bfly_responses_df, new_df)
}

# Order response data frame
bfly_responses_df <- arrange(bfly_responses_df,
                             Site, Year, Month, Day, Response)




# ----- MERGE PREDICTOR AND RESPONSE DATA FRAMES -----

# Merge data frames
bfly_df2 <- merge(bfly_responses_df, bfly_predictors_df, all = TRUE)

# Order response data frame
bfly_df2 <- arrange(bfly_df2,
                    Site, Year, Month, Day, Response, Predictor)




# ----- GENERATE FIGURE 4 -----

# Generate new labels for predictors so they are more descriptive in plot
pred_labels <- c(
  Monsoon = "Monsoon Precipitation",
  Tmax = "30-day Maximum Temperature prior to Sampling Date",
  Tmin = "30-day Minimum Temperature prior to Sampling Date",
  Winter = "Winter Precipitation"
)

# Match labels with Predictor label in bfly_df2
mods <- match(bfly_df2$Predictor, names(pred_labels))

# Add column with new Predictor labels
bfly_df2$Predictor2 <- pred_labels[mods]

# Generate figure
ggplot(bfly_df2, aes(x = Predictor_Value, y = Response_Value)) +
  geom_point() +
  geom_smooth(method = "lm",
              color = "cornflowerblue",
              fill = "lightskyblue") + 
  facet_nested(Response + Season_Sampled ~ Predictor2,
               scales = "free",
               switch = "y", 
               labeller = labeller(Predictor2 = label_wrap_gen(30))) +
  scale_y_continuous(position = "right") + 
  xlab("") + 
  ylab("") + 
  theme_minimal() +
  theme(strip.text = element_text(face = "bold", 
                                  color = "black",
                                  size = 10),
        panel.border = element_rect(fill = "transparent",
                                    color = "black",
                                    linewidth = 1),
        strip.background = element_rect(fill = "grey88",
                                        linetype = "solid",
                                        color = "black",
                                        linewidth = 1),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"))

# Save as shown on Plots
ggsave(file = "fig_4.png",
       plot = last_plot(),
       width = 25,
       height = 15,
       units = "cm")




