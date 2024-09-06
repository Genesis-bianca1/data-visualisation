library(readr) # Read CSV data files
library(dplyr) # Manipulate data

# Import CSV file - path
data <- read_csv("/Users/genesisvega/Desktop/DV_Work_Directory/data.csv")
# Back-up copy of imported dataset
write_csv(data, "data_A.csv")


# Prepare data for analysis
# Logic to convert age range format - reduces time-consumption for readers
# AgeCategory becomes a character vector to carry out string-operations
data$AgeCategory <- as.character(data$AgeCategory)

# Function converts "Age n to n", to "n-n"
convert_age_format <- function(age_range) {
  # Removes "Age"
  age_range <- sub("Age ", "", age_range)  
  # Replaces "to" with "-"
  age_range <- sub(" to ", "-", age_range) 
  # "80 or older" becomes "80+"
  age_range <- sub("80 or older", "80+", age_range) 
  return(age_range)
}

# Apply conversions to age groups in AgeCategory
data$AgeCategory <- sapply(data$AgeCategory, convert_age_format)

# Order factor levels of AgeCategory
age_levels <- c("18-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80+")

# AgeCategory becomes a factor with age_levels and an order
data$AgeCategory <- factor(data$AgeCategory, levels = age_levels)

# Check to verify modifications
head(data$AgeCategory)

# Apply the changes permanently into the back-up dataset
write_csv(data, "data_A.csv")


# Prepare data for analysis
# Recode HadHeartAttack from categorical to numeric, 'Yes' = 1 and 'No' = 0
data <- data %>%
  mutate(
    HadHeartAttack = case_when(HadHeartAttack == "Yes" ~ 1, HadHeartAttack == "No" ~ 0, TRUE ~ NA_integer_) # NA represents missing data 
  )

# Apply changes permanently to the dataset
write_csv(data, "data_A.csv")






# Prepare visualisation_1.png

# Figure 2 - "Proportion of U.S citizens Reporting Heart Attacks by Age and Race, CDC 2022"

library(ggplot2) # Plot creator
library(dplyr)   # Data manipulator
library(readr)   # CSV file reader
library(viridis) # Colorblind friendly palette
library(viridisLite)


# Logic to calculate the "Yes" proportion of "HadHeartAttack"
data_proportional <- data %>%
  # For each race/ethnicity and age group
  group_by(RaceEthnicityCategory, AgeCategory) %>%    
  # Counts the total "Yes" responses, where 1 == "Yes"
  summarise(CountYes = sum(HadHeartAttack == 1, na.rm = TRUE),
            # Total responses per group
            Total = n(), .groups = 'drop') %>%
  # Provides the proportion "Yes" in "HadHeartAttack"
  mutate(ProportionYes = CountYes / Total)

# Construction of bar plot, applying data_proportional
proportional_bar_chart <- ggplot(data_proportional, aes(x = AgeCategory, y = ProportionYes, fill = AgeCategory)) +
  # Bars represent "Yes"
  geom_bar(stat = "identity", position = position_dodge(), color = "black") + # Black borders for each bar
  # Bar plots for each Race/Ethnicity Group
  facet_wrap(~ RaceEthnicityCategory, scales = "free_y") +
  scale_fill_viridis(discrete = TRUE) +  # viridis - colorblind-friendly palette
  # Continuous y axis intervals for all bar charts to help viewers compare charts visually
  scale_y_continuous(breaks = seq(0, 0.20, 0.05)) + # start, end, interval
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "white"), #set white background
    axis.text.x = element_text(angle = 45, vjust = 0.6), # Rotate for improved readability of axis labels
    legend.position = "bottom") +
  labs(title = "Proportion of U.S citizens Reporting Heart Attacks by Age and Race, CDC 2022",
       # x and y labels
       x = "Age Group",
       y = "Proportion of Heart Attacks (%)")

# Show bar chart in RStudio
print(proportional_bar_chart)

# Save the visualisation as a png file in the working directory
# Dimensions in inches & image resolution (dpi)
ggsave("visualisation_1.png", plot = proportional_bar_chart, width = 12, height = 6, dpi = 300)








# Prepare visualisation_2.html:

# Filter for "Multiracial, Non-Hispanic" respondents who said "Yes" to "HadHeartAttack"
# Calculate frequency and total frequency of reported heart attacks by age-group in 2022
data_multiracial <- data %>%
  filter(RaceEthnicityCategory == "Multiracial, Non-Hispanic", HadHeartAttack == 1) %>% # Filters 1 == "Yes" respondents
  group_by(AgeCategory) %>%
  summarise(Frequency = n()) %>%
  mutate(CumulativeFrequency = cumsum(Frequency)) %>% # Cumulative Freq
  ungroup()

# data_multiracial is ready to be applied

# Packages for visualisation_2.html - Figure 3
library(ggplot2) # Plot creator
library(dplyr)   # Data manipulator
library(readr)   # CSV file reader
library(viridis) # Colorblind friendly palette
library(plotly) # Interactive html plot graph
library(htmlwidgets) # Save as html visualisation


# Colorblind-friendly line-graph: 'magma' palette
# Calculate the cumulative freq of Heart Attack confirms among "Multiracial" respondents
my_line_graph <- ggplot(data_multiracial, aes(x = AgeCategory, y = CumulativeFrequency, group = 1)) +
  geom_line(aes(color = CumulativeFrequency), linewidth = 3) + # Width
  geom_point(aes(color = CumulativeFrequency), size = 3) +     # Line points
  scale_color_viridis(option = "magma", discrete = FALSE) +  # Colour scaling, fade/gradient effect
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.0)) + # Good readability of x axis items
  labs(title = "Cumulative Frequency of Heart Attacks in Multiracial Respondents from the U.S - CDC 2022",
       # x & y axis labels
       x = "Age Group",
       y = "Cumulative Frequency")

# Transform into interactive HTML line graph
interactive_graph <- ggplotly(my_line_graph)

# Save as a HTML file
htmlwidgets::saveWidget(interactive_graph, file = "visualisation_2.html")
