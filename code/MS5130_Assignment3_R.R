options(repos = "https://cloud.r-project.org")  # Set CRAN mirror
install.packages('readr')
install.packages('tidyverse')
install.packages("ISLR")
install.packages("gam")
install.packages("interactions")
library(readr)
library(tidyr)
library(dplyr)
library(leaflet)
library(plotly)
library(ggplot2)
library(forcats)
library(gam)
library(ISLR)
library(interactions)

getwd()
setwd("C:/Users/Manoj/Desktop/Semester 2 Assignments/Applied Analytics in Business and Society/Assignment 3/FIFA")

#Import the datasets
matches <- read.csv("matches.csv")
team_appearances <- read.csv("team_appearances.csv")
player_appearances <- read.csv("player_appearances.csv")
goals <- read.csv("goals.csv")

# Combine datasets using common columns
combined_data <- matches %>%
  left_join(team_appearances, by = "match_id") %>%
  left_join(player_appearances, by = "match_id") %>%
  left_join(goals, by = "match_id")

View(combined_data)
head(combined_data)
str(combined_data)
summary(combined_data)

# Exploratory Data Analysis (EDA)

install.packages("corrplot")
library(corrplot)

# Check for missing values
missing_values <- colSums(is.na(combined_data))
print(missing_values)

# Visualizing distribution of home_team_score and away_team_score
plot <- ggplot(combined_data, aes(x = home_team_score, y = away_team_score)) +
  geom_point() +
  labs(x = "Home Team Score", y = "Away Team Score", title = "Distribution of Home and Away Team Scores") +
  theme_minimal()
plotly::ggplotly(plot)

# Correlation matrix correlation matrix of match statistics including home_team_score, away_team_score, goals_for, and goals_against.
correlation_matrix <- cor(combined_data[, c("home_team_score", "away_team_score", "goals_for", "goals_against")])
corrplot(correlation_matrix, method = "color")

# Boxplot of home_team_score and away_team_score by result
plot <- ggplot(combined_data, aes(x = result.x, y = home_team_score)) +
  geom_boxplot() +
  labs(x = "Result", y = "Home Team Score", title = "Boxplot of Home Team Score by Result") +
  theme_minimal()
plotly::ggplotly(plot)

plot <- ggplot(combined_data, aes(x = result.x, y = away_team_score)) +
  geom_boxplot() +
  labs(x = "Result", y = "Away Team Score", title = "Boxplot of Away Team Score by Result") +
  theme_minimal()
plotly::ggplotly(plot)

# Scatterplot of goals_for vs. goals_against
plot <- ggplot(combined_data, aes(x = goals_for, y = goals_against)) +
  geom_point() +
  labs(x = "Goals For", y = "Goals Against", title = "Scatterplot of Goals For vs. Goals Against") +
  theme_minimal()
plotly::ggplotly(plot)

# Histograms to check normality of predictor variables
histograms <- combined_data %>%
  select(home_team_score, away_team_score, goals_for, goals_against) %>%
  gather() %>%
  ggplot(aes(x = value)) +
  geom_histogram(bins = 20, fill = "skyblue", color = "black", alpha = 0.8) +
  facet_wrap(~key, scales = "free") +
  labs(x = "Value", y = "Frequency", title = "Histograms of Predictor Variables") +
  theme_minimal()

plotly::ggplotly(histograms)


# Modeling 

# Load required libraries
library(dplyr)
library(mgcv)

# Convert it to a factor with specified levels
combined_data$result.y <- factor(combined_data$result.y, levels = c("win", "lose", "draw"))

# Check unique values in result.y
unique(model_data$result.y)

# Check for missing values in result.y
sum(is.na(model_data$result.y))

# Write combined_data to a CSV file
#write.csv(combined_data, file = "combined_data.csv", row.names = FALSE)

# Set seed for reproducibility
set.seed(123)

# Sample size for training data
train_size <- floor(0.7 * nrow(combined_data))

# Generate random indices for training data
train_indices <- sample(seq_len(nrow(combined_data)), size = train_size)

# Create training and test datasets
train_data <- combined_data[train_indices, ]
test_data <- combined_data[-train_indices, ]

# Fit the model using training data
library(nnet)

# Check unique values in result.y in train_data
unique(train_data$result.y)

# Check if the "win" category is present in the model
"draw" %in% levels(train_data$result.y)

# Set the reference category to "win"
train_data$result.y <- relevel(train_data$result.y, ref = "draw")

# Fit the multinomial logistic regression model
model <- multinom(result.y ~ home_team_score + away_team_score + goals_for,
                  data = train_data)

# Predict on test data
predictions <- predict(model, newdata = test_data)

# Check levels of result.y after releveling
levels(train_data$result.y)

# Summary of the model
summary(model)


#Text Mining 

# 4. Qualitative Analysis
# Example: Text mining on player names to identify common nationalities
player_nationalities <- combined_data %>%
  select(player_id.x, family_name.x, given_name.x) %>%
  distinct() %>%
  mutate(nationality = case_when(
    grepl("van", family_name.x, ignore.case = TRUE) ~ "Dutch",
    grepl("al-", family_name.x, ignore.case = TRUE) ~ "Arabic",
    grepl("sson", family_name.x, ignore.case = TRUE) ~ "Scandinavian",
    grepl("sch", family_name.x, ignore.case = TRUE) ~ "German",
    grepl("ez", family_name.x, ignore.case = TRUE) ~ "Spanish",
    grepl("eau", family_name.x, ignore.case = TRUE) ~ "French",
    grepl("ini", family_name.x, ignore.case = TRUE) ~ "Italian",
    grepl("ic$", family_name.x, ignore.case = TRUE) ~ "Serbian",
    grepl("son$", family_name.x, ignore.case = TRUE) ~ "Icelandic", # Identify Icelandic players
  ))

# Count the number of players for each nationality
nationality_counts <- table(player_nationalities$nationality)

# Create a data frame for plotting
nationality_data <- data.frame(nationality = names(nationality_counts),
                               count = as.numeric(nationality_counts))

# Plotting an interactive bar chart
plot <- plot_ly(data = nationality_data, x = ~nationality, y = ~count, type = "bar", 
                marker = list(color = 'rgba(144, 238, 144, .6)')) %>%
  layout(title = "Distribution of Player Nationalities",
         xaxis = list(title = "Nationality"),
         yaxis = list(title = "Count"))

# Display the interactive plot
plot



# For qualitative analysis, let's analyze the distribution of player positions using a pie chart
position_distribution <- combined_data %>%
  distinct(player_id.x, position_name) %>%
  filter(!is.na(position_name)) %>%  # Filter out rows with null values in position_name
  group_by(position_name) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# Plotting position distribution as a pie chart
plot <- plot_ly(data = position_distribution, labels = ~position_name, values = ~count, type = "pie", 
                marker = list(colors = rainbow(length(position_distribution$position_name))),
                textinfo = "label+percent") %>%
  layout(title = "Distinct Player Position Distribution")

# Saving the plot as an HTML file
htmlwidgets::saveWidget(plot, "distinct_position_distribution_pie_chart.html")


# For qualitative analysis, let's analyze the distribution of player positions using a bar chart
position_distribution <- combined_data %>%
  group_by(position_name) %>%
  summarise(distinct_player_count = n_distinct(player_id.x)) %>%
  arrange(desc(distinct_player_count))

# Plotting position distribution
plot3 <- position_distribution %>%
  plot_ly(x = ~position_name, y = ~distinct_player_count, type = "bar", 
          marker = list(color = 'rgba(144, 238, 144, .6)')) %>%
  layout(title = "Distinct Count of Players by Position",
         xaxis = list(title = "Position"),
         yaxis = list(title = "Distinct Player Count"))

htmlwidgets::saveWidget(plot3, "plot3.html")


# Grouping data by match_id, minute_label, and goal_id to count distinct goals
goal_distribution <- combined_data %>%
  filter(!is.na(minute_label)) %>%
  group_by(match_id, minute_label, goal_id) %>%
  summarise(goals_count = n_distinct(goal_id))

# Grouping data by minute_label to sum up the goals count for each minute
goal_distribution <- goal_distribution %>%
  group_by(minute_label) %>%
  summarise(goals_count = sum(goals_count))

# Create hover text
hover_text <- paste("Minute = ", goal_distribution$minute_label, "<br>Number of Goals = ", goal_distribution$goals_count)

# Plotting goal distribution by minute
plot_goal_distribution <- plot_ly(x = ~goal_distribution$minute_label, y = ~goal_distribution$goals_count, 
                                  type = "scatter", mode = "lines+markers",
                                  marker = list(color = 'rgba(65, 105, 225, .6)'), 
                                  line = list(shape = "spline"),
                                  text = hover_text) %>%
  layout(title = "Goal Distribution by Minute",
         xaxis = list(title = "Minute"),
         yaxis = list(title = "Goals Count"))

# Save the plot as an HTML widget
htmlwidgets::saveWidget(plot_goal_distribution, "goal_distribution_line_plot.html")

# Calculate total sum of goals scored over all minutes
total_goals <- sum(goal_distribution$goals_count)

# Print total sum of goals
print(paste("Total Goals Scored:", total_goals))

names(combined_data)

#Geographical Data Analysis 

# Load required libraries
library(leaflet)
library(maps)

# Load the world map
world_map <- map_data("world")

# Extract country names and their corresponding latitude and longitude coordinates from world_map
country_data <- data.frame(
  country_name = world_map$region,
  latitude = world_map$lat,
  longitude = world_map$long
)

# Merge with combined_data to add latitude and longitude information
combined_data_with_coords <- merge(combined_data, country_data, by.x = "country_name.x", by.y = "country_name")

# Create a Leaflet map using combined_data_with_coords
map <- leaflet(data = combined_data_with_coords) %>%
  addTiles() %>%
  addMarkers(~latitude, ~longitude, popup = ~paste("Country: ", country_name.x, "<br>",
                                                   "Match Name: ", match_name.x)) %>%
  addMeasure(primaryLengthUnit = "kilometers", primaryAreaUnit = "sqmeters")

# Print the map to display it
print(map)








