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

# Check the present working directory
getwd()

# Set the working directory to the location where the datasets are present
setwd("C:/Users/Manoj/Desktop/Semester 2 Assignments/Applied Analytics in Business and Society/Assignment 3/FIFA")

#Import the datasets
matches <- read.csv("matches.csv")
team_appearances <- read.csv("team_appearances.csv")
player_appearances <- read.csv("player_appearances.csv")
goals <- read.csv("goals.csv")

# Combine datasets using common columns
combined_data <- matches %>%
  inner_join(team_appearances, by = "match_id") %>%
  inner_join(player_appearances, by = "match_id") %>%
  inner_join(goals, by = "match_id")

View(combined_data)
head(combined_data)
str(combined_data)
summary(combined_data)
colnames(combined_data)

# Exploratory Data Analysis (EDA)

install.packages("corrplot")
library(corrplot)

# Check for missing values
missing_values <- colSums(is.na(combined_data))
print(missing_values)

# Distribution of home_team_score and away_team_score
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

# Quantitative Analysis

# Modeling 
library(mgcv)
library(nnet)

# Convert result.y variable to a factor with specified levels
combined_data$result.y <- factor(combined_data$result.y, levels = c("win", "lose", "draw"))

# Check unique values in result.y
unique(combined_data$result.y)

# Check for missing values in result.y
sum(is.na(combined_data$result.y))

# Set seed for reproducibility
set.seed(123)

# Sample size for training data, 70% training and 30% test data
train_size <- floor(0.7 * nrow(combined_data))

# Generate random indices for training data
train_indices <- sample(seq_len(nrow(combined_data)), size = train_size)

# Create training and test datasets
train_data <- combined_data[train_indices, ]
test_data <- combined_data[-train_indices, ]

# Check unique values in result.y in train_data
unique(train_data$result.y)

# Check if the "draw" category is present in the model
"draw" %in% levels(train_data$result.y)

# Set the reference category to "draw"
train_data$result.y <- relevel(train_data$result.y, ref = "draw")

# Fit the multinomial logistic regression model
model <- multinom(result.y ~ home_team_score + away_team_score + goals_for, data = train_data)

# Predict on test data
predictions <- predict(model, newdata = test_data)

# Check levels of result.y after modeling 
levels(train_data$result.y)

# Calculate accuracy
accuracy <- mean(predictions == test_data$result.y)

# Print accuracy in %
print(paste("Accuracy:", round(accuracy * 100, 2), "%"))

# Summary of the model
summary(model)

# Model Analysis
install.packages("pROC")
install.packages("pdp")
library(pROC)
library(pdp)

# Confusion Matrix
conf_matrix <- table(test_data$result.y, predictions)
conf_matrix_plot <- plot_ly(z = ~as.matrix(conf_matrix), colorscale = "Viridis", type = "heatmap",
                            x = colnames(conf_matrix), y = rownames(conf_matrix),
                            colorbar = list(title = "Counts"))
conf_matrix_plot <- layout(conf_matrix_plot, title = "Confusion Matrix", 
                           xaxis = list(title = "Predicted Class"),
                           yaxis = list(title = "Actual Class"))
conf_matrix_plot

# Prediction Distribution Plot

# Extract predicted probabilities for each class
probabilities <- predict(model, newdata = test_data, type = "probs")

# Plot histograms of predicted probabilities for each class
par(mfrow = c(1, length(levels(train_data$result.y))))
for (class in levels(train_data$result.y)) {
  hist(probabilities[, class], main = paste("Predicted Probabilities for", class), xlab = "Probability")
}

# Qualitative Analysis

# Text mining on player names to identify common nationalities
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
    grepl("son$", family_name.x, ignore.case = TRUE) ~ "Icelandic" # Identify Icelandic players
  ))

# Count the number of players for each nationality
nationality_counts <- table(player_nationalities$nationality)

# Create a data frame for plotting
nationality_data <- data.frame(nationality = names(nationality_counts),
                               count = as.numeric(nationality_counts))

# Reorder the levels of nationality in descending order of count
nationality_data$nationality <- factor(nationality_data$nationality, 
                                       levels = rev(nationality_data$nationality))

# Define custom colors for each nationality
custom_colors <- c(
  "Dutch" = "#ff7f0e",     # Orange
  "Arabic" = "#008000",    # Green
  "Scandinavian" = "#ff69b4",  # Pink
  "German" = "#000000",    # Black
  "Spanish" = "#d62728",   # Red
  "French" = "#1f77b4",    # Blue
  "Italian" = "#0000ff",   # Dark Blue
  "Serbian" = "#7f7f7f",   # Gray
  "Icelandic" = "#87ceeb"  # Sky Blue
)

# Reorder the custom colors accordingly
custom_colors <- custom_colors[levels(nationality_data$nationality)]

# Plotting an interactive bar chart
plot1 <- plot_ly(data = nationality_data, x = ~nationality, y = ~count, type = "bar", 
                 marker = list(color = custom_colors[nationality_data$nationality])) %>%
  layout(title = "Player Nationalities prediction based on their names",
         xaxis = list(title = "Nationality", categoryorder = "total descending"),  # Set categoryorder to total descending
         yaxis = list(title = "Count"))

# Display the interactive plot
htmlwidgets::saveWidget(plot1, "player_nationalities_by_their_name.html")

# Distribution of player positions using a bar chart
position_distribution <- combined_data %>%
  group_by(position_name) %>%
  summarise(distinct_player_count = n_distinct(player_id.x)) %>%
  arrange(desc(distinct_player_count)) %>%
  mutate(position_name = factor(position_name, levels = position_name))

# Plotting position distribution
plot2 <- position_distribution %>%
  plot_ly(x = ~position_name, y = ~distinct_player_count, type = "bar") %>%
  layout(title = "Count of Players by Position",
         xaxis = list(title = "Position"),
         yaxis = list(title = "Distinct Player Count"))

# Display the interactive plot
htmlwidgets::saveWidget(plot2, "player_position_distribution.html")

# Goal Distribution by Minute using a line chart
# Grouping data by match_id, minute_label, and goal_id to count distinct goals
goal_distribution <- combined_data %>%
  filter(!is.na(minute_regulation)) %>%
  group_by(match_id, minute_regulation, goal_id) %>%
  summarise(goals_count = n_distinct(goal_id))

# Grouping data by minute_label to sum up the goals count for each minute
goal_distribution <- goal_distribution %>%
  group_by(minute_regulation) %>%
  summarise(goals_count = sum(goals_count))

# Create hover text
hover_text <- paste("Minute = ", goal_distribution$minute_regulation, "<br>Number of Goals = ", goal_distribution$goals_count)

# Plotting goal distribution by minute
plot3 <- plot_ly(x = ~goal_distribution$minute_regulation, y = ~goal_distribution$goals_count, 
                 type = "scatter", mode = "lines+markers",
                 marker = list(color = 'rgba(65, 105, 225, .6)'), 
                 line = list(shape = "spline"),
                 text = hover_text) %>%
  layout(title = "Goal Distribution by Minute",
         xaxis = list(title = "Minute"),
         yaxis = list(title = "Goals Count"))

# Save the plot as an HTML widget
htmlwidgets::saveWidget(plot3, "goal_distribution.html")

# Calculate total sum of goals scored over all minutes
total_goals <- sum(goal_distribution$goals_count)

# Print total sum of goals
print(paste("Total Goals Scored:", total_goals))

#Word Cloud Generation using Match Data
install.packages("tm")
install.packages("wordcloud")
install.packages("RColorBrewer")
library(tm)
library(wordcloud)
library(RColorBrewer)

# Combine text from multiple columns into a single corpus
text_corpus <- paste(combined_data$match_name.x, combined_data$stadium_name.x, combined_data$city_name.x, combined_data$country_name.x, combined_data$team_name.x, combined_data$team_name.y, combined_data$player_team_name)

# Create a corpus
docs <- Corpus(VectorSource(text_corpus))

# Clean the text
docs <- docs %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace) %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords("english"))

# Create a document-term matrix
dtm <- DocumentTermMatrix(docs)

# Convert the matrix to a data frame
dtm_df <- as.data.frame(as.matrix(dtm))

# Compute word frequencies
word_freq <- colSums(dtm_df)

# Sort words by frequency
sorted_word_freq <- sort(word_freq, decreasing = TRUE)

# Create a word cloud
wordcloud(words = names(sorted_word_freq), freq = sorted_word_freq, min.freq = 1, max.words = 200, random.order = FALSE, rot.per = 0.35, colors = brewer.pal(8, "Dark2"))

# Extract player nationalities
player_nationalities <- c(combined_data$player_team_name, combined_data$team_name.x, combined_data$team_name.y)

# Create a word cloud of player nationalities
wordcloud(player_nationalities, 
          min.freq = 5,
          scale = c(5, 0.5),
          colors = brewer.pal(8, "Dark2"),
          random.order = TRUE,
          main = "Player Nationalities Word Cloud")










