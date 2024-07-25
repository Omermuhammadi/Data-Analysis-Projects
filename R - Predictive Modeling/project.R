##installing packages required
install.packages("tidyverse")
install.packages("cluster")
install.packages("factoextra")
install.packages("randomForest")
##libraries
library(stats) ##clusters
library(gridExtra) ##displayplots
library(dplyr)
library(RColorBrewer)
library(randomForest)
library(tidyr)
library(tidytext)
library(stringr)
library(crayon)
library(car) # Create a scatter plot with clusters and ellipses
library(DT)
library(ggthemes)
library(lubridate)
library(tidyselect)
library(scales)
library(ggplot2)
library(car)
library(tools)
library(tidyverse) # data manipulation
library(cluster) # clustering algorithms

library(factoextra) # clustering algorithms & visualization
##csv file
exercise_data <- read.csv("C:/Users/DELL/Desktop/university/introduction to data science
fundamentals/DSF LAB/exercise_dataset.csv")
exercise_data
##exploring datasets
ncol(exercise_data)
nrow(exercise_data)
names(exercise_data)
dim(ex_data)
str(ex_data)
head(ex_data, 5)
tail(ex_data, 4)
summary(ex_data)
##tidy data operations
is.na(exercise_data)
sum(is.na(exercise_data))
##removing & displaying missing values
ex_data <- na.omit(exercise_data)
nrow(ex_data)
##data wrangling

ex_data %>%
  select(Age, Gender, Heart.Rate) %>% head(5)
ex_data %>%
  mutate(Mean_BMI = mean(BMI)) %>%
  select(Age, BMI, Mean_BMI) %>%
  head(5)
ex_data %>%
  filter(Age == "19")%>%
  select(Age, Gender, BMI)%>%
  head(5)

ex_data %>%
  rename(Weight = "Actual.Weight")%>%
  select(Gender, Age, Weight)%>%
  head(5)
ex_data %>%
  arrange(Calories.Burn)%>%
  head(5)
ex_data %>%
  group_by(Gender)%>%
  summarise(Mean_weight = mean(Actual.Weight),
            calories_burned = mean(Calories.Burn),

            age = mean(Age))
##taking random rows
random_rows <- exercise_data[sample(nrow(exercise_data), 150), ]
random_rows
##finding correlation among variables
cor(ex_data$Age, ex_data$Heart.Rate)
cor(ex_data$Exercise.Intensity, ex_data$Calories.Burn)
cor(ex_data$Duration, ex_data$Calories.Burn)
##linear regression
model <- lm(Heart.Rate ~ Age , data = random_rows)
model
## visualize the relationship between Age and Heart.Rate
ggplot(random_rows, aes(x = Age , y = Heart.Rate))+
  geom_point()
## create a scatter plot with a regression line
ggplot(random_rows, aes(x = Age , y = Heart.Rate))+
  geom_point()+
  stat_smooth(method = lm)
##create a bar plot with a color gradient
## displays the relationship between age and heart rate
ggplot(random_rows, aes(x = Age, y = Heart.Rate, fill = Age)) +

geom_bar(stat = "identity") +
  ggtitle("Heart Rate by by Age") +
  theme_bw() +
  scale_fill_gradient(low = "pink", high = "lightblue")
##create a histogram of the "Heart.Rate"
ggplot(random_rows, aes(x = Heart.Rate)) +
  geom_histogram(binwidth = 10, fill = "pink", color = "black") +
  ggtitle("Heart Rate Histogram") +
  xlab("Heart Rate") +
  ylab("Frequency") +
  theme_bw()
##is.finite() function to identify any NaN or Inf values
problematic_values <- !is.finite(random_rows$Age) | !is.finite(random_rows$Heart.Rate)
set.seed(1) # Set seed for reproducibility
k <- 3 # Number of clusters
kmeans_result <- kmeans(random_rows[, c("Age", "Heart.Rate")], centers = k)
# Add cluster labels to the data frame
random_rows$Cluster <- as.factor(kmeans_result$cluster)
# Calculate cluster centers
cluster_centers <- kmeans_result$centers

ggplot(random_rows, aes(x = Age, y = Heart.Rate, color = Cluster)) +
  geom_point(alpha = 0.7) +
  stat_ellipse(aes(fill = Cluster), geom = "polygon", alpha = 0.2, show.legend = FALSE) +
  ggtitle("Cluster Diagram: Heart Rate vs. Age") +
  xlab("Age") +
  ylab("Heart Rate") +
  theme_bw()
##linear-regression
model <- lm(Calories.Burn ~ Duration , data = random_rows)
model
# visualize the relationship between duration & calories.burn
ggplot(random_rows, aes(x = Duration , y = Calories.Burn))+
  geom_point()
## create a scatter plot with a regression line
ggplot(random_rows, aes(x = Duration , y = Calories.Burn))+
  geom_point()+
  stat_smooth(method = lm)
##create a bar plot visualizing the relationship between the "Duration" and "Calories.Burn
ggplot(random_rows, aes(x = Duration , y = Calories.Burn, fill = Duration)) +
  geom_bar(stat = "identity") +

ggtitle("Calories Burned by Duration") +
  theme_bw() +
  scale_fill_gradient(low = "turquoise", high = "orange")
##distribution of the "Calories.Burn" variable
ggplot(random_rows, aes(x = Calories.Burn)) +
  geom_histogram(binwidth = 10, fill = "orange", color = "black") +
  ggtitle("Calories Burned Histogram") +
  xlab("Calories.Burn") +
  ylab("Frequency") +
  theme_bw()
problematic_values <- !is.finite(random_rows$Duration) |
  !is.finite(random_rows$Calories.Burn)
# clustering starts from here
set.seed(1) # Set seed for reproducibility
k <- 3 # Number of clusters
kmeans_result <- kmeans(random_rows[, c("Duration", "Calories.Burn")], centers = k)
# Add cluster labels to the data frame
random_rows$Cluster <- as.factor(kmeans_result$cluster)
# Calculate cluster centers
cluster_centers <- kmeans_result$centers
ggplot(random_rows, aes(x = Duration, y = Calories.Burn, color = Cluster)) +

geom_point(alpha = 0.7) +
  stat_ellipse(aes(fill = Cluster), geom = "polygon", alpha = 0.2, show.legend = FALSE) +
  ggtitle("Cluster Diagram: Duration vs. Calories Burned") +
  xlab("Duration") +
  ylab("Calories Burned") +
  theme_bw()
# Scaling the dataset to remove extreme values
head(exercise_data, 6)
non_numeric_cols <- c("Exercise", "Gender", "Weather.Conditions")
exercise_data[non_numeric_cols] <- lapply(exercise_data[non_numeric_cols], as.factor)
# Ensure numeric columns are correctly identified as numeric
exercise_data <- exercise_data %>%
  mutate_if(is.factor, as.numeric)
# Scale the numeric columns in the dataset
numeric_cols <- setdiff(colnames(exercise_data), non_numeric_cols)
scaled_data <- exercise_data
scaled_data <- na.omit(scaled_data)

scaled_data[numeric_cols] <- scale(scaled_data[numeric_cols])
# Append the target variable (Calories.Burn) to the scaled dataset
scaled_data$Calories.Burn <- exercise_data$Calories.Burn
# Select relevant features
features <- c("Exercise", "Age", "Gender", "Duration", "Heart.Rate", "BMI",
              "Weather.Conditions", "Exercise.Intensity")
target <- "Calories.Burn"
# Split the dataset into training and testing sets
set.seed(123)
train_indices <- sample(1:nrow(scaled_data), 0.8 * nrow(scaled_data))
train_data <- scaled_data[train_indices, ]
test_data <- scaled_data[-train_indices, ]
dim(train_data)
dim(test_data)
# Remove rows with missing values
train_data <- train_data[complete.cases(train_data), ]
# Train the random forest regression model
random_forest_model <- randomForest(Calories.Burn ~ ., data = train_data[, c(features, target)])
# Train the linear regression model
linear_model <- lm(Calories.Burn ~ ., data = train_data[, c(features, target)])
# Perform k-means clustering
kmeans_model <- kmeans(scaled_data[, features], centers = 3)
# Assign cluster labels to the data
scaled_data$Cluster <- as.factor(kmeans_model$cluster)

# Make predictions on the test data
linear_predictions <- predict(linear_model, newdata = test_data)
head(linear_predictions, 5)
random_forest_predictions <- predict(random_forest_model, newdata = test_data)
head(random_forest_predictions, 6)

# Visualize the results
# Scatter plot of actual vs predicted values for linear regression
linear_plot <- ggplot(data = test_data, aes(x = Calories.Burn, y = linear_predictions, color =
                                              abs(linear_predictions - Calories.Burn))) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = "solid", color = "red") +
  ggtitle("Linear Regression: Actual vs Predicted") +
  xlab("Actual Calories.Burn") +
  ylab("Predicted Calories.Burn") +
  theme_bw() +
  scale_color_gradient(low = "blue", high = "red")
linear_plot

# Scatter plot of actual vs predicted values for random forest regression
colors <- brewer.pal(3, "Pastel1")
random_forest_plot <- ggplot(data = test_data, aes(x = Calories.Burn, y =
                                                     random_forest_predictions)) +
  geom_point(color = colors[1], alpha = 0.6) +
  geom_abline(intercept = 0, slope = 1, linetype = "solid", color = "red") +
  ggtitle("Random Forest Regression: Actual vs Predicted") +
  xlab("Actual Calories.Burn") +
  ylab("Predicted Calories.Burn") +
  theme_bw() +
  scale_color_manual(values = colors, guide = guide_colorbar(title = "Cluster", title.position = "right"))
random_forest_plot

# Box plot to compare linear regression and random forest regression
results <- data.frame(Actual = test_data$Calories.Burn, Linear_Predicted = linear_predictions,
                      RF_Predicted = random_forest_predictions)
boxplot_plot <- ggplot(results, aes(x = factor(1), y = Actual)) +
  geom_boxplot(width = 0.5, fill = "lightblue", color = "black") +
  geom_jitter(aes(x = factor(2 ), y = Linear_Predicted), color = "red", size = 2) +
  geom_jitter(aes(x = factor(3), y = RF_Predicted), color = "blue", size = 2) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4, color = "green") +
  geom_smooth(aes(x = factor(1), y = Actual), method = "lm", se = FALSE, color = "blue",
              linetype = "dashed") +
  ggtitle("Linear Regression vs Random Forest Regression") +
  xlab("") +
  ylab("Calories Burned") +
  theme_bw()
boxplot_plot

# Bar plot to compare actual and predicted values
barplot_plot <- ggplot(results, aes(x = factor(1), y = Actual)) +
  geom_bar(stat = "identity", fill = "lightblue", width = 0.5) +
  geom_point(aes(x = factor(2), y = Linear_Predicted), color = "red", size = 3) +
  geom_point(aes(x = factor(3), y = RF_Predicted), color = "blue", size = 3) +
  geom_smooth(aes(x = factor(1), y = Actual), method = "lm", se = FALSE, color = "blue",
              linetype = "dashed") +
  ggtitle("Random Forest Regression: Actual vs Predicted") +
  xlab("Actual vs Predicted") +
  ylab("Calories Burned") +
  theme_bw()
barplot_plot

# Arrange and display the plots
plots <- grid.arrange(linear_plot, random_forest_plot, nrow = 2, ncol = 2)
plots2 <- grid.arrange(boxplot_plot, barplot_plot, nrow = 2, ncol = 1)
final_plot <- grid.arrange(plots, plots2, heights = c(0.6, 0.4))
print(final_plot)

# Create a data frame with the predicted values
predictions <- data.frame(Model = c(rep("Linear Regression", length(linear_predictions)),rep("Random Forest Regression", length(random_forest_predictions)),rep("K-means Clustering", nrow(scaled_data)))
Predicted = c(linear_predictions, random_forest_predictions, scaled_data$Calories.Burn))

# Create the boxplot with regression line
ggplot(predictions, aes(x = Model, y = Predicted)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4, color = "blue") +
  stat_summary(fun = function(x) lm(y ~ x)$coef[1], geom = "line", aes(group = 1), linetype = "dashed", color = "red") +
  ggtitle("Linear Regression, Random Forest Regression, and K-means Clustering") +
  xlab("Model") +
  ylab("Predicted Calories.Burn") +
  theme_bw()

# Calculate the SSE for k-means clustering
kmeans_sse <- sum(kmeans_model$withinss)
# Calculate the MSE for k-means clustering
kmeans_mse <- kmeans_sse / nrow(scaled_data)

# Remove rows with missing values in test_data
test_data <- test_data[complete.cases(test_data), ]

# Generate predictions for linear regression and random forest regression models
linear_predictions <- predict(linear_model, newdata = test_data)
random_forest_predictions <- predict(random_forest_model, newdata = test_data)

# Calculate the MSE for linear regression, random forest regression, and k-means clustering
linear_mse <- mean((linear_predictions - test_data$Calories.Burn)^2)
random_forest_mse <- mean((random_forest_predictions - test_data$Calories.Burn)^2)

# Print the MSE for linear regression, random forest regression, and k-means clustering
cat("Linear Regression MSE:", linear_mse, "\n")
cat("Random Forest Regression MSE:", random_forest_mse, "\n")
cat("K-means Clustering MSE:", kmeans_mse, "\n")

# Calculate R-squared for linear regression
linear_r_squared <- 1 - (sum((test_data$Calories.Burn - linear_predictions)^2) / sum((test_data$Calories.Burn - mean(test_data$Calories.Burn))^2))
# Calculate MAE for linear regression
linear_mae <- mean(abs(test_data$Calories.Burn - linear_predictions))

# Calculate R-squared for random forest regression
random_forest_r_squared <- 1 - (sum((test_data$Calories.Burn - random_forest_predictions)^2) / sum((test_data$Calories.Burn - mean(test_data$Calories.Burn))^2))
# Calculate MAE for random forest regression
random_forest_mae <- mean(abs(test_data$Calories.Burn - random_forest_predictions))

# Print the accuracy metrics
cat("Linear Regression R-squared:", linear_r_squared, "\n")
cat("Linear Regression MAE:", linear_mae, "\n")
cat("Random Forest Regression R-squared:", random_forest_r_squared, "\n")
cat("Random Forest Regression MAE:", random_forest_mae, "\n")

                              
                             