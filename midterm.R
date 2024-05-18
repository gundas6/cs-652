install.packages("tidyverse")
install.packages("caret")
# Load necessary libraries
library(tidyverse)
library(caret)

# Load the dataset
data <- read.csv(file.choose())
head(data)

# Example preprocessing
# Handling missing values
data <- na.omit(data)
data

# Encoding categorical variables if necessary
# data <- data %>% mutate(category_column = as.factor(category_column))

# Splitting the data into training and testing sets
set.seed(42)
training_samples <- data$target %>%
  createDataPartition(p = 0.8, list = FALSE)
train_data <- data[training_samples, ]
test_data <- data[-training_samples, ]

# Fit the linear regression model
model <- lm(target ~ ., data = train_data)
summary(model)

# Making predictions
predictions <- predict(model, test_data)

# Evaluating the model
mse <- mean((test_data$target - predictions)^2)
r2 <- 1 - (sum((test_data$target - predictions)^2) / sum((test_data$target - mean(test_data$target))^2))

print(paste("Mean Squared Error: ", mse))
print(paste("R^2 Score: ", r2))

# Visualizing the results
library(ggplot2)

ggplot() +
  geom_point(aes(x = test_data$target, y = predictions), color = "blue") +
  labs(x = "Actual values", y = "Predicted values", title = "Actual vs Predicted values") +
  theme_minimal()
