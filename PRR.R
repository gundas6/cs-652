# Load necessary libraries

library(readr)
library(caret)
library(dplyr)
library(gplots)
library(ggplot2)
library(pROC)

# Load the Hepatitis dataset

url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/hepatitis/hepatitis.data"
column_names <- c("class", "age", "sex", "steroid", "antivirals", "fatigue", "malaise", "anorexia", "liver_big", "liver_firm", "spleen_palpable", "spiders", "ascites", "varices", "bilirubin", "alk_phosphate", "sgot", "albumin", "protime", "histology")
hepatitis_data <- read_csv(url, col_names = column_names)

## Convert '?' to NA

hepatitis_data[hepatitis_data == "?"] <- NA

# Convert columns to appropriate types

hepatitis_data <- hepatitis_data %>%
  mutate(across(where(is.character), as.factor)) %>%
  mutate(across(everything(), as.numeric))

# Impute missing values with median

hepatitis_data <- hepatitis_data %>%
  mutate_if(is.numeric, ~ifelse(is.na(.), median(., na.rm = TRUE), .))

# Convert 'class' to a binary factor (1 for live, 2 for die)

hepatitis_data$class <- as.factor(ifelse(hepatitis_data$class == 1, 1, 0))

# Split the data into training and testing sets

set.seed(123)
train_index <- createDataPartition(hepatitis_data$class, p = 0.8, list = FALSE)
train_data <- hepatitis_data[train_index, ]
test_data <- hepatitis_data[-train_index, ]



# Train a logistic regression model

model <- glm(class ~ ., data = train_data, family = binomial)

# Summary of the model
summary(model)

# Make predictions on the test set

predictions <- predict(model, test_data, type = "response")
predicted_classes <- ifelse(predictions > 0.5, 1, 0)


# Confusion matrix to evaluate the model

conf_matrix <- table(Predicted = predicted_classes, Actual = test_data$class)
conf_matrix

# Calculate accuracy

accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
accuracy

# Create confusion matrix heatmap

conf_matrix_df <- as.data.frame(as.table(conf_matrix))
colnames(conf_matrix_df) <- c("Actual", "Predicted", "Freq")

heatmap_conf <- ggplot(conf_matrix_df, aes(x = Predicted, y = Actual)) +
  geom_tile(aes(fill = Freq), color = "white") +
  scale_fill_gradient(low = "white", high = "blue") +
  geom_text(aes(label = Freq), vjust = 1) +
  theme_minimal() +
  ggtitle("Confusion Matrix Heatmap") +
  xlab("Predicted") +
  ylab("Actual")
print(heatmap_conf)


# Extract variable importance

var_imp <- summary(model)$coefficients[, "Pr(>|z|)"]
var_imp <- var_imp[-1]  # Remove the intercept


# Create variable importance plot

var_importance_df <- data.frame(Variable = names(var_imp), Importance = -log10(var_imp))

var_importance_plot <- ggplot(var_importance_df, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +
  theme_minimal() +
  ggtitle("Variable Importance Plot") +
  xlab("Variable") +
  ylab("-log10(p-value)")
print(var_importance_plot)




#  Boxplot of Features by Class

boxplot_plot <- ggplot(hepatitis_data, aes(x = class, y = bilirubin, fill = class)) +
  geom_boxplot() +
  theme_minimal() +
  ggtitle("Boxplot of Bilirubin by Class") +
  xlab("Class") +
  ylab("Bilirubin")
print(boxplot_plot)

#  ROC Curve

roc_curve <- roc(test_data$class, predictions)

roc_plot <- ggroc(roc_curve) +
  ggtitle("ROC Curve") +
  xlab("False Positive Rate") +
  ylab("True Positive Rate") +
  theme_minimal()
print(roc_plot)





