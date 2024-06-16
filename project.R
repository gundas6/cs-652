# Load necessary libraries
library(readr)
library(caret)
library(dplyr)
install.packages("gplots")
library(gplots)

# Load the Hepatitis dataset
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/hepatitis/hepatitis.data"
column_names <- c("class", "age", "sex", "steroid", "antivirals", "fatigue", "malaise", "anorexia", "liver_big", "liver_firm", "spleen_palpable", "spiders", "ascites", "varices", "bilirubin", "alk_phosphate", "sgot", "albumin", "protime", "histology")
hepatitis_data <- read_csv(url, col_names = column_names)

# Convert '?' to NA
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
heatmap_conf <- qplot(x = factor(rownames(conf_matrix)), y = factor(colnames(conf_matrix)), 
                      fill = as.vector(as.matrix(conf_matrix)), geom = "tile", 
                      main = "Confusion Matrix Heatmap", xlab = "Predicted", 
                      ylab = "Actual", fill = I("blue"))
print(heatmap_conf)


# Extract variable importance
var_imp <- summary(model)$coefficients[, "Pr(>|z|)"]

# Create variable importance plot
var_importance <- qplot(x = names(var_imp)[-1], y = var_imp[-1], 
                        geom = "bar", stat = "identity", 
                        main = "Variable Importance Plot", 
                        xlab = "Variable", ylab = "Importance", 
                        fill = I("skyblue"))
print(var_importance)