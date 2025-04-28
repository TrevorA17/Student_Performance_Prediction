# Load dataset
StudentData <- read.csv("data/student_performance.csv", colClasses = c(
  Gender = "factor",
  AttendanceRate = "numeric",
  StudyHoursPerWeek = "numeric",
  PreviousGrade = "numeric",
  ExtracurricularActivities = "numeric",
  ParentalSupport = "factor",
  FinalGrade = "numeric"
))

# Load necessary libraries
library(caret)

# Set up LOOCV using caret's trainControl
ctrl <- trainControl(method = "LOOCV")

# Train a Random Forest model using LOOCV
rf_model <- train(FinalGrade ~ ., 
                  data = StudentData, 
                  method = "rf", 
                  trControl = ctrl)

# Print model results
print(rf_model)

# Train a K-Nearest Neighbors model using LOOCV
knn_model <- train(FinalGrade ~ ., 
                   data = StudentData, 
                   method = "knn", 
                   trControl = ctrl)

# Print results for KNN
print(knn_model)

# Extract model performance
rf_results <- rf_model$results
knn_results <- knn_model$results

# Select the best tuning parameter results
rf_best <- rf_results[which.min(rf_results$RMSE), ]
knn_best <- knn_results[which.min(knn_results$RMSE), ]

# Combine into a comparison table
model_comparison <- rbind(
  RandomForest = rf_best,
  KNN = knn_best
)

# View comparison
print(model_comparison)

# Load necessary library
library(ggplot2)

# Create a dataframe for plotting
performance_df <- data.frame(
  Model = c("RandomForest", "KNN"),
  RMSE = c(rf_best$RMSE, knn_best$RMSE),
  Rsquared = c(rf_best$Rsquared, knn_best$Rsquared)
)

# Plot RMSE comparison
ggplot(performance_df, aes(x = Model, y = RMSE, fill = Model)) +
  geom_bar(stat = "identity", width = 0.6) +
  labs(title = "RMSE Comparison", y = "Root Mean Squared Error") +
  theme_minimal()

# Plot R-squared comparison
ggplot(performance_df, aes(x = Model, y = Rsquared, fill = Model)) +
  geom_bar(stat = "identity", width = 0.6) +
  labs(title = "R-squared Comparison", y = "R²") +
  theme_minimal()



