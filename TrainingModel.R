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



