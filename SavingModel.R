# Saving the Random Forest model
saveRDS(rf_model, "./models/saved_rf_model_student.rds")

# Load the saved model
loaded_rf_model_student <- readRDS("./models/saved_rf_model_student.rds")

# Model predicts FinalGrade
new_student <- data.frame(
  Gender = factor("Male", levels = levels(StudentData$Gender)),
  AttendanceRate = 85,
  StudyHoursPerWeek = 15,
  PreviousGrade = 78,
  ExtracurricularActivities = 2,
  ParentalSupport = factor("High", levels = levels(StudentData$ParentalSupport))
)

# Use the loaded model to make predictions
predicted_grade <- predict(loaded_rf_model_student, newdata = new_student)

# Print prediction
print(predicted_grade)
