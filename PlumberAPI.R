# Plumber API for Student FinalGrade Prediction

# Load the saved Random Forest model
loaded_rf_model_student <- readRDS("./models/saved_rf_model_student.rds")

#* @apiTitle Student Final Grade Prediction API
#* @apiDescription Predicts a student's final grade based on attendance, study hours, and other factors.

#* @param Gender Student's gender (Male/Female)
#* @param AttendanceRate Attendance rate percentage (0-100)
#* @param StudyHoursPerWeek Number of study hours per week
#* @param PreviousGrade Previous grade percentage (0-100)
#* @param ExtracurricularActivities Number of extracurricular activities
#* @param ParentalSupport Level of parental support (Low/Medium/High)

#* @get /predict_finalgrade

predict_final_grade <- function(Gender, AttendanceRate, StudyHoursPerWeek, PreviousGrade, ExtracurricularActivities, ParentalSupport) {
  
  # Create a data frame from input parameters
  to_be_predicted <- data.frame(
    Gender = factor(Gender, levels = levels(StudentData$Gender)),
    AttendanceRate = as.numeric(AttendanceRate),
    StudyHoursPerWeek = as.numeric(StudyHoursPerWeek),
    PreviousGrade = as.numeric(PreviousGrade),
    ExtracurricularActivities = as.numeric(ExtracurricularActivities),
    ParentalSupport = factor(ParentalSupport, levels = levels(StudentData$ParentalSupport))
  )
  
  # Use the loaded model to predict
  prediction <- predict(loaded_rf_model_student, newdata = to_be_predicted)
  
  # Return the predicted final grade
  return(prediction)
}
