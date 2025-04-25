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

# Preview structure and data
str(StudentData)
head(StudentData)
View(StudentData)

# Check for missing values in the entire dataset
missing_values <- colSums(is.na(StudentData))

# Print the number of missing values in each column
print(missing_values)

# If you want to see if there are any columns with missing values
if (any(missing_values > 0)) {
  cat("There are missing values in the following columns:\n")
  print(names(missing_values[missing_values > 0]))
} else {
  cat("There are no missing values in the dataset.")
}

# Load VIM package
library(VIM)

# Visualize missing data
aggr_plot <- aggr(StudentData, col=c('skyblue', 'red'), numbers=TRUE, sortVars=TRUE, labels=names(StudentData), cex.axis=0.7, gap=3, ylab=c("Missing data", "Pattern"))
