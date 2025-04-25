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

# Measures of Frequency
# Frequency of Gender
table(StudentData$Gender)

# Frequency of Parental Support levels
table(StudentData$ParentalSupport)

# Measures of Central Tendency
# Central Tendency: Mean & Median
mean(StudentData$AttendanceRate)
median(StudentData$AttendanceRate)

mean(StudentData$StudyHoursPerWeek)
median(StudentData$StudyHoursPerWeek)

mean(StudentData$FinalGrade)
median(StudentData$FinalGrade)

# Mode function (for categorical/factor or numeric variables)
get_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

get_mode(StudentData$ParentalSupport)
get_mode(StudentData$Gender)

# Measures of Distribution
# Standard Deviation
sd(StudentData$AttendanceRate)
sd(StudentData$StudyHoursPerWeek)
sd(StudentData$FinalGrade)

# Range
range(StudentData$AttendanceRate)
range(StudentData$StudyHoursPerWeek)
range(StudentData$FinalGrade)

# IQR (Interquartile Range)
IQR(StudentData$AttendanceRate)
IQR(StudentData$StudyHoursPerWeek)
IQR(StudentData$FinalGrade)

# Correlation matrix for numeric predictors and FinalGrade
numeric_vars <- StudentData[, sapply(StudentData, is.numeric)]
cor(numeric_vars)

# Visual correlation plot (optional)
# install.packages("corrplot") if needed
library(corrplot)
corrplot(cor(numeric_vars), method = "number")

# ANOVA to check if FinalGrade differs by Gender
anova_gender <- aov(FinalGrade ~ Gender, data = StudentData)
summary(anova_gender)

# ANOVA to check if FinalGrade differs by Parental Support
anova_parental_support <- aov(FinalGrade ~ ParentalSupport, data = StudentData)
summary(anova_parental_support)

# Post-hoc Tukey's HSD for ParentalSupport
tukey_result <- TukeyHSD(anova_parental_support)
summary(tukey_result)

# Post-hoc Tukey's HSD for Gender (optional, if needed)
tukey_gender <- TukeyHSD(anova_gender)
summary(tukey_gender)


