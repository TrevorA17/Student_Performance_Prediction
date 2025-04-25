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

# Load ggplot2 for plotting
library(ggplot2)

# Histogram for FinalGrade (continuous variable)
ggplot(StudentData, aes(x = FinalGrade)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
  labs(title = "Histogram of FinalGrade", x = "FinalGrade", y = "Frequency")

# Boxplot for StudyHoursPerWeek (continuous variable)
ggplot(StudentData, aes(x = "", y = StudyHoursPerWeek)) +
  geom_boxplot(fill = "orange", color = "black") +
  labs(title = "Boxplot of StudyHoursPerWeek", y = "Study Hours Per Week")

# Bar plot for Gender (categorical variable)
ggplot(StudentData, aes(x = Gender)) +
  geom_bar(fill = "lightgreen", color = "black") +
  labs(title = "Bar Plot of Gender", x = "Gender", y = "Count")

# Bar plot for ParentalSupport (categorical variable)
ggplot(StudentData, aes(x = ParentalSupport)) +
  geom_bar(fill = "lightcoral", color = "black") +
  labs(title = "Bar Plot of Parental Support", x = "Parental Support", y = "Count")

# Scatter plot for StudyHoursPerWeek vs FinalGrade (continuous vs continuous)
ggplot(StudentData, aes(x = StudyHoursPerWeek, y = FinalGrade)) +
  geom_point(color = "blue") +
  labs(title = "Scatter Plot of Study Hours vs Final Grade", x = "Study Hours Per Week", y = "Final Grade") +
  geom_smooth(method = "lm", color = "red")  # Adds a linear regression line

# Scatter plot faceted by Gender
ggplot(StudentData, aes(x = StudyHoursPerWeek, y = FinalGrade)) +
  geom_point(aes(color = Gender)) +
  labs(title = "Study Hours vs Final Grade by Gender", x = "Study Hours Per Week", y = "Final Grade") +
  facet_wrap(~ Gender)  # Facets the plot by Gender

# Boxplot for FinalGrade by ParentalSupport
ggplot(StudentData, aes(x = ParentalSupport, y = FinalGrade, fill = ParentalSupport)) +
  geom_boxplot() +
  labs(title = "Boxplot of FinalGrade by Parental Support", x = "Parental Support", y = "Final Grade") +
  scale_fill_brewer(palette = "Set3")

# Load the corrplot package
library(corrplot)

# Select continuous variables for correlation
cont_vars <- StudentData[, c("AttendanceRate", "StudyHoursPerWeek", "PreviousGrade", "FinalGrade")]

# Compute the correlation matrix
cor_matrix <- cor(cont_vars)

# Plot the correlation matrix
corrplot(cor_matrix, method = "circle", type = "upper", tl.col = "black", tl.srt = 45)



