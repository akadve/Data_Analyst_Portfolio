#Part 1 - Data analysis
# Load required libraries
# optional installation step, if you don't have the libraries installed:
# install.packages(c("ggplot2", "dplyr","car"))
library(ggplot2)
library(dplyr)
library(car)
# 1. Load the data
data <- read.csv('C:/Users/nikol/365/365 - Courses/365 Data Science/Working/In-house/Projects/R project/housing_data.csv')

# 2. Summary statistics
print(summary(data))

# 3. Calculate mean, mode and median, as well as the standard deviation for each variable
# Mode function
mode_func <- function(x) {
  uniqv <- unique(x)
  uniqv[which.max(tabulate(match(x, uniqv)))]
}

means <- sapply(data, mean, na.rm = TRUE)  # Calculate means
modes <- sapply(data, mode_func)  # Calculate modes
medians <- sapply(data, median, na.rm = TRUE)  # Calculate medians
sds <- sapply(data, sd) 

medians
# Print results
print(data.frame(Variable = names(data), Mean = means, Mode = modes, Median = medians, SDT = sds))

# 4. Compute the correlation between each pair of variables
correlation_matrix <- cor(data, use = "complete.obs")
print(correlation_matrix)
max(correlation_matrix[correlation_matrix < 1])

means <- sapply(data, mean)  # Calculate means
sds <- sapply(data, sd)      # Calculate standard deviations
print(data.frame(Variable = names(data), Mean = means, Std_Deviation = sds))

# Fill in missing values with the mean for each variable
data <- data %>%
  mutate_all(~ifelse(is.na(.), mean(., na.rm=TRUE), .))
# Part 2 - Data visualization
# Create a histogram of house prices with styling and 8 bins
histogram <- ggplot(data, aes(x = `Median.Home.Value`)) +
  geom_histogram(bins = 9, fill="darkcyan", color="white", alpha=0.7) +
  labs(title = "Distribution of Median Home Values",
       x = "Median Home Value",
       y = "Frequency") +
  theme_minimal()
histogram

#Part 3 - Hypothesis testing
# 1. Define "high" and "low" crime rates based on the median of the Crime.Rate variable
median_crime_rate <- medians["Crime.Rate"]

data$Crime.Category <- ifelse(data$Crime.Rate > median_crime_rate, "High", "Low")
unique(data$Crime.Category)
levels(data$Crime.Category)
levene_test <- leveneTest(Median.Home.Value ~ Crime.Category, data = data)
print(levene_test)


# 2. Additional: Check for assumptions
# For a t-test, it's essential to check for normality & homogeneity of variance.

# Check normality for each group
shapiro_test_high <- shapiro.test(data$Median.Home.Value[data$Crime.Category == "High"])
shapiro_test_low <- shapiro.test(data$Median.Home.Value[data$Crime.Category == "Low"])
shapiro_test_high
shapiro_test_low

print(paste("Shapiro Test p-value for High Crime Rate:", shapiro_test_high$p.value))
print(paste("Shapiro Test p-value for Low Crime Rate:", shapiro_test_low$p.value))

# If the p-value is < 0.05 for the Shapiro test, then the data is not normally distributed.

# Check for homogeneity of variance using Levene's Test (need to install & load the 'car' package)

# If the p-value is < 0.05 for the Levene's test, then variances between the groups are not equal.

# 3. Conduct an independent two-sample t-test

t_test_result <- t.test(Median.Home.Value ~ Crime.Category, data = data, var.equal = TRUE)
print(t_test_result)


# 4. Print the results
print(t_test_result)

alpha <- 0.05
df <- nrow(data) - 1  # where n is your sample size

# For a two-tailed test, we take the alpha value and divide by 2 for the upper tail. 
# We use 1 - (alpha/2) because qt() gives the left tail by default, and subtracting from 1 gives us the right tail.
critical_t <- qt(1 - (alpha/2), df)
print(critical_t)


# Part 4 - Linear regression
# for predicting the median home value with the average rooms variable
model <- lm(`Median.Home.Value` ~ `Average.Rooms`, data=data)
print(summary(model))

regression_scatter <- ggplot(data, aes(x=Average.Rooms, y=Median.Home.Value)) +
  geom_point( alpha=0.6) +  # Color points by Crime Rate for added depth
  geom_smooth(method="lm", col="red", se=FALSE) +  # Linear regression line without shaded region
  labs(title="Regression Scatter Plot of Average Rooms vs. Median Home Value",
       x="Average Rooms",
       y="Median Home Value") +
  theme_minimal()
regression_scatter
