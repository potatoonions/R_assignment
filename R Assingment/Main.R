# Lim Wen Yi, TP067930
# Keith Lo Ze Hui, TP067663
# Jaeden Loong Deng Ze, TP068347
# Muhammad Hadi, TP077049

# Load necessary libraries
library(tidyverse)
library(caret)

# Data import
file_path <- "C:/Users/Wajiz.pk/Downloads/5. credit_risk_classification.csv"
credit_data <- read_csv(file_path)

# Data cleaning/pre-processing (remove rows with missing values)
credit_data_clean <- credit_data %>%
  drop_na()

# Further pre-processing: Convert necessary columns to appropriate data types
credit_data_clean <- credit_data_clean %>%
  mutate(
    installment_commitment = as.numeric(installment_commitment),  # Convert to numeric
    class = as.factor(class),  # Convert to factor for classification
    age = as.numeric(age),  # Ensure age is numeric
    duration = as.numeric(duration),  # Ensure loan duration is numeric
    existing_credits = as.numeric(existing_credits)  # Change for feature 3: existing credits
  )

# Display the first few rows of cleaned data
View(head(credit_data_clean, 100))  # Displays the first 100 rows after cleaning


### Objective 1: Objective 1: To investigate the relationship between installment commitment and credit class. – Muhammad Hadi, TP077049
## Analysis 1-1: Correlation Analysis between 'installment_commitment' and 'credit_amount'
# Calculating the correlation between installment_commitment and credit_amount
correlation_result <- cor(credit_data_clean$installment_commitment, credit_data_clean$credit_amount)
cat("Analysis 1-1: Correlation between Installment Commitment and Credit Amount: ", correlation_result, "\n")

# Plot the correlation
ggplot(credit_data_clean, aes(x = installment_commitment, y = credit_amount)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Correlation between Installment Commitment and Credit Amount",
       x = "Installment Commitment",
       y = "Credit Amount") +
  theme_minimal()

## Analysis 1-2: Logistic Regression for Credit Classification
# Performing logistic regression to see the relationship between installment commitment and credit class
logistic_model <- glm(class ~ installment_commitment, data = credit_data_clean, family = binomial)

# Summary of the logistic regression model
summary(logistic_model)

# Plot the logistic regression results
ggplot(credit_data_clean, aes(x = installment_commitment, y = as.numeric(class) - 1)) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), col = "red") +
  labs(title = "Logistic Regression of Credit Class based on Installment Commitment",
       x = "Installment Commitment",
       y = "Probability of Bad Credit (1)") +
  theme_minimal()

## Analysis 1-3: Linear Regression for Installment Commitment and Credit Amount
# Performing linear regression to explore the relationship between installment commitment and credit amount
linear_model <- lm(credit_amount ~ installment_commitment, data = credit_data_clean)

# Summary of the linear regression model
summary(linear_model)

# Plot the linear regression results
ggplot(credit_data_clean, aes(x = installment_commitment, y = credit_amount)) +
  geom_point() +
  geom_smooth(method = "lm", col = "green") +
  labs(title = "Linear Regression of Credit Amount based on Installment Commitment",
       x = "Installment Commitment",
       y = "Credit Amount") +
  theme_minimal()

## Analysis 1-4: Credit Class Distribution by Installment Commitment
# Distribution of Credit Class based on Installment Commitment
ggplot(credit_data_clean, aes(x = installment_commitment, fill = class)) +
  geom_histogram(binwidth = 5, position = "dodge") +
  labs(title = "Distribution of Installment Commitment by Credit Class",
       x = "Installment Commitment",
       y = "Count") +
  theme_minimal()

## Extra Feature Analysis 1-5: Correlation between Age and Installment Commitment
# Correlation analysis
correlation_age_commitment <- cor(credit_data_clean$age, credit_data_clean$installment_commitment)
cat("Analysis 1-5: Correlation between Age and Installment Commitment: ", correlation_age_commitment, "\n")

# Line plot for the relationship between age and installment commitment
ggplot(credit_data_clean, aes(x = age, y = installment_commitment, group = 1)) +
  geom_line(col = "purple") +
  labs(title = "Age vs Installment Commitment (Line Plot)",
       x = "Age",
       y = "Installment Commitment") +
  theme_minimal()

## Extra Feature Analysis 1-6: Loan Duration vs Installment Commitment
# Correlation analysis
correlation_duration_commitment <- cor(credit_data_clean$duration, credit_data_clean$installment_commitment)
cat("Analysis 2.2: Correlation between Loan Duration and Installment Commitment: ", correlation_duration_commitment, "\n")

# Line plot for the relationship between duration and installment commitment
ggplot(credit_data_clean, aes(x = duration, y = installment_commitment, group = 1)) +
  geom_line(col = "orange") +
  labs(title = "Loan Duration vs Installment Commitment (Line Plot)",
       x = "Loan Duration",
       y = "Installment Commitment") +
  theme_minimal()

## Extra Feature Analysis 1-7: Existing Credits vs Installment Commitment (Line Plot)
# Correlation analysis
correlation_existing_credits_commitment <- cor(credit_data_clean$existing_credits, credit_data_clean$installment_commitment)
cat("Analysis 2.3: Correlation between Existing Credits and Installment Commitment: ", correlation_existing_credits_commitment, "\n")

# Line plot for the relationship between existing credits and installment commitment
ggplot(credit_data_clean, aes(x = existing_credits, y = installment_commitment, group = 1)) +
  geom_line(col = "brown") +
  labs(title = "Existing Credits vs Installment Commitment (Line Plot)",
       x = "Number of Existing Credits",
       y = "Installment Commitment") +
  theme_minimal()

# Final output message
cat("\nAnalysis complete with additional features 1-5, 1-6, and 1-7.")
