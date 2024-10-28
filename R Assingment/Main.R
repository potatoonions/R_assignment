# Lim Wen Yi, TP067930
# Keith Lo Ze Hui, TP067663
# Jaeden Loong Deng Ze, TP068347
# Muhammad Hadi, TP077049


# Load necessary libraries
library(tidyverse)
library(caret)


### Data Preparation
## Data import
file_path <- "./dataset/credit_risk_classification.csv"
df <- read.csv(file_path, stringsAsFactors = TRUE, row.names = 1)

## Cleaning and Preprocessing
# Remove Duplicates
duplicate_rows <- df[duplicated(df), ]
cat("Number of duplicate rows:", nrow(duplicate_rows), "\n")
df <- df |> distinct()

# Missing Data Summary
missing_data_count <- df |>
  summarize(across(everything(), ~ sum(is.na(.)))) |>
  pivot_longer(
    everything(),
    names_to = "column",
    values_to = "missing_count"
  )

missing_data_percentage <- df |>
  summarize(across(everything(), ~ mean(is.na(.)) * 100)) |>
  pivot_longer(
    everything(),
    names_to = "column",
    values_to = "missing_percentage"
  )

missing_data_combined <- merge(
  missing_data_count,
  missing_data_percentage,
  by = "column"
)

print("Missing Data Summary:")
missing_data_combined

# Remove rows with missing data
df <- df |>
  drop_na()

# Trim Whitespace
df <- df |> mutate(across(where(is.character), ~ trimws(.)))

# Column Types
ranged_columns <- c("checking_status", "savings_status", "employment")
unique_ranged_values <- sapply(df[ranged_columns], unique)
for (col in ranged_columns) {
  df[[col]] <- ordered(df[[col]], levels = unique_ranged_values[[col]])
}

# List All Numeric Columnns For Later Use
numeric_columns <- df |>
  select(where(is.numeric)) |>
  colnames()

# Print Column Types To Check
print("Column Types:")
sapply(df, class)

# Display the first 100 rows of cleaned data
View(head(df, 100))

## Data Validation (outliers)
# Visualise Outliers With Boxplot
# TODO remove this?
for (col in numeric_columns) {
  boxplot(
    df[[col]],
    main = col, ylab = "Values", col = "lightblue",
    cex.main = 1.5, cex.lab = 1.2, cex.axis = 1.2
  )
}

# Identify And Cap Outliers Using IQR
detect_outliers <- function(column, cap = FALSE) {
  q1 <- quantile(column, 0.10, na.rm = TRUE)
  q3 <- quantile(column, 0.90, na.rm = TRUE)
  iqr_value <- IQR(column, na.rm = TRUE)

  lower_bound <- q1 - 1.5 * iqr_value
  upper_bound <- q3 + 1.5 * iqr_value

  outliers <- column[column < lower_bound | column > upper_bound]

  if (cap) {
    column <- ifelse(column > upper_bound, upper_bound, column)
    column <- ifelse(column < lower_bound, lower_bound, column)
  }

  return(outliers)
}

columns_to_cap <- c(
  "duration",
  "credit_amount"
)
for (col in numeric_columns) {
  if (col %in% columns_to_cap) {
    outliers <- detect_outliers(df[[col]], cap = TRUE)
    cat("Outliers capped in", col, ":", length(outliers), "\n")
  } else {
    outliers <- detect_outliers(df[[col]], cap = FALSE)
    cat("Outliers not capped in", col, ":", length(outliers), "\n")
  }
  cat(outliers, "\n\n")
}


### Objective 1: To investigate the relationship between installment commitment and credit class – Muhammad Hadi, TP077049
## Analysis 1-1: Correlation Analysis between 'installment_commitment' and 'credit_amount'
# Calculating the correlation between installment_commitment and credit_amount
correlation_result <- cor(df$installment_commitment, df$credit_amount)
cat("Analysis 1-1: Correlation between Installment Commitment and Credit Amount: ", correlation_result, "\n")

# Plot the correlation
ggplot(df, aes(x = installment_commitment, y = credit_amount)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(
    title = "Correlation between Installment Commitment and Credit Amount",
    x = "Installment Commitment",
    y = "Credit Amount"
  ) +
  theme_minimal()


## Analysis 1-2: Logistic Regression for Credit Classification
# Performing logistic regression to see the relationship between installment commitment and credit class
logistic_model <- glm(class ~ installment_commitment, data = df, family = binomial)

# Summary of the logistic regression model
summary(logistic_model)

# Plot the logistic regression results
ggplot(df, aes(x = installment_commitment, y = as.numeric(class) - 1)) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), col = "red") +
  labs(
    title = "Logistic Regression of Credit Class based on Installment Commitment",
    x = "Installment Commitment",
    y = "Probability of Bad Credit (1)"
  ) +
  theme_minimal()


## Analysis 1-3: Linear Regression for Installment Commitment and Credit Amount
# Performing linear regression to explore the relationship between installment commitment and credit amount
linear_model <- lm(credit_amount ~ installment_commitment, data = df)

# Summary of the linear regression model
summary(linear_model)

# Plot the linear regression results
ggplot(df, aes(x = installment_commitment, y = credit_amount)) +
  geom_point() +
  geom_smooth(method = "lm", col = "green") +
  labs(
    title = "Linear Regression of Credit Amount based on Installment Commitment",
    x = "Installment Commitment",
    y = "Credit Amount"
  ) +
  theme_minimal()


## Analysis 1-4: Credit Class Distribution by Installment Commitment
# Distribution of Credit Class based on Installment Commitment
ggplot(df, aes(x = installment_commitment, fill = class)) +
  geom_histogram(binwidth = 5, position = "dodge") +
  labs(
    title = "Distribution of Installment Commitment by Credit Class",
    x = "Installment Commitment",
    y = "Count"
  ) +
  theme_minimal()


## Extra Feature Analysis 1-5: Correlation between Age and Installment Commitment
# Correlation analysis
correlation_age_commitment <- cor(df$age, df$installment_commitment)
cat("Analysis 1-5: Correlation between Age and Installment Commitment: ", correlation_age_commitment, "\n")

# Line plot for the relationship between age and installment commitment
ggplot(df, aes(x = age, y = installment_commitment, group = 1)) +
  geom_line(col = "purple") +
  labs(
    title = "Age vs Installment Commitment (Line Plot)",
    x = "Age",
    y = "Installment Commitment"
  ) +
  theme_minimal()


## Extra Feature Analysis 1-6: Loan Duration vs Installment Commitment
# Correlation analysis
correlation_duration_commitment <- cor(df$duration, df$installment_commitment)
cat("Analysis 2.2: Correlation between Loan Duration and Installment Commitment: ", correlation_duration_commitment, "\n")

# Line plot for the relationship between duration and installment commitment
ggplot(df, aes(x = duration, y = installment_commitment, group = 1)) +
  geom_line(col = "orange") +
  labs(
    title = "Loan Duration vs Installment Commitment (Line Plot)",
    x = "Loan Duration",
    y = "Installment Commitment"
  ) +
  theme_minimal()


## Extra Feature Analysis 1-7: Existing Credits vs Installment Commitment (Line Plot)
# Correlation analysis
correlation_existing_credits_commitment <- cor(df$existing_credits, df$installment_commitment)
cat("Analysis 2.3: Correlation between Existing Credits and Installment Commitment: ", correlation_existing_credits_commitment, "\n")

# Line plot for the relationship between existing credits and installment commitment
ggplot(df, aes(x = existing_credits, y = installment_commitment, group = 1)) +
  geom_line(col = "brown") +
  labs(
    title = "Existing Credits vs Installment Commitment (Line Plot)",
    x = "Number of Existing Credits",
    y = "Installment Commitment"
  ) +
  theme_minimal()


# Final output message
cat("\nAnalysis complete with additional features 1-5, 1-6, and 1-7.")


### TODO Objective 2: Evaluate the Impact of Employment Duration on Credit Class - Jaeden Loong Deng Ze, TP068347


### Objective 3: To Investigate the effects of different credit histories on a person’s credit classification - Keith Lo Ze Hui, TP067663


### TODO Objective 4: Assess the Effect of Loan Amount and Installment Commitment on Credit Class - Lim Wen Yi, TP067930
