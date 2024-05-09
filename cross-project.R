

CROSS <-read.csv("/Users/balwiiredavis/Desktop/project/CROSS.csv", header=TRUE)

library(ggplot2)  
library(dplyr)   
library(summarytools)
library(stats)
library(broom)

# Data summary
summary(CROSS)

# Distribution of age
ggplot(CROSS, aes(x = age)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Age", x = "Age", y = "Frequency")

# Distribution of gender
ggplot(CROSS, aes(x = sex)) +
  geom_bar(fill = "lightgreen", color = "black") +
  labs(title = "Distribution of Gender", x = "Gender", y = "Count")

# Exposure to forest levels
ggplot(CROSS, aes(x = exposureforest)) +
  geom_bar(fill = "lightcoral", color = "black") +
  labs(title = "Exposure to Forest", x = "Exposure Level", y = "Count")


# Create `loiasis` variable based on 'mf' or 'eyeworm'
CROSS$loiasis <- ifelse(CROSS$mf == "Yes" | CROSS$eyeworm == "Yes", "Yes", "No")

# Distribution of loiasis
ggplot(CROSS, aes(x = loiasis)) +
  geom_bar(fill = "orange", color = "black") +
  labs(title = "Loiasis Status", x = "Loiasis", y = "Count")

# Create age categories
CROSS <- CROSS %>%
  mutate(
    age_category = case_when(
      age < 25 ~ "young",
      age >= 25 & age <= 40 ~ "adults",
      age > 40 ~ "elderly"
    )
  )


# Frequency of age categories
ggplot(CROSS, aes(x = age_category)) +
  geom_bar(fill = "lightblue", color = "black") +
  labs(title = "Distribution of Age Categories", x = "Age Category", y = "Count")


# contingency table (cross-tabulation) between loiasis and exposureforest
loiasis_exposure_table <- table(CROSS$loiasis, CROSS$exposureforest)

# contingency table
print(loiasis_exposure_table)

# Chi-squared test to check for association
chi_squared_result <- chisq.test(loiasis_exposure_table)

# Display the result of the Chi-squared test
print(chi_squared_result)


# Visualize the relationship between loiasis and exposureforest 
ggplot(CROSS, aes(x = exposureforest, fill = loiasis)) +
  geom_bar(position = "dodge", color = "black") +
  labs(
    title = "Relationship between Loiasis and Exposure to Tropical Forest",
    x = "Exposure to Forest",
    y = "Count",
    fill = "Loiasis"
  )

# check for confounding and effect modification
# Stratified analysis: Relationship between loiasis and exposureforest by sex

ggplot(CROSS, aes(x = exposureforest, fill = loiasis)) +
  geom_bar(position = "dodge", color = "black") +
  facet_wrap(~ sex) +
  labs(
    title = "Loiasis and Exposure to Forest by Sex",
    x = "Exposure to Forest",
    y = "Count",
    fill = "Loiasis"
  )

# Stratified analysis: Relationship between loiasis and exposureforest by itch

ggplot(CROSS, aes(x = exposureforest, fill = loiasis)) +
  geom_bar(position = "dodge", color = "black") +
  facet_wrap(~ itch) +
  labs(
    title = "Loiasis and Exposure to Forest by Itch",
    x = "Exposure to Forest",
    y = "Count",
    fill = "Loiasis"
  )

# Stratified analysis: Relationship between loiasis and exposureforest by age category

ggplot(CROSS, aes(x = exposureforest, fill = loiasis)) +
  geom_bar(position = "dodge", color = "black") +
  facet_wrap(~ age_category) +
  labs(
    title = "Loiasis and Exposure to Forest by Age Category",
    x = "Exposure to Forest",
    y = "Count",
    fill = "Loiasis"
  )


# Logistic regression to assess the relationship between loiasis and exposureforest, controlling for confounders

logistic_model <- glm(loiasis ~ exposureforest + age + sex + itch, data = CROSS, family = "binomial")

# Summary of the logistic regression model

summary(logistic_model)



# Add interaction terms to the logistic regression model

interaction_model <- glm(
  loiasis ~ exposureforest * sex + exposureforest * age + exposureforest * itch,
  data = CROSS,
  family = "binomial"
)


# Summary of the interaction model
summary(interaction_model)


# age, sex, and itch are Confounders


# Ensure correct format
CROSS <- CROSS %>%
  mutate(
    loiasis = as.factor(loiasis),
    exposureforest = as.factor(exposureforest),
    sex = as.factor(sex),
    itch = as.factor(itch)
  )


# faceted bar chart stratified by sex, age category, and itch
ggplot(CROSS, aes(x = exposureforest, fill = loiasis)) +
  geom_bar(position = "dodge", color = "black") +
  facet_grid(sex ~ age_category + itch) +
  labs(
    title = "Loiasis and Exposure to Forest Stratified by Sex, Age Category, and Itch",
    x = "Exposure to Forest",
    y = "Count",
    fill = "Loiasis"
  )



# logistic regression model with loiasis as the outcome
logistic_model <- glm(
  loiasis ~ exposureforest + age + sex + itch,
  data = CROSS,
  family = "binomial"
)


# model summary  
summary(logistic_model)


# logistic regression model for visualization
model_tidy <- broom::tidy(logistic_model)


#  coefficient plot
ggplot(model_tidy, aes(x = estimate, y = term)) +
  geom_point() +  # Point for the estimate
  geom_errorbarh(aes(xmin = estimate - 1.96 * std.error, xmax = estimate + 1.96 * std.error), height = 0.2) +  # Error bars for 95% confidence intervals
  geom_vline(xintercept = 0, linetype = "dashed") +  # Vertical line at 0
  labs(
    title = "Logistic Regression Coefficients For loiasis",
    x = "Coefficient Estimate",
    y = "Predictor"
  ) +
  theme_minimal()

