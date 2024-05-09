

long <-read.csv("/Users/balwiiredavis/Desktop/project/LONG.csv", header=TRUE)

# required packages 

library(survival)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(ggfortify)

head(long)


# Summary statistics

summary(long)


# Data preparation: Calculate the time-to-event

long <- long %>%
  mutate(
    date_incl = dmy(date_incl),  
    date_event = dmy(date_event),  
    time = as.numeric(date_event - date_incl)  
  )

head(long)


# Visualize the distribution of time-to-event

ggplot(long, aes(x = time)) +
  geom_histogram(binwidth = 30, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Time-to-Event", x = "Time (days)", y = "Count")


# Categorize age into different groups

long <- long %>%
  mutate(
    age_category = case_when(
      age < 10 ~ "child",
      age >= 10 & age <= 19 ~ "teenager",
      age >= 20 & age <= 40 ~ "young adult",
      age > 40 ~ "old"
    )
  )



# Visualize the count of each age category

library(ggplot2)  # For plotting
ggplot(long, aes(x = age_category)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(
    title = "Distribution of Age Categories",
    x = "Age Category",
    y = "Count"
  )


# Visualize the count of sex

library(ggplot2)
ggplot(long, aes(x = sex)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(
    title = "Distribution of sex",
    x = "sex",
    y = "Count"
  )


# Create a Kaplan-Meier survival 

km_fit <- survfit(Surv(time, event) ~ 1, data = long)

print(km_fit)


# Plot the Kaplan-Meier survival curve 

autoplot(km_fit) +
  labs(
    title = "Kaplan-Meier Survival Curve",
    x = "Time (days)",
    y = "Survival Probability"
  ) +
  theme_minimal()


# Create a Kaplan-Meier survival object stratified by sex

km_fit_sex <- survfit(Surv(time, event) ~ sex, data = long)

# Plot Kaplan-Meier survival curves by sex

autoplot(km_fit_sex) +
  labs(
    title = "Kaplan-Meier Survival Curves by Sex",
    x = "Time (days)",
    y = "Survival Probability",
    color = "Sex"
  ) +
  theme_minimal()


# Create a Kaplan-Meier survival object stratified by age category

km_fit_age <- survfit(Surv(time, event) ~ age_category, data = long)


# Plot Kaplan-Meier survival curves by age category

autoplot(km_fit_age) +
  labs(
    title = "Kaplan-Meier Survival Curves by Age Category",
    x = "Time (days)",
    y = "Survival Probability",
    color = "Age Category"
  ) +
  theme_minimal()




# Scatter plot to visualize age vs. time-to-event, coloring by event status

ggplot(long, aes(x = age_category, y = time, color = sex)) +
  geom_point(alpha = 0.7, position = position_jitter(width = 0.2)) +
  labs(
    title = "Time-to-Event by Age Category and Sex",
    x = "Age Category",
    y = "Time (days)",
    color = "Sex"
  ) +
  theme_minimal()



# Create a violin plot to visualize time-to-event across age categories, colored by sex

ggplot(long, aes(x = age_category, y = time, fill = sex)) +
  geom_violin(trim = FALSE) +  
  labs(
    title = "Time-to-Event by Age Category and Sex",
    x = "Age Category",
    y = "Time (days)",
    fill = "Sex"
  ) +
  theme_minimal()















