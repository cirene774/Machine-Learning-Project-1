
# Fashion Purchase Prediction Projects In Linear Regression
# Author: Irene Castro
# Date: 2026
# Description: Predict whether a customer will purchase
# a fashion item based on price, rating, and outfit.



# Load Libraries

library(ggplot2)
library(dplyr)
library(caret)
library(ggthemes)
library(rpart)
library(rpart.plot)


# Set Seed for Reproducibility

set.seed(123)


# Create Synthetic Dataset

fashion_data <- data.frame(
  item = sample(c("Dress", "Shirt", "Bottoms", "Shoes", "Bag"), 300, replace = TRUE),
  price = runif(300, 10, 200),
  rating = runif(300, 1, 5),
  purchased = sample(c("Yes", "No"), 300, replace = TRUE)
)

# Convert to factors
fashion_data$item <- as.factor(fashion_data$item)
fashion_data$purchased <- as.factor(fashion_data$purchased)

# Train/Test Split (70/30)

train_index <- createDataPartition(fashion_data$purchased, p = 0.7, list = FALSE)

train_data <- fashion_data[train_index, ]
test_data  <- fashion_data[-train_index, ]


# Train Model (Decision Tree)

model <- train(
  purchased ~ price + rating + item,
  data = train_data,
  method = "rpart"
)


# Predictions on Test Data
preds <- predict(model, test_data)


# Model Evaluation
conf_matrix <- confusionMatrix(preds, test_data$purchased)

print("Confusion Matrix:")
print(conf_matrix)

# -------------------------------
# 8. Feature Importance
# -------------------------------
importance <- varImp(model)

print("Feature Importance:")
print(importance)

plot(importance, main = "Feature Importance")

# -------------------------------
# 9. Visualize Decision Tree
# -------------------------------
rpart.plot(model$finalModel, main = "Decision Tree")

# -------------------------------
# 10. Prediction Probabilities
# -------------------------------
test_data$prob_yes <- predict(model, test_data, type = "prob")[, "Yes"]

ggplot(test_data, aes(x = prob_yes)) +
  geom_histogram(fill = "pink", bins = 20) +
  labs(
    title = "Probability Distribution of Purchase",
    x = "Probability of 'Yes'",
    y = "Count"
  ) +
  theme_fivethirtyeight()


# Actual vs Predicted Summary

test_data$predicted <- preds

summary_data <- test_data %>%
  group_by(item, purchased, predicted) %>%
  summarise(count = n(), .groups = "drop")


# Visualization

ggplot(summary_data, aes(x = item, y = count, fill = predicted)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ purchased) +
  labs(
    title = "Actual vs Predicted Purchases by Item",
    subtitle = "Comparison across Fashion Categories",
    x = "Item Category",
    y = "Count",
    fill = "Predicted"
  ) +
  scale_fill_brewer(palette = "Pastel1") +
  theme_fivethirtyeight() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    legend.position = "top"
  )


# Business Insight Example
test_data <- test_data %>%
  mutate(price_segment = cut(price,
                             breaks = c(0, 50, 150, 300),
                             labels = c("Low", "Medium", "High")))

ggplot(test_data, aes(x = price_segment, fill = purchased)) +
  geom_bar(position = "fill") +
  labs(
    title = "Purchase Rate by Price Segment",
    x = "Price Segment",
    y = "Proportion"
  ) +
  theme_minimal()


# Linear Regression Visualization

# Convert purchased to numeric
test_data$purchased_numeric <- ifelse(test_data$purchased == "Yes", 1, 0)

# Linear Regression Plot: Price vs Purchase Probability
ggplot(test_data, aes(x = price, y = purchased_numeric)) +
  geom_point(alpha = 0.5, color = "skyblue") +
  geom_smooth(method = "lm", color = "yellow", se = TRUE) +
  labs(
    title = "Linear Regression: Price vs Purchase Likelihood",
    subtitle = "Trend of purchase behavior based on price",
    x = "Price",
    y = "Purchase (1 = Yes, 0 = No)"
  ) +
  theme_minimal()

# Optional: Rating vs Purchase
ggplot(test_data, aes(x = rating, y = purchased_numeric)) +
  geom_point(alpha = 0.5, color = "orange") +
  geom_smooth(method = "lm", color = "pink", se = TRUE) +
  labs(
    title = "Linear Regression: Rating vs Purchase Likelihood",
    subtitle = "Trend of purchase behavior based on rating",
    x = "Rating",
    y = "Purchase (1 = Yes, 0 = No)"
  ) +
  theme_minimal()

log_model <- glm(purchased_numeric ~ price + rating, data = test_data, family = "binomial")
summary(log_model)


# End of Script
