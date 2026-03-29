# Install & load required packages
if (!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)

# Create dataset
set.seed(42)
food_data <- data.frame(
  Food = c("Pizza", "Burger", "Sushi", "Pasta", "Salad", "Tacos", "Cake", "Steak"),
  Popularity = runif(8, 60, 100),
  Price = runif(8, 5, 40),
  Rating = runif(8, 3.0, 5.0)
)

# Prepare numeric features for clustering
features <- scale(food_data[, c("Popularity", "Price", "Rating")])

# Apply K-means clustering
set.seed(42)
kmeans_model <- kmeans(features, centers = 3, nstart = 25)

# Attach cluster labels
food_data$Cluster <- as.factor(kmeans_model$cluster)


# SCATTER PLOT + LINEAR REGRESSION
ggplot(food_data, aes(x = Popularity, y = Rating, color = Cluster, label = Food)) +
  geom_point(size = 5) +
  geom_text(vjust = -0.8) +
  geom_smooth(method = "lm", se = TRUE, color = "#C71585", linetype = "dashed") +
  scale_color_manual(values = c("#FFB6C1", "#FF69B4", "#FF1493")) +
  labs(
    title = "K-Means Clustering with Linear Regression",
    x = "Popularity Score",
    y = "User Rating"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(color = "#FF1493", size = 16, face = "bold"),
    axis.title = element_text(color = "#C71585"),
    legend.title = element_text(color = "#C71585")
  )

# -------------------------------
# HISTOGRAM (POPULARITY DISTRIBUTION)
# -------------------------------
ggplot(food_data, aes(x = Popularity)) +
  geom_histogram(
    binwidth = 5,
    fill = "#FF69B4",
    color = "#C71585",
    alpha = 0.8
  ) +
  labs(
    title = "Distribution of Food Popularity",
    x = "Popularity Score",
    y = "Count"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(color = "#FF1493", size = 16, face = "bold"),
    axis.title = element_text(color = "#C71585")
  )

# PIE CHART (CLUSTER PERCENTAGES)

cluster_counts <- as.data.frame(table(food_data$Cluster))
colnames(cluster_counts) <- c("Cluster", "Count")

# Calculate percentages
cluster_counts$Percentage <- (cluster_counts$Count / sum(cluster_counts$Count)) * 100
cluster_counts$Label <- paste0(cluster_counts$Cluster, " (", round(cluster_counts$Percentage, 1), "%)")

ggplot(cluster_counts, aes(x = "", y = Count, fill = Cluster)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  geom_text(aes(label = Label), position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = c("#FFB6C1", "#FF69B4", "#FF1493")) +
  labs(title = "Cluster Distribution (%)") +
  theme_void() +
  theme(
    plot.title = element_text(color = "#FF1493", size = 16, face = "bold")
  )

# END OF SCRIPT