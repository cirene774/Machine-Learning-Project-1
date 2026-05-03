# Setup
if (!requireNamespace("sqldf", quietly = TRUE)) {
  stop("Package 'sqldf' is required. Please install it with install.packages('sqldf').")
}
library(sqldf)

# Data 
products <- data.frame(
  product_id = c(1, 2, 3, 4),
  product_name = c("Coca-Cola", "Diet Coke", "Sprite", "Fanta"),
  category = c("Soda", "Soda", "Soda", "Soda"),
  stringsAsFactors = FALSE
)

sales <- data.frame(
  product_id = c(1, 1, 2, 3, 5),
  region = c("North", "South", "North", "East", "West"),
  units_sold = c(100, 150, 200, 130, 90),
  stringsAsFactors = FALSE
)

# Join once 
joined_data <- sqldf("
  SELECT 
    p.product_name,
    p.category,
    s.region,
    s.units_sold
  FROM products p
  LEFT JOIN sales s
    ON p.product_id = s.product_id
")

# Market share (only among listed products) 
market_share <- sqldf("
  SELECT
    product_name,
    COALESCE(SUM(units_sold), 0) AS total_units,
    ROUND(
      100.0 * COALESCE(SUM(units_sold), 0) /
      SUM(COALESCE(SUM(units_sold), 0)) OVER (),
      2
    ) AS pct_share
  FROM joined_data
  GROUP BY product_name
")

# Output 
print(joined_data)
print(market_share)

# Pie chart
labels <- paste0(
  market_share$product_name, " (", market_share$pct_share, "%)"
)

colors <- c("#E41A1C", "#377EB8", "#4DAF4A", "#FF7F00")

pie(
  market_share$total_units,
  labels = labels,
  col = colors,
  main = "Coca-Cola Product Market Share"
)