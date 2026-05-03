# Install package if needed
install.packages("sqldf")

library(sqldf)

# Create Coca-Cola product table
products <- data.frame(
  product_id = c(1, 2, 3, 4),
  product_name = c("Coca-Cola", "Diet Coke", "Sprite", "Fanta"),
  category = c("Soda", "Soda", "Soda", "Soda")
)

# Create sales table
sales <- data.frame(
  product_id = c(1, 1, 2, 3, 5),
  region = c("North", "South", "North", "East", "West"),
  units_sold = c(100, 150, 200, 130, 90)
)

# SQL JOIN executed inside R
coke_report <- sqldf("
  SELECT 
    p.product_name,
    p.category,
    s.region,
    s.units_sold
  FROM products p
  LEFT JOIN sales s
  ON p.product_id = s.product_id
")

# View result
print(coke_report)

market_share <- sqldf("
  SELECT 
    p.product_name,
    SUM(s.units_sold) AS total_units,
    ROUND(100.0 * SUM(s.units_sold) / 
      (SELECT SUM(units_sold) FROM sales), 2) AS pct_share
  FROM products p
  LEFT JOIN sales s
  ON p.product_id = s.product_id
  GROUP BY p.product_name
")

print(market_share)


# Remove NA values (if any product had no sales)
market_share_clean <- na.omit(market_share)

# Create labels with percentages
labels <- paste(
  market_share_clean$product_name,
  paste0(market_share_clean$pct_share, "%")
)

# Pie chart
pie(
  market_share_clean$total_units,
  labels = labels,
  col = rainbow(length(labels)),
  main = "Coca-Cola Product Market Share"
)


