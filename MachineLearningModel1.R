# Install packages (run once)
install.packages("ggplot2")
install.packages("caTools")
install.packages("caret")

# Load libraries
library(ggplot2)
library(caTools)
library(caret)

data(iris)
head(iris)
summary(iris) 

ggplot(iris, aes(x = Petal.Length, y = Petal.Width, color = Species)) +
  geom_point() +
  theme_minimal() 

set.seed(123)

# Create random indices
index <- sample(1:nrow(iris), 0.7 * nrow(iris))

train_data <- iris[index, ]
test_data  <- iris[-index, ]

install.packages("rpart")
library(rpart)

model <- rpart(Species ~ ., data = train_data, method = "class")

install.packages("rpart.plot")
library(rpart.plot)

rpart.plot(model)

predictions <- predict(model, test_data, type = "class") 

confusionMatrix(predictions, test_data$Species) 

model2 <- train(Species ~ ., data = train_data, method = "multinom")

pred2 <- predict(model2, test_data)

confusionMatrix(pred2, test_data$Species) 