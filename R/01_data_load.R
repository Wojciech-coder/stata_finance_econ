data("Boston")
Boston <- as.data.frame(Boston)

str(Boston)
head(Boston)
summary(Boston)

# medv capped at 50 for some observations
sum(Boston$medv == 50)
