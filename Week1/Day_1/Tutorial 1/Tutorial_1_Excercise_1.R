library(MASS)
# Excercise 1
y <- chem
# a)
y_min <- min(y)
y_max <- max(y)
y_mean <- mean(y)
y_median <- median(y)
# b)
plot(y)
# c)
n <- length(y)
for (i in 1:n) {
    y_bar <- sum(y[1:i] / i)
    }
for (i in 1:n) {
    sum_error <- sum((y[1:i] - y_bar)^2)
    }
s <- sqrt((1 / (n - 1)) * sum_error)
s_compare <- sd(y)
# d)

index_max <- which.max(y)
y_2 <- y[-index_max] # Negative indexing to remove
plot(y_2)
