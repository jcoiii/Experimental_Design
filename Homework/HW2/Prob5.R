rm()
#5a
library(BsMD)
data(BM93.e3.data, package="BsMD")

library(BsMD)

full.factorial.model <- lm(y ~ ., data = BM93.e3.data)
X <- model.matrix(full.factorial.model)
print(dim(X))
XTX <- t(X) %*% X

identical_cols <- which(duplicated(XTX), arr.ind=TRUE)
print(identical_cols)

#5b
mask <- rownames(XTX) != colnames(t(XTX))
pairs <- which(mask & (XTX == 20 | XTX == -20), arr.ind=TRUE)
print(pairs)

#5c
model <- lm(y ~ (A+B+C+D+E+F+G+H)^2, data = BM93.e3.data) 
effects <- coef(model)[-1] # omit intercept

#increasing order -> absolute effect
ordered_effects <- sort(abs(effects))

#generate expected order statistics from a half-normal dist
n <- length(ordered_effects)
expected <- qnorm((1:n) / (n + 1))
#half normal plot with the effects
library(ggplot2)
ggplot() +
  geom_point(aes(expected, ordered_effects)) +
  geom_line(aes(expected, expected)) +
  labs(x = "Expected", y = "Observed")

# reduced model based on half normal plot
reduced_model <- lm(y ~ E + A:F + C:D + C + A:E + F + B + A:G + D + A:D + A:B 
                    + A + G + B:D, data = BM93.e3.data)
summary(reduced_model)

plot(reduced_model$fitted.values, resid(reduced_model))

library(car)
cutoff <- 4 / ((nrow(BM93.e3.data) - length(coef(reduced_model))) - 1)
plot(cooks.distance(reduced_model), pch = "*", cex = 2, main = "Cooks distance")
abline(h = cutoff, col = "red")