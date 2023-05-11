library(chemometrics)
library(psych)
data(PAC)

x_value <- PAC$X
x_value_selection <-  x_value[, 366:371]
# a)
boxplot(x_value_selection)
# b)
hist(x_value[, 366])
# c)
qqnorm(x_value[, 366])
qqline(x_value[, 366], col = "steelblue", lwd = 2)
# d)
qqnorm(x_value[, 366:371])
qqline(x_value[, 366:371], col = "steelblue", lwd = 2)
# e)
pairs.panels(x_value[, 366:371])
# e)
eigen(cor(x_value[, 366:371]))
# e)
y_value <- PAC$y
cor_x_y <- cor(x_value, y_value)
hist(cor_x_y)
length(cor_x_y[abs(cor_x_y) > 0.8]) # To count number of values greater than 0.8
