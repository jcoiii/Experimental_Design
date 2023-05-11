rm()
library(readxl)
library(matrixcalc)
#Read data from excel
x1 <- read_excel(path = "/home/jonathan/Documents/ED/Week2/Data.xlsx",
sheet = 9, range = "AZ3:AZ15")
x2 <- read_excel(path = "/home/jonathan/Documents/ED/Week2/Data.xlsx",
sheet = 9, range = "BA3:BA15")
y <- read_excel(path = "/home/jonathan/Documents/ED/Week2/Data.xlsx",
sheet = 9, range = "BB3:BB15")
x1x1 <- read_excel(path = "/home/jonathan/Documents/ED/Week2/Data.xlsx",
sheet = 9, range = "BC3:BC15")
x2x2 <- read_excel(path = "/home/jonathan/Documents/ED/Week2/Data.xlsx",
sheet = 9, range = "BD3:BD15")
x1x2 <- read_excel(path = "/home/jonathan/Documents/ED/Week2/Data.xlsx",
sheet = 9, range = "BE3:BE15")
# Change list to numeric
x1 <- as.numeric(unlist(x1))
x2 <- as.numeric(unlist(x2))
y <- as.numeric(unlist(y))
x1x1 <- as.numeric(unlist(x1x1))
x2x2 <- as.numeric(unlist(x2x2))
x1x2 <- as.numeric(unlist(x1x2))

data <- data.frame(x1 = x1, x2 = x2, x1_sq = x1^2,
x2_sq = x2^2, x1_x2 = x1*x2, y = y)
model <- lm(y ~ x1 + x2 + x1_sq + x2_sq + x1_x2, data = data)
summary(model)