rm()
library(readxl)
library(matrixcalc)
#Read data from excel
con <- read_excel(path = "/home/jonathan/Documents/ED/Week2/Data.xlsx",
sheet = 9, range = "U3:U11")
tem <- read_excel(path = "/home/jonathan/Documents/ED/Week2/Data.xlsx",
sheet = 9, range = "V3:V11")
yie <- read_excel(path = "/home/jonathan/Documents/ED/Week2/Data.xlsx",
sheet = 9, range = "W3:W11")
# Change list to numeric
con <- as.numeric(unlist(con))
tem <- as.numeric(unlist(tem))
yie <- as.numeric(unlist(yie))
# Create Matrix X
x0 <- matrix(rep(1, 8), nrow = 1)
x1 <- matrix(con, nrow = 1)
x2 <- matrix(tem, nrow = 1)
x_t <- rbind(x0, x1, x2)
x <- t(x_t)
## Answer 1
res <- x_t %*% x
## Answer 2
is.diagonal.matrix(res)
## Answer 3
x1c <- (x1 - 1.5) / 0.5
x2c <- (x2 - 165) / 15
x_t_c <- rbind(x0, x1c, x2c)
x_c <- t(x_t_c)
res_c <- x_t_c %*% x_c
is.diagonal.matrix(res_c)
## Answer 4
x1c2 <- (x1 - 1.0) / 1.0
x2c2 <- (x2 - 150) / 30
x_t_c2 <- rbind(x0, x1c2, x2c2)
x_c2 <- t(x_t_c2)
res_c2 <- x_t_c2 %*% x_c2
is.diagonal.matrix(res_c2)