library(readxl)
#Read data from excel
machine_1 <- read_excel(path = "/home/jonathan/Documents/ED/Week2/Data.xlsx",
sheet = 1, range = "H3:H13")
machine_2 <- read_excel(path = "/home/jonathan/Documents/ED/Week2/Data.xlsx",
sheet = 1, range = "I3:I13")
# Change list to numeric
machine_1 <- as.numeric(unlist(machine_1))
machine_2 <- as.numeric(unlist(machine_2))
# Test Hypothesis with alpha = 0.05
n_1 <- length(machine_1)
n_2 <- n_1
y_1_bar <- mean(machine_1)
y_2_bar <- mean(machine_2)
s_1 <- var(machine_1)
s_2 <- var(machine_2)
v <- (s_1/n_1 + s_2/n_2)^2 / (((s_1/n_1)^2 / n_1-1) + ((s_2/n_2)^2 / n_2-1))
t <- (y_1_bar - y_2_bar)/sqrt(s_1/n_1 + s_2/n_2)