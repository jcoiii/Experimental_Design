rm()
library(readxl)
library(matrixcalc)
library(ggplot2)
library(agricolae)
##1
#Read data from excel
dosa <- read_excel(path = "/home/jonathan/Documents/ED/Week2/Data.xlsx",
sheet = 2, range = "N3:N15")
acti <- read_excel(path = "/home/jonathan/Documents/ED/Week2/Data.xlsx",
sheet = 2, range = "O3:O15")
resi <- read_excel(path = "/home/jonathan/Documents/ED/Week2/Data.xlsx",
sheet = 2, range = "P3:P15")
dosa <- as.numeric(unlist(dosa))
dosa_f <- factor(dosa)
acti <- as.numeric(unlist(acti))
resi <- as.numeric(unlist(resi))

# Perform the ANOVA test
res <- aov(acti ~ dosa_f, data = data.frame(dosa_f, acti))
summary(res)

# Perform pairwise comparisons
pairwise <- TukeyHSD(res)

residuals <- resid(res)

# Plot the residuals
plot(residuals ~ fitted(res), main = "Residuals vs. Fitted Values")
abline(h = 0, lty = 2)