rm()
library(readxl)
library(matrixcalc)
library(ggplot2)
library(agricolae)
##1
#Read data from excel
loom <- read_excel(path = "/home/jonathan/Documents/ED/Week2/Data.xlsx",
sheet = 2, range = "DA3:DA28")
outp <- read_excel(path = "/home/jonathan/Documents/ED/Week2/Data.xlsx",
sheet = 2, range = "DB3:DB28")
loom <- as.numeric(unlist(loom))
loom <- factor(loom)
outp <- as.numeric(unlist(outp))
df <- data.frame(loom, outp)

# Perform the ANOVA test
model <- aov(outp ~ loom, data=df)
ss_1 <- sum((loom - mean(loom))^2)
summary(model)