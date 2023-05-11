rm()
library(readxl)
library(matrixcalc)
library(ggplot2)
library(agricolae)

path_file <- "/home/jonathan/Documents/ED/Week2/Data.xlsx"
cs <- read_excel(path = path_file, sheet = 5, range = "B3:B27")
tg <- read_excel(path = path_file, sheet = 5, range = "C3:C27")
ca <- read_excel(path = path_file, sheet = 5, range = "D3:D27")
lh <- read_excel(path = path_file, sheet = 5, range = "E3:E27")

cs <- as.numeric(unlist(cs))
tg <- as.numeric(unlist(tg))
ca <- as.numeric(unlist(tg))
lh <- as.numeric(unlist(lh))
df <- data.frame(cs, tg, ca, lh)

# Calculating the factor effects
a <- (df$lh[df$cs == 1 & df$tg == -1 & df$ca == -1])
ab <- (df$lh[df$cs == 1 & df$tg == 1 & df$ca == -1])
ac <- (df$lh[df$cs == 1 & df$tg == -1 & df$ca == 1])
abc <- (df$lh[df$cs == 1 & df$tg == 1 & df$ca == 1])
a_plus <- a + ab + ac + abc
average_low_a <- mean(a+ab+abc)
average_high_a <- mean(df$lh[df$cs == 1 & df$tg == 1 & df$ca == 1])
effect_a <- average_high_a - average_low_a

average_low_b <- mean(df$lh[df$cs == -1 & df$tg == -1 & df$ca == -1])
average_high_b <- mean(df$lh[df$cs == 1 & df$tg == 1 & df$ca == 1])

effect_b <- average_high_b - average_low_b

average_low_c <- mean(df$lh[df$cs == -1 & df$tg == -1 & df$ca == -1])
average_high_c <- mean(df$lh[df$cs == 1 & df$tg == 1 & df$ca == 1])

effect_c <- average_high_c - average_low_c
