rm()
library(readxl)
library(ggplot2)
#1b
type1 <- read_excel(path = "Data.xlsx",
                   sheet = 1, range = "AQ3:AQ13")
type2 <- read_excel(path = "Data.xlsx",
                   sheet = 1, range = "AR3:AR13")
type1 <- as.numeric(unlist(type1))
type2 <- as.numeric(unlist(type2))
#1b
t.test(type1, type2, var.equal = TRUE)
#1c
shapiro.test(type1)
shapiro.test(type2)
#1d
var.test(type1, type2)
#1e
df <- data.frame(
  value = c(type1, type2),
  type = rep(c("Type 1", "Type 2"), each = 10)
)
#Density plot 1e
ggplot(df, aes(x = value, fill = type)) +
  geom_density(alpha = 0.5) +
  ggtitle("Density Plot of Type 1 and Type 2 Measurements") +
  xlab("Measurement Values") +
  ylab("Density") +
  scale_fill_manual(values = c("lightblue", "lightgreen"))

#QQ plot 1e
ggplot(df, aes(sample = value)) +
  geom_qq() +
  geom_qq_line() +
  facet_wrap(~type, nrow = 1) +
  ggtitle("QQ-plot of Type 1 and Type 2 Measurements") +
  xlab("Theoretical Quantiles") +
  ylab("Sample Quantiles")