rm()
library(readxl)
library(matrixcalc)
library(ggplot2)
library(agricolae)
##1
#Read data from excel
tech <- read_excel(path = "/home/jonathan/Documents/ED/Week2/Data.xlsx",
sheet = 2, range = "G3:G19")
stre <- read_excel(path = "/home/jonathan/Documents/ED/Week2/Data.xlsx",
sheet = 2, range = "H3:H19")
tech <- as.numeric(unlist(tech))
stre <- as.numeric(unlist(stre))
#Create data frame
data <- data.frame(tech, stre)
#Perform anova test
res <- aov(stre ~ tech, data)
summary(res)
#plot(res)
#Plotting
means <- aggregate(stre ~ tech, data = data, FUN = mean)
##2
ggplot(data, aes(x=as.factor(tech), y=stre)) +
  geom_boxplot(fill="lightblue", color="black") +
  labs(x="Mixing Technique", y="Tensile Strength") +
  ggtitle("Box Plot of Tensile Strength by Mixing Technique")

##3
# perform Fisher LSD test
fisher.lsd <- LSD.test(res, "tech", alpha = 0.05)
##4
# Create model
model <- lm(stre ~ tech, data = data)
##5
# Make NP plot
qqnorm(model$residuals, main = "Normal Probability Plot of Residuals")
qqline(model$residuals)
summary(model)
# Residuals v predicted plot
plot(model$fitted.values, model$residuals,xlab = "Predicted Tensile Strength", ylab = "Residuals",
     main = "Residuals vs. Predicted Tensile Strength")

##6
ggplot(data, aes(x = tech, y = stre)) +
  geom_point() +
  labs(x = "Mixing Technique", y = "Tensile Strength", title = "Mixing Technique vs. Tensile Strength")

