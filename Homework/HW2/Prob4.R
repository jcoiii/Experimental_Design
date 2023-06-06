rm()
#4b and c
library(DoE.base)

corrPlot(VSGFS)
summary(lm(VSGFS, response = "Yield"))
plot(VSGFS)
anova(lm(VSGFS, response = "Yield"))
library("car")
model_4b <-lm(VSGFS, response = "Yield")
print(model_4b)
par(mfrow = c(2, 2))
  plot(model_4b,1) # res vs fitted val
  plot(model_4b,2) # normal Q-Q
  plot(model_4b,3)  # Scale-Location (sqrt|standardized residuals| vs fitted val)
  plot(model_4b,5)  # res vs leverage
par(mfrow = c(1, 1))
summary(model_4b)



