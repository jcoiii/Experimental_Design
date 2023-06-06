rm()
#2a
library(nlme)
dat <- as.data.frame(Oxide)
model_2a <- lm(Thickness ~ Source + Site, data = dat)
print(anova(model_2a))

#2b
library(lme4)
model_2b <- lme(Thickness ~ 1, random = ~1|Lot/Wafer, data = dat)
conf_int <- intervals(model_2b)
print(conf_int)
print(summary(model_2b))

#2c
library(lattice)
dat$Predicted <- predict(model_2b)

xyplot(Thickness ~ as.numeric(as.character(dat$Wafer)) | Lot, data = dat, type = c("p", "r"),
       panel = function(x, y, ...) {
         panel.xyplot(x, y, ...)
         panel.abline(lm(y ~ x), col = "red")
       },
       xlab = "Wafer", ylab = "Thickness")
