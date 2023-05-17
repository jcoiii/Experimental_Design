rm(list = ls())
library(readxl)
library(car)
library(DoE.base)
library(FrF2)
library(modelsummary)
library(mixexp)

path_file <- "/home/jonathan/Documents/ED/Week2/Data.xlsx"
dat <- read_excel(path = path_file, sheet = 7,  range = "OF3:OL19", 
col_types = "guess")
dat <- data.frame(dat)                 #6 factors; 2^(6-2)  IV design
names(dat)[7] <- 'y'
mod <- lm(y ~ (.)^2, dat)
aliases(mod, code=TRUE,condense=TRUE)      # several aliases, try ME first
summary(mod <- lm(y ~  A + B + C + D + E + F, data = dat))
summary(mod <- lm(y ~  (A + D + E)^2, dat))
summary(mod <- lm(y ~  A + D + E + D:E, dat))  #D:E aliased

ModelPlot(mod,dimensions = list(x1="A",x2="D",x3="E"),
          slice = list(process.vars=c(z1=-1, z2=-1, z3=-1)),
          main="z1=-1, z2=-1, z3=-1",
          constraints=FALSE,contour=TRUE,cuts=10,fill=FALSE,
          axislabs=c("A","D","E"),
          cornerlabs = c("A", "D", "E"),pseudo=FALSE)
# several specification possible for layer thickness between 140 and 160
predict(mod, newdata = data.frame(A=0.4,D=0.1,E=0.6),interval = "prediction")

summary(mod.1 <- lm(y ~  A + D + E + I(D*E), dat))  #D:E aliased
tmp <- list(A=seq(-1,1,by=0.1),D=seq(-1,1,by=0.1),E=seq(-1,1,by=0.1))
new.data <- expand.grid(tmp)
new.data$fit <- predict(mod.1,new.data)
contourplot(fit ~ D * E,new.data,xlab="D",ylab="E",cuts=20)
contourplot(fit ~ D * E|A, new.data,xlab="D",ylab="E",cuts=5)  # nice plots per cut value of A
predict(mod, newdata = data.frame(A=0.2,D=0.5,E=0.5),interval = "prediction")
# the 95% prediction interval is wide!
