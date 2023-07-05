library(dplyr)
library(car)

# initial data observation 16 runs (this was a 2 time replication)
data <- data.frame(
  run = 1:16,
  typ = c(35, 35, 35, 35, 35, 35, 35, 35, 40, 40, 40, 40, 40, 40, 40, 40),
  K = c(18000, 18000, 18000, 18000, 26000, 26000, 26000, 26000, 18000, 18000, 18000, 18000, 26000, 26000, 26000, 26000),
  C = c(418, 418, 673, 673, 418, 418, 673, 673, 418, 418, 673, 673, 418, 418, 673, 673),
  Mass = c(41, 81, 41, 81, 41, 81, 41, 81, 41, 81, 41, 81, 41, 81, 41, 81),
  RC = c(0.655, 1.15, 0.515, 0.495, 0.66, 1.305, 0.485, 0.42, 0.885, 1.225, 0.56, 0.5, 0.855, 1.21, 0.59, 0.575)
)
C = data$C
M = data$Mass
# make as factors
data$typ <- as.factor(ifelse(data$typ == 35, -1, +1))
data$K <- as.factor(ifelse(data$K == 18000, -1, +1))
data$C <- as.factor(ifelse(data$C == 418, -1, +1))
data$Mass <- as.factor(ifelse(data$Mass == 41, -1, +1))
print(data)

t = data$typ
k = data$K
cw = data$C
m = data$Mass

par(mfrow=c(2,2))
qqnorm(data$RC)
qqline(data$RC, col = 2)
boxplot(data$RC, horizontal=TRUE, main="Box Plot", xlab="Ride Comfort")
hist(data$RC, main="Histogram", xlab="Ride Comfort")
plot(data$run, data$RC, xlab="Run", ylab="Ride Comfort",
     main="Run ")
par(mfrow=c(1,1))

par(mfrow=c(2,2))
boxplot(RC~typ, data=data, main="Ride Comfort by Tyre Pressure",
        xlab="Tyre Pressure",ylab="Ride Comfort")

boxplot(RC~K, data=data, main="Strength by Spring constant",
        xlab="Spring constant",ylab="Ride Comfort")

boxplot(RC~C, data=data, main="Strength by Damping constant",
        xlab="Damping constant",ylab="Ride Comfort")

boxplot(RC~Mass, data=data, main="Strength by Sprung mass",
        xlab="Sprung mass",ylab="Ride Comfort")

par(mfrow=c(1,1))

## Fit a model with up to second order interactions.
model = aov(RC~(typ+K+C+Mass)^2,data=data)
summary(model)

## Stepwise regression based on AIC.
sreg = step(model,direction="backward")
summary(sreg)

redmod = aov(formula = RC ~ typ + C + Mass + C:Mass, data = data)
summary(redmod)

## Print adjusted R squared.
summary.lm(redmod)$adj.r.squared

## Fit a model with all effects.
model_2 = aov(RC~(typ+K+C+Mass)^4,data=data)

## Save effects in a vector, but remove intercept.
model_2ef = model_2$effects
model_2ef = model_2ef[-1]

## Sort effects and save labels.
sef = model_2ef[order(model_2ef)]
qlab = names(sef)

large = c(1,2)
sef = sef[-large]
qlab = qlab[-large]

## Generate theoretical quantiles.
ip = ppoints(length(sef))
zp = qnorm(ip)

## Generate normal probability plot of all effects (excluding the
## intercept).
plot(zp,sef,
     ylab="Effect", xlab="Theoretical Quantiles",
     main="Normal Probability Plot of Saturated Model Effects")
qqline(sef, col=2)
abline(h=0, col=4)
text(-2,90,"Direction and Batch not shown",pos=4)

small = c(6:(length(sef)-3))
small2 = c((length(sef)-4):(length(sef)-3))
text(zp[-small],sef[-small],label=qlab[-small],pos=4,cex=0.8)
text(zp[small2],sef[small2],label=qlab[small2],pos=2,cex=0.8)

small = c(6:(length(sef)-3))
small2 = c((length(sef)-4):(length(sef)-3))
text(zp[-small],sef[-small],label=qlab[-small],pos=4,cex=0.8)
text(zp[small2],sef[small2],label=qlab[small2],pos=2,cex=0.8)
par(mfrow=c(1,1))

## Plot residuals versus predicted response.
plot(predict(redmod),redmod$residuals,ylab="Residual",
     xlab="Predicted RC")
abline(h=0)
par(mfrow=c(1,1))

par(mfrow=c(2,2))
qqnorm(redmod$residuals)
qqline(redmod$residuals, col = 2)
abline(h=0)
boxplot(redmod$residuals, horizontal=TRUE, main="Box Plot", xlab="Residual")
hist(redmod$residuals, main="Histogram", xlab="Residual")
plot(data$run, redmod$residuals, xlab="Actual Run Order", ylab="Residual",
     main="Run Order Plot")
par(mfrow=c(1,1))


library(MASS)
bc = boxcox(RC ~ typ + C + Mass + C:Mass, data = data,
            lambda = seq(-2, 2, length = 50))
title("Box-Cox Transformation")
lambda = bc$x[which.max(bc$y)]
lambda

library(psych)
## Generate new transformed response variable and add to data frame.
newsRC = (data$RC^lambda - 1)/
  (lambda*(geometric.mean(data$RC)^(lambda-1)))
data =  data.frame(data,newsRC)
summary(aov(formula = RC ~ typ + C + Mass + C:Mass, data = data))

cm = C*M
data = data.frame(data,cm)

newredmod = lm(formula = RC ~ typ + C + Mass + C:Mass, data = data)
summary.lm(newredmod)

## Plot residuals versus predicted, transformed response.
par(mfrow=c(1,1))
plot(predict(newredmod),newredmod$residuals,ylab="Residual",
     xlab="Predicted Transformed Ride Comfort")
abline(h=0)

## Generate four plots of residuals based on transformed response.
par(mfrow=c(2,2))
qqnorm(newredmod$residuals)
qqline(newredmod$residuals, col = 2)
abline(h=0)
boxplot(newredmod$residuals, horizontal=TRUE, main="Box Plot", 
        xlab="Residual")
hist(newredmod$residuals, main="Histogram", xlab="Residual")
plot(data$run, newredmod$residuals, xlab="Run", 
     ylab="Residual", main="Run Order Plot")
par(mfrow=c(1,1))

## Rearrange data so that factors and levels are in single columns.
n = length(data$RC[t==1])
kq = qt(.975,n-1)

group = rep(1:4,each=length(data$RC))
nstr = rep(newsRC,4)
level = c(t,k,cw,m)
dflong = data.frame(group,level,nstr)

gmn = aggregate(x=dflong$nstr,by=list(dflong$group,dflong$level),FUN="mean")
gsd = aggregate(x=dflong$nstr,by=list(dflong$group,dflong$level),FUN="sd")
cibar = kq*gsd[3]/sqrt(n)
cgroup = rep(c("typ","K","C","Mass"),2)

dfp = data.frame(cgroup,gmn,gsd[3],cibar)
names(dfp)=c("cgroup","group","level","tmean","std","cibar")

library(lattice)
par(mfrow=c(1,1))
xyplot(tmean~level|cgroup,data=dfp,layout=c(4,1),
       ylab="Transformed RC",xlab="Factor Levels", type="b",
       panel = function(x, y, ...){
         panel.xyplot(x, y, ...)
         panel.abline(h = mean(newsRC), lty = 2, col = 2)})

## 2-way interaction plot showing means for all combinations of
## levels for the two factors.

## Compute means for plotting.
dfi = data.frame(t,k,cw,m,newsRC)

mntk = aggregate(x=dfi$newsRC,by=list(dfi$t,dfi$k),FUN="mean")
mntc = aggregate(x=dfi$newsRC,by=list(dfi$t,dfi$cw),FUN="mean")
mntm = aggregate(x=dfi$newsRC,by=list(dfi$t,dfi$m),FUN="mean")

mnkt = aggregate(x=dfi$newsRC,by=list(dfi$k,dfi$t),FUN="mean")
mnkc = aggregate(x=dfi$newsRC,by=list(dfi$k,dfi$cw),FUN="mean")
mnkm = aggregate(x=dfi$newsRC,by=list(dfi$k,dfi$m),FUN="mean")


mnct = aggregate(x=dfi$newsRC,by=list(dfi$cw,dfi$t),FUN="mean")
mnck = aggregate(x=dfi$newsRC,by=list(dfi$cw,dfi$k),FUN="mean")
mncm = aggregate(x=dfi$newsRC,by=list(dfi$cw,dfi$m),FUN="mean")

mnmt = aggregate(x=dfi$newsRC,by=list(dfi$m,dfi$t),FUN="mean")
mnmk = aggregate(x=dfi$newsRC,by=list(dfi$m,dfi$k),FUN="mean")
mnmc = aggregate(x=dfi$newsRC,by=list(dfi$m,dfi$cw),FUN="mean")

xcol = rbind(mntk,mntc,mntm, mnkt,mnkc,mnkm,
             mnct,mnck,mncm, mnmt,mnmk,mnmc)
gi = rep(c("t*k","t*c","t*m",
           "k*t","k*c","k*m",
           "c*t","c*k","c*m",
           "m*t","m*k","m*c"),each=4)
dff = data.frame(gi,xcol)

## Generate the lattice plot.
sp = c(T,F,F,F, F,T,F,F, F,F,T,F, F,F,F,T)
xyplot(x ~ Group.1 | gi, data=dff, group=Group.2,
       layout=c(4,4), skip=sp, 
       ylab = "Transformed Ride Comfort", xlab = "Factor Level",
       main = "Blue: low level, Pink: high level",
       type=c("p","l"), pch=20, cex=1, col=c(4,6),
       panel=function(x,y,...){panel.superpose(x,y,...)})
trellis.focus("toplevel") ## has coordinate system [0,1] x [0,1]
panel.text(0.200, 0.200, "C",     cex=1)
panel.text(0.400, 0.400, "K", cex=1)
panel.text(0.600, 0.600, "Mass",      cex=1)
panel.text(0.800, 0.800, "Typ",      cex=1)
trellis.unfocus()

#Simulation Study
n_sim <- 1000 

# true effects
true_typ <- 0.65
true_K <- 0.85
true_C <- 0.56
true_M <- 0.66
true_error_sd <- 0.5

# Initialize vectors to store results
est_typ <- numeric(n_sim)
est_K <- numeric(n_sim)
est_C <- numeric(n_sim)
est_M <- numeric(n_sim)

# Run simulation
for(i in 1:n_sim){
  
  # Generate data
  typ <- rnorm(16, mean = 35, sd = 2.5) 
  K <- rnorm(16, mean = 22000, sd = 4000) 
  C <- rnorm(16, mean = 545.5, sd = 127.5) 
  M <- rnorm(16, mean = 61, sd = 20) 
  error <- rnorm(16, mean = 0, sd = true_error_sd)
  
  # Calculate response variable
  RC <- true_typ*typ + true_K*K + true_C*C + true_M*M + error
  
  # Fit model
  data <- data.frame(RC, typ, K, C, M)
  model <- lm(RC ~ .^2, data = data)
  
  # Store estimated coefficients
  est_typ[i] <- coef(model)["typ"]
  est_K[i] <- coef(model)["K"]
  est_C[i] <- coef(model)["C"]
  est_M[i] <- coef(model)["M"]
}

# Calculate bias and MSE
bias_typ <- mean(est_typ - true_typ)
mse_typ <- mean((est_typ - true_typ)^2)

bias_K <- mean(est_K - true_K)
mse_K <- mean((est_K - true_K)^2)

bias_C <- mean(est_C - true_C)
mse_C <- mean((est_C - true_C)^2)

bias_M <- mean(est_M - true_M)
mse_M <- mean((est_M - true_M)^2)

# Print results
print(paste0("typ: bias = ", bias_typ, ", MSE = ", mse_typ))
print(paste0("K: bias = ", bias_K, ", MSE = ", mse_K))
print(paste0("C: bias = ", bias_C, ", MSE = ", mse_C))
print(paste0("M: bias = ", bias_M, ", MSE = ", mse_M))

par(mfrow = c(2, 2))
# histogram of the estimated typ effects
hist(est_typ, main = "Histogram of estimated typ effects", xlab = "Estimated typ effect")
abline(v = true_typ, col = "red", lwd = 2)
# histogram of the estimated K effects
hist(est_K, main = "Histogram of estimated K effects", xlab = "Estimated K effect")
abline(v = true_K, col = "red", lwd = 2)
# histogram of the estimated C effects
hist(est_C, main = "Histogram of estimated C effects", xlab = "Estimated C effect")
abline(v = true_C, col = "red", lwd = 2)
# histogram of the estimated M effects
hist(est_M, main = "Histogram of estimated M effects", xlab = "Estimated M effect")
abline(v = true_M, col = "red", lwd = 2)
par(mfrow = c(1, 1))
