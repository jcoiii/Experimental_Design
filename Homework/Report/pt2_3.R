library(dplyr)
library(FrF2)
data <- data.frame(
  A = c(-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1),
  B = c(-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1),
  C = c(-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1),
  D = c(-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1),
  E = c(1,-1,-1,1,-1,1,1,-1,-1,1,1,-1,1,-1,-1,1),
  Response = c(0.282,0.35,0.296,0.417,0.303,0.392,0.277,0.372,0.3,0.39,0.287,0.37,0.285,0.356,0.295,0.411)
)
run <- 1:16
design <- FrF2(nlevels = 2, nfactors = 5, replications = 1, resolution = 4, randomize = FALSE)
print(design)
## Generate four plots.
par(mfrow=c(2,2))
qqnorm(data$Response)
qqline(data$Response, col = 2)
boxplot(data$Response, horizontal=TRUE, main="Box Plot", xlab="Drag Coefficient")
hist(data$Response, main="Histogram", xlab="Drag Coefficient")
plot(run, data$Response, xlab="Runr", ylab="Drag Coefficient",
     main="Experiment Run Plot")
par(mfrow=c(1,1))

# Plots of responses versus factor columns
par(mfrow=c(2,3))
boxplot(Response~design$A, data=data, main="Drag Coef by Spoiler Angle",
        xlab="Spoiler Angle",ylab="Drag Coefficient")

boxplot(Response~design$B, data=data, main="Drag Coef by Spoiler Length",
        xlab="Spoiler Length",ylab="Drag Coefficient")

boxplot(Response~design$C, data=data, main="Drag Coef by Rear End Height",
        xlab="Rear End Height",ylab="Drag Coefficient")

boxplot(Response~design$D, data=data, main="Drag Coef by Tail Angle",
        xlab="Tail Angle",ylab="Drag Coefficient")

boxplot(Response~design$E, data=data, main="Drag Coef by Diffuser Angle",
        xlab="Diffuser Angle",ylab="Drag Coefficient")
par(mfrow=c(1,1))


# Fit a linear regression model
model <- lm(Response ~ A + B + C + D + E + A:B + A:C + A:D + A:E + B:C
            + B:D + B:E+ C:E+ D:E, data = data)
summary(model)

## Generate normal probability plot of the effects.
## Save parameters in a vector, but remove intercept and the two largest
## parameters (b & l).
qef = model$coef
ex = c(1,2)
qef = qef[-ex]

## Sort effects and save labels.
sef = qef[order(qef)]
qlab = names(sef)


## Generate theoretical quantiles.
ip = ppoints(length(sef))
zp = qnorm(ip)

## Generate normal probability plot of all effects (excluding the
## intercept).  Bands and length are not shown.
plot(zp,sef, 
     ylab="Parameter Estimate", xlab="Theoretical Quantiles",
     main="Normal Probability Plot of Saturated Model Effects")
qqline(sef, col=2)
abline(h=0, col=4)
## Add labels for largest 4 effects
small2 = c(1:(length(sef)-3))
text(zp[1],sef[1],label=qlab[1],pos=4,cex=1, font=2)
text(zp[-small2],sef[-small2],label=qlab[-small2],pos=2,cex=1, font=2)
par(mfrow=c(1,1))

## Generate effect estimates.
model_2 = lm(Response ~ A + B + E + A:B + A:D + A:E + B:C + B:E + C:E,data=data)
summary(model_2)
anova(model_2)

## Generate four plots.
par(mfrow=c(2,2))
qqnorm(model_2$residuals)
qqline(model_2$residuals, col = 2)
abline(h=0)
boxplot(model_2$residuals, horizontal=TRUE, main="Box Plot", xlab="Residual")
hist(model_2$residuals, main="Histogram", xlab="Residual")
plot(run, model_2$residuals, xlab="Run Order", ylab="Residual",
     main="Run Order Plot")
par(mfrow=c(1,1))

## Plot residuals versus predicted response.
par(mfrow=c(1,1))
plot(predict(model_2),model_2$residuals,ylab="Residual",
     xlab="Predicted Drag Coeff", col=4)
abline(h=0, col=2)
par(mfrow=c(1,1))

## Plots of residuals versus the factor variables
par(mfrow=c(2,3))
plot(model_2$residuals~A, data=data, main="Residuals by Spoiler Angle",
     xlab="Spoiler Angle",ylab="Residual", xlim=c(-1.5,1.5), col=4)

plot(model_2$residuals~B, data=data, main="Residuals by Spoiler Length",
     xlab="Spoiler Length",ylab="Residual", xlim=c(-1.5,1.5), col=4)

plot(model_2$residuals~C, data=data, main="Residuals by Rear End Height",
     xlab="Rear End Height",ylab="Residual", xlim=c(-1.5,1.5), col=4)

plot(model_2$residuals~D, data=data, main="Residuals by Tail Angle",
     xlab="Tail Angle",ylab="Residual", xlim=c(-1.5,1.5), col=4)

plot(model_2$residuals~E, data=data, main="Residuals by Diffuser Angle",
     xlab=" Diffuser Angle",ylab="Residual", xlim=c(-1.5,1.5), col=4)
par(mfrow=c(1,1))

## Add log(Drag Coeff) to the data frame.
logdist = log(data$Response)
data = data.frame(data,logdist)

## Fit a model with up to second order interactions.
z = lm(logdist~A + B + E + A:B + A:D + A:E + B:C + B:E + C:E,data=data)
summary(z)

## Fit model with significant effects.
z = lm(logdist~A + B + E + A:B + A:E + B:C + C:E,data=data)
summary(z)

anova(z)

## Generate four plots.
par(mfrow=c(2,2))
qqnorm(z$residuals)
qqline(z$residuals, col = 2)
abline(h=0)
boxplot(z$residuals, horizontal=TRUE, main="Box Plot", xlab="Residual")
hist(z$residuals, main="Histogram", xlab="Residual")
plot(run, z$residuals, xlab="Run", ylab="Residual",
     main="Run Plot")
par(mfrow=c(1,1))

## Plot residuals versus predicted response.
par(mfrow=c(1,1))
plot(predict(z),z$residuals,ylab="Residual",
     xlab="Predicted log(Drag Coeff)", col=4)
abline(h=0,col=2)
par(mfrow=c(1,1))

## Plots of residuals versus the factor variables
par(mfrow=c(2,3))
plot(z$residuals~A, data=data, main="Residuals by Spoiler Angle",
     xlab="Spoiler Angle",ylab="Residual", xlim=c(-1.5,1.5), col=4)

plot(z$residuals~B, data=data, main="Residuals by Spoiler Length",
     xlab="Spoiler Length",ylab="Residual", xlim=c(-1.5,1.5), col=4)

plot(z$residuals~C, data=data, main="Residuals by Rear End Height",
     xlab="Rear End Height",ylab="Residual", xlim=c(-1.5,1.5), col=4)

plot(z$residuals~D, data=data, main="Residuals by Tail Angle",
     xlab="Tail Angle",ylab="Residual", xlim=c(-1.5,1.5), col=4)

plot(z$residuals~E, data=data, main="Residuals by Diffuser Angle",
     xlab=" Diffuser Angle",ylab="Residual", xlim=c(-1.5,1.5), col=4)
par(mfrow=c(1,1))

## Rearrange data so that factors and levels are in single columns.
group = rep(1:5,each=length(data$logdist))
newd = rep(logdist,5)
level = c(design$A,design$B,design$C,design$D,design$E)
dflong = data.frame(group,level,newd)
dflong = dflong[order(group,level),]

## Generate means by factor and level.
gmn = aggregate(x=dflong$newd,by=list(dflong$group,dflong$level),FUN="mean")
cgroup = rep(c("A","B","C","D","E"),1)
cgroup = cgroup[-8]
dfp = data.frame(cgroup,gmn)
names(dfp)=c("cgroup","group","level","tmean")

## Attach lattice library and generate main effects plot.
library(lattice)
xyplot(tmean~level|cgroup,data=dfp,layout=c(5,1),
       ylab="log(Distance)",xlab="Factor Levels", type="b",
       panel = function(x, y, ...){
         panel.xyplot(x, y, ...)
         panel.abline(h = mean(logdist), lty = 2, col = 2)})
par(mfrow=c(1,1))

# Simulation Study
n_sim <- 1000

# True effects
true_A <- 0.0458125
true_B <- 0.0041875
true_E <- 0.0061875
true_AB <- 0.0060625
true_AE <- 0.0140625
true_BC <- 0.0011875

# Initialize vectors to store results
est_A <- numeric(n_sim)
est_B <- numeric(n_sim)
est_E <- numeric(n_sim)
est_AB <- numeric(n_sim)
est_AE <- numeric(n_sim)
est_BC <- numeric(n_sim)

# Run simulation
for (i in 1:n_sim) {
  
  # Generate data
  A <- rnorm(16, mean = 0, sd = 1)
  B <- rnorm(16, mean = 0, sd = 1)
  C <- rnorm(16, mean = 0, sd = 1)
  E <- rnorm(16, mean = 0, sd = 1)
  error <- rnorm(16, mean = 0, sd = 0.003326)
  
  # Calculate response variable
  Response <- true_A * A + true_B * B + true_E * E + true_AB * A * B + true_AE * A * E + true_BC * B * C + error
  
  # Fit model
  data <- data.frame(Response, A, B, E)
  model <- lm(Response ~ A + B + E + A:B + A:E + B:C, data = data)
  
  # Store estimated coefficients
  est_A[i] <- coef(model)["A"]
  est_B[i] <- coef(model)["B"]
  est_E[i] <- coef(model)["E"]
  est_AB[i] <- coef(model)["A:B"]
  est_AE[i] <- coef(model)["A:E"]
  est_BC[i] <- coef(model)["B:C"]
}

# Calculate bias and MSE
bias_A <- mean(est_A - true_A)
mse_A <- mean((est_A - true_A)^2)

bias_B <- mean(est_B - true_B)
mse_B <- mean((est_B - true_B)^2)

bias_E <- mean(est_E - true_E)
mse_E <- mean((est_E - true_E)^2)

bias_AB <- mean(est_AB - true_AB)
mse_AB <- mean((est_AB - true_AB)^2)

bias_AE <- mean(est_AE - true_AE)
mse_AE <- mean((est_AE - true_AE)^2)

bias_BC <- mean(est_BC - true_BC)
mse_BC <- mean((est_BC - true_BC)^2)

# Print results
print(paste0("A: bias = ", bias_A, ", MSE = ", mse_A))
print(paste0("B: bias = ", bias_B, ", MSE = ", mse_B))
print(paste0("E: bias = ", bias_E, ", MSE = ", mse_E))
print(paste0("A:B: bias = ", bias_AB, ", MSE = ", mse_AB))
print(paste0("A:E: bias = ", bias_AE, ", MSE = ", mse_AE))
print(paste0("B:C: bias = ", bias_BC, ", MSE = ", mse_BC))

par(mfrow = c(2, 3))
# Histogram of the estimated A effects
hist(est_A, main = "Histogram of estimated A effects", xlab = "Estimated A effect")
abline(v = true_A, col = "red", lwd = 2)
# Histogram of the estimated B effects
hist(est_B, main = "Histogram of estimated B effects", xlab = "Estimated B effect")
abline(v = true_B, col = "red", lwd = 2)
# Histogram of the estimated E effects
hist(est_E, main = "Histogram of estimated E effects", xlab = "Estimated E effect")
abline(v = true_E, col = "red", lwd = 2)
# Histogram of the estimated A:B effects
hist(est_AB, main = "Histogram of estimated A:B effects", xlab = "Estimated A:B effect")
abline(v = true_AB, col = "red", lwd = 2)
# Histogram of the estimated A:E effects
hist(est_AE, main = "Histogram of estimated A:E effects", xlab = "Estimated A:E effect")
abline(v = true_AE, col = "red", lwd = 2)
# Histogram of the estimated B:C effects
hist(est_BC, main = "Histogram of estimated B:C effects", xlab = "Estimated B:C effect")
abline(v = true_BC, col = "red", lwd = 2)
par(mfrow = c(1, 1))

