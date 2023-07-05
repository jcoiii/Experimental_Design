library(dplyr)
library(car)

# initial data observation 16 runs (this was a 2 time replication)
data <- data.frame(
  typ = c(35, 35, 35, 35, 35, 35, 35, 35, 40, 40, 40, 40, 40, 40, 40, 40),
  K = c(18000, 18000, 18000, 18000, 26000, 26000, 26000, 26000, 18000, 18000, 18000, 18000, 26000, 26000, 26000, 26000),
  C = c(418, 418, 673, 673, 418, 418, 673, 673, 418, 418, 673, 673, 418, 418, 673, 673),
  Mass = c(41, 81, 41, 81, 41, 81, 41, 81, 41, 81, 41, 81, 41, 81, 41, 81),
  RC = c(0.655, 1.15, 0.515, 0.495, 0.66, 1.305, 0.485, 0.42, 0.885, 1.225, 0.56, 0.5, 0.855, 1.21, 0.59, 0.575)
)

# make as factors
data$typ <- as.factor(ifelse(data$typ == 35, -1, +1))
data$K <- as.factor(ifelse(data$K == 18000, -1, +1))
data$C <- as.factor(ifelse(data$C == 418, -1, +1))
data$Mass <- as.factor(ifelse(data$Mass == 41, -1, +1))

# see data summary
plot(data)
print(data)
summary(data)

# Model with main effect only
model <- aov(RC ~ ., data = data)
summary(model)
vif(model)
alias(model)
confint(model)

plot(model) #Check residuals

# Model with main effect plus second-degree interactions
model <- aov(RC ~ .^2, data = data)
summary(model)
vif(model, type = "predictor")
alias(model)
confint(model)
plot(model)

# Model with main effect plus third-degree interactions
model <- aov(RC ~ .^3, data = data)
summary(model)
vif(model, type = "predictor")
alias(model)
confint(model)
plot(model)

# Model with main effect and all possible interaction effect
model <- aov(RC ~ .^4, data = data)
summary.aov(model, scope = 1)
alias(model)
plot(model) #Check residuals
vif(model, type = "predictor") #Check multicollinearity

# Model with only the significant effects
model <- aov(RC ~ C + Mass + C:Mass, data = data)
summary(model)
alias(model)
plot(model)

# Fitting the model with only significant effects
model <- aov(RC ~ C + Mass + C:Mass, data = data)

new_data <- expand.grid(C = levels(data$C), Mass = levels(data$Mass))

# Making predictions using the model
new_data$RC_predicted <- predict(model, newdata = new_data)

# Finding the maximum predicted yield of RC
max_yield <- max(new_data$RC_predicted)
max_yield


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