rm()
library(MPV)
#3a
model1 <- lm(y ~ ., data = table.b13)
model2 <- lm(y ~ x1 + x5 + x6, data = table.b13)

par(mfrow=c(2,2))
plot(model1)
plot(model2)
#3b
library(xtable)
cooksd <- cooks.distance(model1)
cooks_table <- data.frame(Row = 1:length(cooksd), Cooks_Distance = cooksd)
cooks_latex <- xtable(cooks_table, digits = 5, caption = "Cook's distances")
influential_obs <- which(cooksd > 0.5)

hats <- hatvalues(model1)
hats_table <- data.frame(Row = 1:length(hats), hatvalue = hats)
hats_latex <- xtable(hats_table, digits = 5, caption = "hat values")
high_leverage_obs <- which(hats > (2*(length(model1$coefficients)/(length(table.b13$y)))))

library(car)
library(boot)
vif <- vif(model1)
high_vif_vars <- names(which(vif > 10))

#3c
#bootstrap function
boot_func <- function(data, index) {
  model <- lm(y ~ ., data = data[index,])
  return(coef(model))
}
set.seed(100) 
boot_results <- boot(data = table.b13, statistic = boot_func, R = 2000)
confint_lm <- confint(model1, level = 0.95)
confint_lm_table <- data.frame(Row = 1:length(confint_lm), confint_lm_val = confint_lm)
confint_lm_latex <- xtable(confint_lm_table, digits = 5, caption = "Linear Regression")
confint_boot <- t(sapply(1:ncol(boot_results$t), 
                         function(i) quantile(boot_results$t[,i], 
                                              c(0.025, 0.975))))
confint_boot_table <- data.frame(Row = 1:length(confint_boot), confint_boot_val = confint_boot)
confint_boot_latex <- xtable(confint_boot_table, digits = 5, caption = "Bootstrap")

#print(confint_lm_latex, caption.placement = "top", include.rownames = FALSE, booktabs = TRUE)

#3d
f_test <- anova(model2, model1)
print(f_test)

#3e
model2
plot(model2)

boot_func2 <- function(data, index) {
  model2 <- lm(y ~ x1 + x5 + x6, data = data[index,])
  return(coef(model2))
}
boot_results2 <- boot(data = table.b13, statistic = boot_func2, R = 2000)
confint_lm_2 <- confint(model2, level = 0.95)
confint_lm_table_2 <- data.frame(Row = 1:length(confint_lm_2), confint_lm_val_2 = confint_lm_2)
confint_lm_latex_2 <- xtable(confint_lm_table_2, digits = 5, caption = "Linear Regression")
confint_boot2 <- t(sapply(1:ncol(boot_results2$t), 
                         function(i) quantile(boot_results2$t[,i], 
                                              c(0.025, 0.975))))
confint_boot_table2 <- data.frame(Row = 1:length(confint_boot2), confint_boot_val2 = confint_boot2)
confint_boot_latex2 <- xtable(confint_boot_table2, digits = 5, caption = "Bootstrap")

