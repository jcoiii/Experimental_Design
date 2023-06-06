rm()
library(xtable)
library(BHH2)
library(dplyr)
library(gplots)

#1a
data(corrosion.data)
mn_sd_table <- corrosion.data %>%
  group_by(heats, coating) %>%
  summarise(
    mean_resistance = mean(resistance, na.rm = TRUE),
    sd_resistance = sd(resistance, na.rm = TRUE)
  )
mn_sd_latex <- xtable(mn_sd_table, digits = 2, caption = "Results for 1(a)")

#1b
corrosion.data$heats <- as.factor(corrosion.data$heats)
corrosion.data$coating <- as.factor(corrosion.data$coating)
fit <- aov(resistance ~ heats*coating, data = corrosion.data)
summary(fit)

#1c
fit <- aov(resistance ~ heats*coating, data = corrosion.data)
par(mfrow = c(2, 2))
  plot(fit, 1) # res vs fitted val
  plot(fit, 2) # normal Q-Q
  plot(fit, 3) # Scale-Location (sqrt|standardized residuals| vs fitted val)
  plot(fit, 5) # res vs leverage
par(mfrow = c(1, 1))

#1d
tukey <- TukeyHSD(fit, "heats")
print(tukey)

#1e
means <- tapply(corrosion.data$resistance, corrosion.data$heats, mean)
sds <- tapply(corrosion.data$resistance, corrosion.data$heats, sd)
n <- tapply(corrosion.data$resistance, corrosion.data$heats, length)
ci <- qt(0.975, df = n - 1) * sds / sqrt(n)
data_summary <- data.frame(heats = names(means), mean = means, ci = ci)
print(data_summary)
#1e can use this too, this is a direct method to plot means
plotmeans(resistance ~ heats, data = corrosion.data, 
          xlab = "Heats", ylab = "Mean resistance",
          main = "Mean resistance with 95% CI by Heats")