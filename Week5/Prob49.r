rm()
library(readxl)
library(ggplot2)
library(DoE.base)
library(FrF2)

path_file <- "/home/jonathan/Documents/ED/Week2/Data.xlsx"
pres <- read_excel(path = path_file, sheet = 7, range = "ML3:ML19")
temp <- read_excel(path = path_file, sheet = 7, range = "MM3:MM19")
mois <- read_excel(path = path_file, sheet = 7, range = "MN3:MN19")
flow <- read_excel(path = path_file, sheet = 7, range = "MO3:MO19")
part <- read_excel(path = path_file, sheet = 7, range = "MP3:MP19")
yiel <- read_excel(path = path_file, sheet = 7, range = "MQ3:MQ19")
pres <- as.numeric(unlist(pres))
temp <- as.numeric(unlist(temp))
mois <- as.numeric(unlist(mois))
flow <- as.numeric(unlist(flow))
part <- as.numeric(unlist(part))
yiel <- as.numeric(unlist(yiel))
#a Identify the defining relation and the alias relationships.
experiment_df <- data.frame(
    pres,
    temp,
    mois,
    flow,
    part,
    yiel
)

factors <- c("Pressure", "Temperature", "Moisture", "Flow", "ParticleSize")
levels <- list(
  Pressure = c(415, 550),
  Temperature = c(25, 95),
  Moisture = c(5, 15),
  Flow = c(40, 60),
  ParticleSize = c(1.28, 4.05)
)

model <- lm(yiel ~ ., data = experiment_df)
summary(model)
alias(model)

#b Estimate the factor effects and
# use a normal probability plot to tentatively 
# identify the important factors.
factor_effects <- effects(model)
print(factor_effects)
qqnorm(model$residuals, main = "Normal Probability Plot of Residuals")
qqline(model$residuals)

#c Perform an appropriate statistical analysis to test the
# hypotheses that the factors identified in part (b) have a
# significant effect on the yield of peanut oil.
anova_result <- anova(model)
print(anova_result)

#d Fit a model that could be used to predict peanut oil
# yield in terms of the factors that you have identified as
# important
reduced_model <- lm(yiel ~ pres + part, data = experiment_df)
summary(reduced_model)

#e Analyze the residuals from this experiment and com-
# ment on model adequacy.
residuals_red <- residuals(reduced_model)

qqnorm(residuals_red, main = "Normal Q-Q Plot of reduced model")
qqline(residuals_red)