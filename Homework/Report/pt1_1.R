rm()
library(readxl)
library(FrF2)

# Load the entire sheet
data <- read_excel("dataset.xlsx")
model <- lm(RC ~ typ + K + C + Mass, data = data)
anova_result <- anova(model)
alias_result <- alias(model)

# Print the design matrix
design_matrix <- FrF2(nruns = 16, nfactors = 4)
print(design_matrix)

# Assess the resolution of the design
k <- ncol(design_matrix)  # Number of factors in the design
p <- k - 1  # Assuming all factors are independent (excluding intercept)
resolution <- 2^(k - p)
print(resolution)

# Evaluate aberration in the design
interactions <- combn(names(design_matrix), 2)  # Get all possible pairs of factor names
aberration <- NULL

# Check for aberration in each interaction
for (i in 1:ncol(interactions)) {
  factor1 <- interactions[1, i]
  factor2 <- interactions[2, i]
  interaction_term <- paste(factor1, factor2, sep = ":")
  if (!interaction_term %in% colnames(design_matrix)) {
    aberration <- c(aberration, interaction_term)
  }
}

# Print aberrations, if any
if (length(aberration) == 0) {
  print("No aberrations found in the design.")
} else {
  print("Aberrations found in the design:")
  print(aberration)
}