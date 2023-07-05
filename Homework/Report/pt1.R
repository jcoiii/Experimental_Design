rm()
library(readxl)

# Load the entire sheet
data <- read_excel("dataset.xlsx")
model <- lm(rc ~ typ + K + C + mass, data = data)
anova_result <- anova(model)
alias(model)


# Define the factors and levels
typ <- c(35000, 40000)
k <- c(18000, 26000)
c <- c(418, 673)
m <- c(41, 81)

# Create the design matrix
design_matrix <- expand.grid(typ = typ, k = k, c = c, m = m)

# Print the design matrix
print(design_matrix)

# Assuming your design matrix is stored in a variable named 'design_matrix'
# Assess the resolution of the design
k <- ncol(design_matrix)  # Number of factors in the design
p <- k - 1  # Assuming all factors are independent (excluding intercept)

# Calculate the resolution
resolution <- 2^(k - p)

# Print the resolution
print(resolution)

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