library(readxl)
#Read data from excel
flow_rate <- read_excel(path = "Data.xlsx",
sheet = 1, range = "K3:K15")
uniformity <- read_excel(path = "Data.xlsx",
sheet = 1, range = "L3:L15")
# Change list to numeric
flow_rate <- as.numeric(unlist(flow_rate))
uniformity <- as.numeric(unlist(uniformity))

results <- t.test(flow_rate, uniformity)
print(results)