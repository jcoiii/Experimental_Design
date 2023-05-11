library(readxl)
#Read data from excel
caliper_1 <- read_excel(path = "Data.xlsx",
sheet = 1, range = "V3:V15")
caliper_2 <- read_excel(path = "Data.xlsx",
sheet = 1, range = "W3:W15")
# Change list to numeric
caliper_1 <- as.numeric(unlist(caliper_1))
caliper_2 <- as.numeric(unlist(caliper_2))

results <- t.test(caliper_1, caliper_2, alpha = 0.05)
print(results)
#(a) There are no significant difference between the two means,
# even thought there are difference in values of them.