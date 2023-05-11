library(readxl)
#Read data from excel
bhp <- read_excel(path = "/home/jonathan/Documents/ED/Week2/Data.xlsx",
sheet = 9, range = "O3:O15")
rpm <- read_excel(path = "/home/jonathan/Documents/ED/Week2/Data.xlsx",
sheet = 9, range = "P3:P15")
ron <- read_excel(path = "/home/jonathan/Documents/ED/Week2/Data.xlsx",
sheet = 9, range = "Q3:Q15")
cmp <- read_excel(path = "/home/jonathan/Documents/ED/Week2/Data.xlsx",
sheet = 9, range = "R3:R15")
# Change list to numeric
bhp_1 <- as.numeric(unlist(bhp))
rpm_1 <- as.numeric(unlist(rpm))
ron_1 <- as.numeric(unlist(ron))
cmp_1 <- as.numeric(unlist(cmp))
#Fit Data
data <- data.frame(x1 = rpm_1, x2 = ron_1, x3 = cmp_1, y = bhp_1)
model <- lm(y ~ x1 + x2 + x3, data = data)
summary(model)