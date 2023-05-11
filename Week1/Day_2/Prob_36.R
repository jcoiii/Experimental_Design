library(readxl)
#Read data from excel
karlshure <- read_excel(path = "Data.xlsx",
sheet = 1, range = "AC3:AC12")
lehigh <- read_excel(path = "Data.xlsx",
sheet = 1, range = "AD3:AD12")
# Change list to numeric
karlshure <- as.numeric(unlist(karlshure))
lehigh <- as.numeric(unlist(lehigh))