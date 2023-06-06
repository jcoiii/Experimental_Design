rm()
library(readxl)
library(ggplot2)
#4a
fr <- read_excel(path = "Data.xlsx",
                    sheet = 2, range = "CR3:CR23")
pr <- read_excel(path = "Data.xlsx",
                    sheet = 2, range = "CS3:CS23")
sd <- read_excel(path = "Data.xlsx",
                    sheet = 2, range = "CT3:CT23")
fr <- as.numeric(unlist(fr))
fr <- factor(fr)
pr <- as.numeric(unlist(pr))
sd <- as.numeric(unlist(sd))

df <- data.frame(
  FeedRate = fr,
  ProductionRun = pr,
  StdDev = sd
)

result <- aov(StdDev ~ FeedRate, data = df)
summary(result)

#4b
boxplot(StdDev ~ FeedRate, data = df, main="Standard Deviations across Different Feed Rates", xlab="Feed Rate", ylab="Standard Deviation")

#4c
tukey <- TukeyHSD(result)
print(tukey)

library(agricolae)
lsd <- LSD.test(result, "FeedRate", group=FALSE)
print(lsd)