data <- read.csv("C:/Users/Happy/Documents/GitHub/Econometrics/output.csv")
head(data)
summary(data)
str(data)

model <- lm(rank ~ year + total + gold_medals + silver_medals + bronze_medals + honourable_mentions, data=data)
summary(model)
plot(data$year, data$rank, main="Year vs. Rank", xlab="Year", ylab="Rank")
