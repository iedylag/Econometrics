data <- read.csv("Zeszyt2.csv")
head(data)
summary(data)
str(data)

model <- lm(ranking.22 ~ pop2023 + growthRate + regionNO + landAreaKm
                    + Tertiary + HDI + Mean_years_of_schooling + GNI
                    + Happiness2021 + Happiness2020 + numbeoQoL, data = data)
summary(model)

#wykres reszt standardowych vs wartości przewidywane
plot(fitted(model), rstandard(model), main = "Residuals vs Fitted", 
     xlab = "Fitted values", ylab = "Standardized residuals")
abline(h = 0, col = "red")

#histogram reszt standardowych
hist(rstandard(model), main = "Histogram of Standardized Residuals", 
     xlab = "Standardized residuals", ylab = "Frequency")

plot(log(data$pop2023), data$ranking.22, main = "Ranking vs Population in 2023",
     xlab = "Population in 2023", ylab = "Ranking")
abline(model$coefficients[1], model$coefficients[2], col = "red")

plot(data$Happiness2021, data$ranking.22, main = "Ranking vs Happiness in 2021",
     xlab = "Happiness in 2021", ylab = "Ranking")
abline(lm(data$ranking.22 ~ data$Happiness2021), col = "red")

plot(data$Happiness2020, data$ranking.22, main = "Rank 2021 vs. Happiness 2020",
     xlab = "Ranking", ylab = "Happiness in 2020")
abline(lm(data$ranking.21 ~ data$Happiness2020), col = "red")



model22 <- lm(ranking.22 ~ Happiness2021 + Happiness2020, data = data)
summary(model22)
model21 <- lm(ranking.21 ~ Happiness2021 + Happiness2020, data = data)
summary(model21)
model20 <- lm(ranking.20 ~ Happiness2021 + Happiness2020, data = data)
summary(model20)

model2 <- lm(ranking.22 ~ pop2023 + growthRate + landAreaKm
                        + Education.Foreignborn + Education.Nativeborn
                        + Migration + HDI + Mean_years_of_schooling + GNI, data = data)
summary(model2)

subset(data, country == "Poland")
subset(data, country == "Poland", select = c(region, ranking.22))
subset(data, ranking.22 <= 10, select = c(country, ranking.22))

data[order(data$region, data$ranking.22), ]
