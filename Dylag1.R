data <- read.csv("Zeszyt2.csv")
head(data)
summary(data)
str(data)

model <- lm(ranking.22 ~ pop2023 + growthRate + regionNo + landAreaKm
                    + Tertiary + HDI + Mean_years_of_schooling + GNI
                    + Happiness2021 + Happiness2020 + numbeoQoL, data = data)
summary(model)


#Wykres Q-Q residua
plot(model, 2)


