library(ggplot2)
library(dplyr)
library(nortest)
library(viridisLite)
library(car)
library(lmtest)
library(carData)
library(corrplot)
library(stats)
library(MASS)
library(leaps)
library(rworldmap)
library(rworldxtra)

#Wczytanie zbioru danych
data <- read.csv("Dylag1.csv")

#Wstępna analiza danych
head(data)
summary(data)
str(data)

##Wstepna analiza modelu
model <- lm(ranking2021 ~ pop2023 + growthRate + landArea 
            + hdi + education + gni + qol
            + happiness + crime + iq
            + Europe, data = data)
summary(model)

##Czyszczenie danych

#Usunięcie wierszy z brakującymi danymi
data2 <- data[complete.cases(data$hdi, data$qol), ] 
rownames(data2) <- NULL

##Wizualizacja danych

#Model regresji
model <- lm(ranking2021 ~ pop2023 + growthRate + landArea 
            + hdi + education + gni + qol
            + happiness + crime + iq
            + Europe, data = data2)
summary(model)
IQR(model$residuals)

#liniowosc
#Macierz korelacji
selected_data <- dplyr::select(data2, Europe, pop2023, growthRate, landArea, hdi, education, gni, qol, happiness, crime, iq)
corr_matrix <- cor(selected_data)
#współczynnik kappa
eigen_values <- eigen(corr_matrix)$values
kappa <- sqrt(max(eigen_values)/min(eigen_values))
kappa
vif_data <- vif(model)
barplot(vif_data, main = "Variance Inflation Factors", 
        ylab = "VIF Value", ylim = c(0, max(vif_data) * 1.2))
#Wizualizacja korelacji między zmiennymi niezależnymi
corr_df <- as.data.frame(as.table(corr_matrix))
ggplot(corr_df, aes(Var1, Var2, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient2(low = "goldenrod2", mid = "white", high = "hotpink2", 
                       midpoint = 0, limits = c(-1, 1)) +
  labs(title = "Heatmap of Correlation Matrix", 
       x = "Variables", 
       y = "Variables") +
  theme(axis.text.x = element_text(angle = 90,hjust=1)) +
  geom_text(aes(label = round(Freq, 2)), 
            data = subset(corr_df, abs(Freq) > 0.65),
            size = 3, color = "black", fontface = "bold")

plot(model,1)
raintest(model)
resettest(model)

#Normalnosc residuów
#Wykres Q-Q residua
plot(model, 2)
qqPlot(model)
#wykres gestosci
plot(density(model$residuals),main = "Residual Density")
shapiro.test(model$residuals) #hipoteza zerowa, ze dane pochodza z populacji o rozkladzie normalnym
ks.test(model$residuals, 'pnorm') #hipotezy zerowej (brak roznic miedzy rozkladami)
ad.test(model$residuals) #hipoteza zerowa, ze dane pochodza z populacji o rozkladzie normalnym
hist(model$residuals, breaks = seq(-50,50, by = 5), main="Histogram of residuals") #sa problemy z normalnoscia, ale mimo wszystko przyjmujemy normalnosc

#Stalosc wariancji
plot(model$residuals ~ model$fitted.values)
bptest(model)
gqtest(model)
hmctest(model)
crPlots(model)
plot(model$residuals ~ log(data2$pop2023, 10))
var.test(model$residuals[which(log(data2$pop2023,10)<7.2)],model$residuals[which(log(data2$pop2023,10)>=7.2)])   

#Autokorelacja residuow
dwtest(model)
Box.test(model$residuals,type='Box-Pierce')
Box.test(model$residuals,type='Ljung-Box')
# autokorelacja 1. rzedu
m = length(model$residuals)
plot(model$residuals[2:m] ~ model$residuals[1:(m-1)])

##CZĘŚĆ 2

#Wartości odstające
plot(data2$ranking2021 ~ model$fitted.values)
abline(0,1)
leverage <- hatvalues(model)
treshold <- 2*sum(leverage)/nrow(data2)
cooks.distance(model)
#wykres dzwigni
plot(leverage, main="Leverage Plot", ylab="Leverage", xlab="Index")
text(x=1:length(leverage), y=leverage, labels=ifelse(leverage>treshold, names(leverage),""), cex=0.7)
#wykres cooka
plot(model,4)

##Badanie nowego modelu
map <- getMap()
map$color <- NA
kraje_do_pokolorowania <- data$country
map$color[map$NAME %in% kraje_do_pokolorowania] <- "hotpink2"
par(mar=c(0,0,0,0))
plot(map, col=map$color, bg="white", border="black", lwd=0.5)

model1a <- lm(ranking2021 ~ pop2023 + growthRate + landArea +
                education + gni + qol + happiness + crime + iq +
                Europe, data = data2)
summary(model1a)
RSS <- sum(model$residuals^2)
n <- nrow(data2)
AIC <- n * log(RSS/n) + 2 * length(coef(model))
AIC
RSS1a <- sum(model1a$residuals^2)
AIC1a <- n * log(RSS1a/n) + 2 * length(coef(model1a))
AIC1a
plot(model1a,1)
#sprawdzamy czy lepiej byłoby usunąć gni
model1aa <- lm(ranking2021 ~ pop2023 + growthRate + landArea +
                 education + hdi + qol + happiness + crime + iq +
                 Europe, data = data2)
summary(model1aa)
RSS1aa <- sum(model1aa$residuals^2)
AIC1aa <- n * log(RSS1aa/n) + 2 * length(coef(model1aa))
AIC1aa

selected_data <- dplyr::select(data2, Europe, pop2023, growthRate, landArea, education, gni, qol, happiness, crime, iq)
corr_matrix <- cor(selected_data)
eigen_values <- eigen(corr_matrix)$values
kappa <- sqrt(max(eigen_values)/min(eigen_values))
kappa #zmniejszyła sie kappa
vif_data <- vif(model1a) #VIF w normie ponizej 6
barplot(vif_data, main = "Variance Inflation Factors", 
        ylab = "VIF Value", ylim = c(0, max(vif_data) * 1.2))
corr_df <- as.data.frame(as.table(corr_matrix)) #pozostaje silna korelacja zmiennych happiness i qol
ggplot(corr_df, aes(Var1, Var2, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient2(low = "goldenrod2", mid = "white", high = "hotpink2", 
                       midpoint = 0, limits = c(-1, 1)) +
  labs(title = "Heatmap of Correlation Matrix", 
       x = "Variables", 
       y = "Variables") +
  theme(axis.text.x = element_text(angle = 90,hjust=1)) +
  geom_text(aes(label = round(Freq, 2)), 
            data = subset(corr_df, abs(Freq) > 0.65),
            size = 3, color = "black", fontface = "bold")
#Liniowa struktura
plot(model1a,1)
raintest(model1a)
resettest(model1a)
#Normalnosc residuów
qqPlot(model1a)
plot(density(model1a$residuals),main = "Residual Density")
#Stalosc wariancji
plot(model1a$residuals ~ model1a$fitted.values)
#Autokorelacja residuow
m = length(model1a$residuals)
plot(model1a$residuals[2:m] ~ model1a$residuals[1:(m-1)])

model1aa <- lm(ranking2021 ~ pop2023 + growthRate + landArea +
                 education + hdi + qol + crime + iq +
                 Europe, data = data2)
RSS1aa <- sum(model1aa$residuals^2)
n <- nrow(data2)
AIC1aa <- n * log(RSS1aa/n) + 2 * length(coef(model1aa))
AIC1aa
model1ab <- lm(ranking2021 ~ pop2023 + growthRate + landArea +
                 education + hdi + happiness + crime + iq +
                 Europe, data = data2)
RSS1ab <- sum(model1ab$residuals^2)
AIC1ab <- n * log(RSS1ab/n) + 2 * length(coef(model1ab))
AIC1ab #wartosc AIC nie zmienia się bardzo wiec zostawiamy obie zmienne

#wykresy rozrzutu miedzy y a x1,x2...
crPlots(model1a) #zauwazamy dużą nieliniowosc
model1b <- lm(ranking2021 ~ log(pop2023) + growthRate + landArea + iq +
                education + gni + qol + happiness + crime +
                Europe, data = data2)
crPlots(model1b)
summary(model1b)
RSS1b <- sum(model1b$residuals^2)
n <- nrow(data2)
AIC1b <- n * log(RSS1b/n) + 2 * length(coef(model1b))
AIC1b
AIC1a
aic <- step(model1b, direction = "both", k = 2)

#Korelacji między zmiennymi niezależnymi
vif_data <- vif(model1b)
barplot(vif_data, main = "Variance Inflation Factors", 
        ylab = "VIF Value", ylim = c(0, max(vif_data) * 1.2))
#Liniowa struktura
plot(model1b,1)
raintest(model1b)
resettest(model1b)
#Normalnosc residuów
qqPlot(model1b)
plot(density(model1b$residuals),main = "Residual Density")
#Stalosc wariancji
plot(model1b,1)
#Autokorelacja residuow
m = length(model1b$residuals)
plot(model1b$residuals[2:m] ~ model1b$residuals[1:(m-1)])
#dalej występuje problem nieliniowosci
#przekształcamy zmienną landArea
model1bb <- lm(ranking2021 ~ log(pop2023) + growthRate +
                 sqrt(landArea) + gni + education + qol +
                 happiness + crime + iq +
                 Europe, data = data2)
summary(model1bb)
crPlots(model1bb)
RSS1bb <- sum(model1bb$residuals^2)
AIC1bb <- n * log(RSS1bb/n) + 2 * length(coef(model1bb))
AIC1bb
AIC1b
aic <- step(model1bb, direction = "both", k = 2)
#STWIERDZAM, ZE PRZEKSZTALCENIE ZMIENNEJ landArea nie ma sensu
#przekształcsmy zmienną qol
model1c <- lm(ranking2021 ~ log(pop2023) + growthRate + landArea + iq +
                education + gni + qol + I(qol^2) + happiness + crime +
                Europe, data = data2)
crPlots(model1c)
summary(model1c)
RSS1c <- sum(model1c$residuals^2)
AIC1c <- n * log(RSS1c/n) + 2 * length(coef(model1c))
AIC1c
AIC1b
aic <- step(model1c, direction = "both", k = 2)
#AIC spadło troche
plot(model1c,1)

#Korelacji między zmiennymi niezależnymi
vif_data <- vif(model1c)
barplot(vif_data, main = "Variance Inflation Factors", 
        ylab = "VIF Value", ylim = c(0, max(vif_data) * 1.2))
#Liniowa struktura
plot(model1c,1)
raintest(model1c)
resettest(model1c)
#Normalnosc residuów
qqPlot(model1c)
plot(density(model1c$residuals),main = "Residual Density")
#Stalosc wariancji
plot(model1c,1)
#Autokorelacja residuow
m = length(model1c$residuals)
plot(model1c$residuals[2:m] ~ model1c$residuals[1:(m-1)])

#transformacja boxcox
b=boxcox(model1c)
l <- b$x[which.max(b$y)]
model1d <- lm(((ranking2021)^l-1)/l ~ log(pop2023) + growthRate + log(landArea)
              + gni + education + qol + I(qol^2)
              + happiness + crime + iq
              + Europe, data=data2)
summary(model1d)
crPlots(model1d)
RSS1d <- sum(model1d$residuals^2)
AIC1d <- n * log(RSS1d/n) + 2 * length(coef(model1d))
AIC1d
AIC1c
#Korelacji między zmiennymi niezależnymi
vif_data <- vif(model1d)
barplot(vif_data, main = "Variance Inflation Factors", 
        ylab = "VIF Value", ylim = c(0, max(vif_data) * 1.2))
#Liniowa struktura
plot(model1d,1)
raintest(model1d)
resettest(model1d)
#Normalnosc residuów
qqPlot(model1d)
plot(density(model1d$residuals),main = "Residual Density")
#Stalosc wariancji
plot(model1d,1)
bptest(model1d)
gqtest(model1d)
hmctest(model1d)
#Autokorelacja residuow
dwtest(model1d)
Box.test(model1d$residuals,type='Box-Pierce')
Box.test(model1d$residuals,type='Ljung-Box')
m = length(model1d$residuals)
plot(model1d$residuals[2:m] ~ model1d$residuals[1:(m-1)])

## Tworzymy nowy model 1e
aic <-step(model1d, drection='both', k=2)

model1e <- lm(((ranking2021)^l-1)/l ~ log(pop2023) + education + 
                qol + I(qol^2) + crime + growthRate + iq, data=data2)
summary(model1e)
crPlots(model1e)
RSS1e <- sum(model1e$residuals^2)
AIC1e <- n * log(RSS1e/n) + 2 * length(coef(model1e))
AIC1e
AIC1d
#Liniowa struktura
plot(model1e,1)
raintest(model1e)
resettest(model1e)
#Normalnosc residuów
qqPlot(model1e)
plot(density(model1e$residuals),main = "Residual Density")
shapiro.test(model1e$residuals) 
ks.test(model1e$residuals, 'pnorm') 
ad.test(model1e$residuals)
hist(model1e$residuals, breaks = seq(-4,4, by = 0.55), main="Histogram of residuals")
#Stalosc wariancji
plot(model1e,1)
bptest(model1e)
gqtest(model1e)
hmctest(model1e)
ncvTest(model1e)
#Autokorelacja residuow
dwtest(model1e)
Box.test(model1e$residuals,type='Box-Pierce')
Box.test(model1e$residuals,type='Ljung-Box')
m = length(model1e$residuals)
plot(model1e$residuals[2:m] ~ model1e$residuals[1:(m-1)])

#Wartości odstające
leverage <- hatvalues(model1e)
treshold <- 2*sum(leverage)/n
cooks.distance(model1e)
#wykres dzwigni
plot(leverage, main="Leverage Plot", ylab="Leverage", xlab="Index")
text(x=1:length(leverage), y=leverage, labels=ifelse(leverage>treshold, names(leverage),""), cex=0.7)
#wykres cooka
plot(model1e,4)


selected_data <- dplyr::select(data2, pop2023, growthRate, education, qol, crime, iq)
corr_matrix <- cor(selected_data)
eigen_values <- eigen(corr_matrix)$values
kappa <- sqrt(max(eigen_values)/min(eigen_values))
kappa
vif_data <- vif(model1e)
barplot(vif_data, main = "Variance Inflation Factors", 
        ylab = "VIF Value", ylim = c(0, max(vif_data) * 1.2))
corr_df <- as.data.frame(as.table(corr_matrix))
ggplot(corr_df, aes(Var1, Var2, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient2(low = "goldenrod2", mid = "white", high = "hotpink2", 
                       midpoint = 0, limits = c(-1, 1)) +
  labs(title = "Heatmap of Correlation Matrix", 
       x = "Variables", 
       y = "Variables") +
  theme(axis.text.x = element_text(angle = 90,hjust=1)) +
  geom_text(aes(label = round(Freq, 2)), 
            data = subset(corr_df, abs(Freq) > 0.65),
            size = 3, color = "black", fontface = "bold")
summary(data2$qol)
plot(data2$qol + I(data2$qol^2))

#walidacja krzyżowa
X <- model.matrix(model)
cv_errors_m <- rep(0, nrow(data2))
for (i in 1:nrow(data2)) {
  fit <- lm.fit(X[-i, ], data2$ranking2021[-i])
  cv_errors_m[i] <- (data2$ranking2021[i] - fit$coef %*% X[i, ]) ^ 2
}
mse <- mean(cv_errors_m)

X1e <- model.matrix(model1e) #  Macierz projektowa
cv_errors_m1e <- rep(0, n)
for (i in 1:n) {
  fit <- lm.fit(X1e[-i, ], data2$ranking2021[-i]) #leave-one-out cv
  cv_errors_m1e[i] <- (data2$ranking2021[i] - fit$coef %*% X1e[i, ]) ^ 2  #błąd predykcji
}
mse1e <- mean(cv_errors_m1e)
sqrt(mse)
sqrt(mse1e)
