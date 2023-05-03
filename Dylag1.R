library(ggplot2)
library(dplyr)
library(nortest)
library(viridisLite)
library(car)
library(lmtest)
library(carData)
library(corrplot)
library(stats)

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
corr_matrix <- cor(select(data2,'Europe','pop2023','growthRate','landArea','hdi','education','gni','qol','happiness','crime','iq'))
det(corr_matrix)

high_correlations <- data.frame(v1 = colnames(corr_matrix)[which(abs(corr_matrix) > 0.8 & corr_matrix != 1, arr.ind = TRUE)[,1]],
                                v2 = colnames(corr_matrix)[which(abs(corr_matrix) > 0.8 & corr_matrix != 1, arr.ind = TRUE)[,2]],
                                correlation = corr_matrix[which(abs(corr_matrix) > 0.8 & corr_matrix != 1, arr.ind = TRUE)])
high_correlations
ggplot(data2, aes(x = gni, y = hdi)) + 
  geom_point() +
  labs(x = "gni", y = "hdi")

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
  scale_fill_gradient2(low = "goldenrod2", mid = "white", high = "green4", 
                       midpoint = 0, limits = c(-1, 1)) +
  labs(title = "Heatmap of Correlation Matrix", 
       x = "Variables", 
       y = "Variables") +
  theme(axis.text.x = element_text(angle = 90,hjust=1)) +
  geom_text(aes(label = round(Freq, 2)), 
            data = subset(corr_df, abs(Freq) > 0.5),
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
bptest(model)
crPlots(model)
plot(model$residuals ~ log(data2$pop2023))
var.test(model$residuals[which(log(data2$pop2023)<16)],model$residuals[which(log(data2$pop2023)>16)])   

##Autokorelacja residuow
dwtest(model)
Box.test(model$residuals,type='Box-Pierce')
Box.test(model$residuals,type='Ljung-Box')
# autokorelacja 1. rzedu
n = length(model$residuals)
plot(model$residuals[2:n] ~ model$residuals[1:(n-1)])
