# QUESTÃO 1
dados <- data.frame(volume = c(656,692,588,799,766,800,693,602,737,921,923,945,816,584,642,970), peso = c(630,745,690,890,825,960,835,570,705,955,990,725,840,640,740,945))
install.packages("ggplot2")
library(ggplot2)

# I) Modelo de regressão linear
# y = peso, x = volume
Modelo_1 <- lm(formula = peso ~ volume, data = dados)
summary(Modelo_1)

# II) Gráfico de dispersão
ggplot(data = dados, aes(x = volume, y= peso))+
  geom_point(size = 4) +
  theme_classic() +
  xlab("volume cm³") +
  ylab("peso(g)") +
  geom_smooth(method = "lm", se = FALSE)

# III) Ajuste do modelo
summary(Modelo_1) # R quadrado é de 58% da variação do peso
plot(dados$volume, modelo$residuals)
plot(Modelo_1)

# Iv) Intervalo de confiança
confint(Modelo_1)

# V) Tabela de intervalo de confiança
novos_dados <- data.frame(volume = c(600,700,800,900,1000))
predict(Modelo_1, novos_dados, interval= "predict")

# VI) 
Modelo_1 <- lm(formula = peso ~ volume + 0, data = dados)
summary(Modelo_1)


