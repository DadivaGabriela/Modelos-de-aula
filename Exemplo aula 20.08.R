##### REGRESSÃO LINEAR MULTIPLA #####
install.packages("corrplot")
library(corrplot)

dados_carros <- mtcars
?mtcars

# Matriz de correlações
M <- cor(dados_carros)
corrplot(M, method = "number")

# Modelo
modelo_carros <- lm(formula = mpg ~., data = dados_carros)
summary(modelo_carros)
# y = 12,30337 - 0,11144cyl + 0,01334disp -0,02148....


