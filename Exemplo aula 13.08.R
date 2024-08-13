install.packages("ggplot2")
library(ggplot2)

dados_BOSTON <- MASS::Boston                       # Dados sobre a influência no preço de imóveis
?MASS::Boston

############### Criar gráfico de dispersão ###############

ggplot(data =dados_BOSTON, aes(x = rm, y = medv))+ # Gráfico entre número de quartos e preço de venda
  geom_point()+                                    # Diagrama de dispersão
  theme_classic()+                                 # Opcional do professor
  xlab("Número de quartos")+                       # Eixo x
  ylab("Valor de venda (milhares de dólares)")+    # Eixo y
  geom_smooth(method = "lm", se = FALSE)           # Reta de tendencia

#### Coeficiente de correlação linear para rm e medv ####

cor(dados_BOSTON$rm, dados_BOSTON$medv)

################# Matriz de correlações #################

M <- cor(dados_BOSTON)
corrplot(M, method = "number")

################### Exemplo distancia ###################

######################## Pacote ########################
install.packages("ggplot2")
library(ggplot2)

################ Diagrama de dispersão #################
ggplot(data = distancia, aes(x = idade, y = distancia))+
  geom_point(size = 4)+                           # Size é o tamanho da bolinha
  theme_classic()+
  xlab("idade")+
  ylab("Distância(m)")+
  geom_smooth(method = "lm", se = FALSE)

########### Coeficiente de correlação linear ###########
cor(distancia$idade, distancia$distancia)

############ Ajuste do modelo de regressão #############
modelo_1 <- lm(formula =distancia ~ idade, data = distancia)

############# Mostra o resultado do modelo #############
summary(modelo_1)

###### intervalo de confiança para os parâmetros #######
confint(modelo_1)

############# Predição para um novo valor #############
novo_dado <- data.frame(idade = 24)
predict(modelo_1, novo_dado)

################# Análise de resíduos #################
plot(modelo_1)