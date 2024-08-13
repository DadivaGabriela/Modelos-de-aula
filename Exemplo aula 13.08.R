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

