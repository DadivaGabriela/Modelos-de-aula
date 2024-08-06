#Importação dos dados

install.packages("readr") #instala pacote
library(readr)            #carrega pacote

# carrega os dados
dados <- read.csv("C:/Users/20231enpro0453/Downloads/olimpiadas.csv")

############################################
# Tabela de frequência
############################################
# Tabela simples

table(dados$Sex) # Valores absolutos
table(dados$Sex)/nrow(dados)

# Tabela de dupla entrada

table(dados$Sex, dados$NOC)

############################################
# Gráficos
############################################

install.packages("esquisse")
library(esquisse)


library(ggplot2)

ggplot(dados) +
 aes(x = Sex) +
 geom_bar(fill = "#112446") +
 theme_minimal()


############################################
# Gráficos
############################################

install.packages("dplyr")
library(dplyr)

dados_brasil <- dados %>% filter(NOC == "BRA")

############################################
# Estatisticas descritivas
############################################

min(dados$Age, na.rm = TRUE) # minimio
max(dados$Age, na.rm = TRUE) # máximo
mean(dados$Age, na.rm = TRUE)# mádia
sd(dados$Age, na.rm = TRUE)# desvio padrão
esquisser(dados)

dados_F <- dados %>% filter(Sex == "F")
mean(dados_F$weight, na.rm = TRUE)
dados_F <- dados %>% filter(Sex == "M")
mean(dados_M$weight, na.rm = TRUE)

dados %>% group_by(Sex) %>% summarise(media = mean(Weight, na.rm = TRUE))



