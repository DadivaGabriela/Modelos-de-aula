# Pacotes
install.packages("kmed")
library(kmed)
install.packages("dplyr")
library(dplyr)
install.packages("sjPlot")
library(sjplot)
install.packages("ggplot2")
library(ggplot2)

# Importando dados:
dados <- heart

# 1 - preparação dos dados:
# Selecionando colunas #
dados_novos <- dados %>% select(age,sex,cp,thalach,class)
dados_novos <- dados_novos %>% rename(idade = age, sexo = sex, dor_no_peito = cp, freq_maxima = thalach, doença_cardiaca = class)
# Renomeando valores paraa a variavel sexo #
dados_novos$sexo <- factor(dados_novos$sexo, levels = c(FALSE, TRUE), labels = c("feminino","masculino"))
# Renomear valores para dor no peito #
dados_novos$dor_no_peito <- factor(dados_novos$dor_no_peito,levels = 1:4,labels = c("angina típica","angina atípica","dor não_anginal","assintomático"))
# Cofificando para doente ou não doente #
dados_novos$doença_cardiaca <- ifelse(dados_novos$doença_cardiaca == 0, 0, 1)
# Passando para fator #
dados_novos$doença_cardiaca <- factor(dados_novos$doença_cardiaca, levels = c(0,1), labels = c('sem doença','com doença'))


# 2 - Modelo 1 - Regressão logíca simples - variável independente quantitativa:
ggplot(data = dados_novos, aes(y = idade, fill = doença_cardiaca)) +
  geom_boxplot()
dados_novos %>% group_by(doença_cardiaca) %>% summarise(media = mean(idade))
# Modelo para associação entre doença cardíaca e idade #
modelo_1 <- glm(doença_cardiaca ~ idade, data = dados_novos, family = "binomial")
summary(modelo_1)
# Quando o coeficiente é igual a zero, x e y são independentes.
# Quando o coeficiente é > 0, a probabilidade de y = 1 (doente) aumenta com x.
# Quando o coeficiente é < 0, a probabilidade de y = 1 (doente) diminui com x.

# Quantificando a relação:
exp(coef(modelo_1)["idade"])
# Um ano extra de vida aumenta a chance de desenvolver uma doença cardíaca por um fator de 1,05.

# Análise do intercepto:
exp(coef(modelo_1)[1])/(1 + exp(coef(modelo_1)[1]))
# Uma pessoa de 0 ano tem uma chance de desenvolver doença cardíaca de 0,04.

# Predizendo a probabilidade de ter doença:
novo_paciente <- data.frame(idade = 30)
predict(modelo_1, novo_paciente, type = "response")
# Uma pessoa de 30 anos tem chance de 18,79% de ter doença cardíaca.

plot_model(modelo_1, type = "pred", terms = "idade")


# 3 - modelo 2 - Regressão logística simples - variavel independente qualitativa:
# analise grafica #
ggplot(data = dados_novos, aes(x = sexo, fill = doença_cardiaca)) +
  geom_bar()
# teste qui_quadrado para independencia #
chisq.test(table(dados_novos$doença_cardiaca, dados_novos$sexo))
# Valor-p < 0,05, podemos rejeitar a hipotese de independencia das variaveis e supor que elas sejam associadas, com um nível de confiança de 95%

modelo_2 <- glm(doença_cardiaca ~ sexo, data = dados_novos, family = "binomial")
summary(modelo_2)

exp(coef(modelo_2)["sexomasculino"])
# Uma pessoa do sexo masculino tem um fato multiplicado por 3,754 de desenvolver doença cardíaca.

plot_model(modelo_2, type = "pred", terms = "sexo")


# 4 - Modelo 3 - várias variáveis:
modelo_3 <- glm(doença_cardiaca ~., data = dados_novos, family = "binomial")
summary(modelo_3)