#**********************************************************
# ESTUDO DE CASO - MODELO PREDITIVO DE CHURN
#**********************************************************

#**********************************************************
#Mapear diret�rio de trabalho
getwd()
#Aten��o: Alterar Diret�rio
setwd("C:\\Users\\Camila Melo\\Documents\\Estudos\\LABData Fia\\aula8")
#**********************************************************

#*
# **********************************************************
# Leitura da base de telefonia
telefonia <- read.table("Telefonia.txt", header = TRUE, sep = "\t", dec = ".")

# **********************************************************
# Verificar estrutura e informa��es sobre a base de telefonia
names(telefonia)
nrow(telefonia)

# **********************************************************
# (a) Fa�a a an�lise explorat�ria univariada dos dados, avalie a consist�ncia das 
# informa��es e missing values.

# Medidas resumo das vari�vel, menos a primeira coluna
summary(telefonia[,-1])

# Tratamento da vari�vel 'Idade'
telefonia$Idade <- ifelse(is.na(telefonia$Idade), 999, telefonia$Idade)

#Idades inconsistentes
idade_17 <- ifelse(telefonia$Idade<18, 1, 0)
sum(idade_17)
idade_100 <- ifelse(telefonia$Idade>100 & telefonia$Idade!= 999, 1, 0)
sum(idade_100)

#Atribuindo idades incosistentes para missing
telefonia$Idade <- ifelse(telefonia$Idade<18|telefonia$Idade>100, 999, telefonia$Idade)

summary(telefonia$Idade)

# Tratamento da vari�vel 'Minutos realizados'
telefonia$Minutos_realizados_T0 <- ifelse(is.na(telefonia$Minutos_realizados_T0), 0, telefonia$Minutos_realizados_T0)
summary(telefonia$Minutos_realizados_T0)

# Tabela de frequ�ncia para a vari�vel 'resposta'
(resposta_a <- table(telefonia$resposta))
(resposta_p <- prop.table(resposta_a) * 100) # 0,8% de cancelamento volunt�rio

# Qtde de renten��o
# Apesar de ser uma vari�vel quantitativa, por se tratar de uma vari�vel quantitativa discreta tamb�m � interessante tamb�m avaliar a frequ�ncia de cada valor
(freq_Retencao <- table(telefonia$Qtd_retencao_6meses))
(pct_Retencao <- prop.table(table(telefonia$Qtd_retencao_6meses))) # utilizando a fun��o prop.table, temos como output a tabela em percentual
round(pct_Retencao * 100, 2) #Percentual de cada categoria

# Qtde de produtos
# Apesar de ser uma vari�vel quantitativa, por se tratar de uma vari�vel quantitativa discreta tamb�m � interessante tamb�m avaliar a frequ�ncia de cada valor
(freq_Prod <- table(telefonia$Qtd_prod))
pct_Prod <- prop.table(table(telefonia$Qtd_prod)) # utilizando a fun��o prop.table, temos como output a tabela em percentual
round(pct_Prod * 100, 2) # percentual de cada categoria

# **********************************************************
# (b) Fa�a a an�lise descritiva bivariada covari�vel x resposta e identifique as 
# covari�veis que tem mais rela��o com a resposta.

# Pacote com a fun��o 'quantcut'
library(gtools) 
# Recomendado 4 a 5 mais comum usada em a�oes de mercado

telefonia$Idade_q_aux <- quantcut(telefonia$Idade, 4)
telefonia$Idade_q <- ifelse(telefonia$Idade == 999, "Missing", telefonia$Idade_q_aux)
telefonia$Minutos_realizados_T0_q <- quantcut(telefonia$Minutos_realizados_T0, 4)
telefonia$Tempo_safra_q <- quantcut(telefonia$Tempo_safra, 4)
telefonia$Tempo_casa_q <- quantcut(telefonia$Tempo_casa, 4)
telefonia$Qtd_retencao_6meses_q <- quantcut(telefonia$Qtd_retencao_6meses, 4)
telefonia$Qtd_prod_q <- quantcut(telefonia$Qtd_prod, 4)

# Tabela bidimensional: covari�vel x resposta
Idade_table_q <- table(telefonia$Idade_q, telefonia$resposta)
Minutos_table_q <- table(telefonia$Minutos_realizados_T0_q, telefonia$resposta)
Tempo_safra_table_q <- table(telefonia$Tempo_safra_q, telefonia$resposta)
Tempo_casa_table_q <- table(telefonia$Tempo_casa_q, telefonia$resposta)
Qtd_retencao_table_q <- table(telefonia$Qtd_retencao_6meses_q, telefonia$resposta)
Qtd_prod_table_q <- table(telefonia$Qtd_prod_q, telefonia$resposta)

# Multiplicando por 100 para virar porcentagem e arredondamento para 2 casas decimais
round(prop.table(Idade_table_q, 1) * 100, 2) # par�metro 1 dentro de prop.table indica que � a propor��o da linha
table(telefonia$Idade_q,telefonia$Idade_q_aux)#Ver as categorias
round(prop.table(Minutos_table_q, 1) * 100, 2) 
round(prop.table(Tempo_safra_table_q, 1) * 100, 2)
round(prop.table(Tempo_casa_table_q, 1) * 100, 2)
round(prop.table(Qtd_retencao_table_q, 1) * 100, 2)
round(prop.table(Qtd_prod_table_q, 1) * 100, 2)

# **********************************************************
# (c) Fa�a a an�lise da associa��o entre as covari�veis categorizadas.

# Biblioteca para o c�lculo da estat�stica de Cramers'V
library(lsr)

# Idade com as demais covari�veis
cramersV(table(telefonia$Idade_q, telefonia$Minutos_realizados_T0_q))
cramersV(table(telefonia$Idade_q, telefonia$Tempo_safra_q))
cramersV(table(telefonia$Idade_q, telefonia$Tempo_casa_q))
cramersV(table(telefonia$Idade_q, telefonia$Qtd_retencao_6meses_q))
cramersV(table(telefonia$Idade_q, telefonia$Qtd_prod_q))

# Minutos realizados com as demais covari�veis
cramersV(table(telefonia$Minutos_realizados_T0_q, telefonia$Tempo_safra_q))
cramersV(table(telefonia$Minutos_realizados_T0_q, telefonia$Tempo_casa_q))
cramersV(table(telefonia$Minutos_realizados_T0_q, telefonia$Qtd_retencao_6meses_q))
cramersV(table(telefonia$Minutos_realizados_T0_q, telefonia$Qtd_prod_q))

# Tempo_safra com as demais covari�veis
cramersV(table(telefonia$Tempo_safra_q, telefonia$Tempo_casa_q))
cramersV(table(telefonia$Tempo_safra_q, telefonia$Qtd_retencao_6meses_q))
cramersV(table(telefonia$Tempo_safra_q, telefonia$Qtd_prod_q))

# Tempo_casa com as demais covari�veis
cramersV(table(telefonia$Tempo_casa_q, telefonia$Qtd_retencao_6meses_q))
cramersV(table(telefonia$Tempo_casa_q, telefonia$Qtd_prod_q))

# Qtde de reten��o com Qtd de produtos
cramersV(table(telefonia$Qtd_retencao_6meses_q, telefonia$Qtd_prod_q))

# **********************************************************
# (d) Rode a regress�o log�stica considerando as covari�veis categorizadas. 
# Identifique quais vari�veis foram selecionadas pelo modelo, interprete-as, 
# e avalie o desempenho do modelo pela Tabela de Classifica��o.

# Modelo completo
modelo_full <- glm(resposta ~ Idade_q +
                     Minutos_realizados_T0_q +
                     Tempo_safra_q +
                     Tempo_casa_q +
                     Qtd_retencao_6meses_q +
                     Qtd_prod_q,
                   family = binomial(link = "logit"),
                   data = telefonia)
summary(modelo_full)

# Para o modelo log�stico, com a fun��o 'predict', tendo como par�metro type = 'response' conseguimos obter as probabilidades do modelo para a classifica��o '1'
telefonia$reg_log_p1 <- predict(modelo_full, newdata = telefonia, type = "response")
summary(telefonia$reg_log_p1)

# Cria vari�vel resposta predita com base na probabilidade predita pelo modelo
# AENCAO A COMPARACAO COM A PROBABILIDADE DO EVENTO DE E=INTERESSE MUDA
telefonia$resp_bin1 <- as.factor(ifelse(telefonia$reg_log_p1 >= 0.008685467, 1, 0)) # transforma a probabilidade em vari�vel bin�ria

# Mostra a tabela de desempenho: Predito x Resposta observada
(tabela_desempenho <- table(telefonia$resposta, telefonia$resp_bin1))

# Calcula as medidas de desempenho: Sensibilidade, Especificidade e Acur�cia
(sensibilidade <- tabela_desempenho[2, 2] / sum(tabela_desempenho[2, ]))
(especificidade <- tabela_desempenho[1, 1] / sum(tabela_desempenho[1, ]))
(n <- nrow(telefonia))
(acuracia <- sum(tabela_desempenho[1, 1] + tabela_desempenho[2, 2]) / n)
(ks <- abs(sensibilidade - (1 - especificidade)))

# Modelo reduzido: sem tempo safra
modelo_red1 <- glm(resposta ~ Idade_q + Minutos_realizados_T0_q +
                     Tempo_casa_q +
                     Qtd_retencao_6meses_q +
                     Qtd_prod_q,
                   family = binomial(link = "logit"),
                   data = telefonia)
summary(modelo_red1)

telefonia$reg_log_p1 <- predict(modelo_red1, newdata = telefonia, type = "response")
summary(telefonia$reg_log_p1)

# Transforma a probabilidade em vari�vel bin�ria
telefonia$resp_bin1 <- as.factor(ifelse(telefonia$reg_log_p1 >= 0.008685467, 1, 0))

(tabela_desempenho <- table(telefonia$resposta, telefonia$resp_bin1))
(sensibilidade <- tabela_desempenho[2, 2] / sum(tabela_desempenho[2, ]))
(especificidade <- tabela_desempenho[1, 1] / sum(tabela_desempenho[1, ]))
(n <- nrow(telefonia))
(acuracia <- sum(tabela_desempenho[1, 1] + tabela_desempenho[2, 2]) / n)

# Modelo reduzido: sem tempo casa 
modelo_red2 <- glm(resposta ~ Idade_q +
                     Minutos_realizados_T0_q +
                     Tempo_safra_q +
                     Qtd_retencao_6meses_q +
                     Qtd_prod_q,
                   family = binomial(link = "logit"),
                   data = telefonia)
summary(modelo_red2)

telefonia$reg_log_p1 <- predict(modelo_red2, newdata = telefonia, type = "response")
summary(telefonia$reg_log_p1)
telefonia$resp_bin1 <- as.factor(ifelse(telefonia$reg_log_p1 >= 0.008685467, 1, 0)) # transforma a probabilidade em vari�vel bin�ria

(tabela_desempenho <- table(telefonia$resposta, telefonia$resp_bin1))
(sensibilidade <- tabela_desempenho[2, 2] / sum(tabela_desempenho[2, ]))
(especificidade <- tabela_desempenho[1, 1] / sum(tabela_desempenho[1, ]))
(n <- nrow(telefonia))
(acuracia <- sum(tabela_desempenho[1, 1] + tabela_desempenho[2, 2]) / n)

# Modelo reduzido: sem tempo casa e Idade (tem categoria missing)
modelo_red3 <- glm(resposta ~ Minutos_realizados_T0_q +
                     Tempo_safra_q +
                     Qtd_retencao_6meses_q +
                     Qtd_prod_q,
                   family = binomial(link = "logit"),
                   data = telefonia)
summary(modelo_red3)

telefonia$reg_log_p1 <- predict(modelo_red3, newdata = telefonia, type = "response")
summary(telefonia$reg_log_p1)
telefonia$resp_bin1 <- as.factor(ifelse(telefonia$reg_log_p1 >= 0.008685467, 1, 0)) # transforma a probabilidade em vari�vel bin�ria

(tabela_desempenho <- table(telefonia$resposta, telefonia$resp_bin1 ))
(sensibilidade <- tabela_desempenho[2, 2] / sum(tabela_desempenho[2, ]))
(especificidade <- tabela_desempenho[1, 1] / sum(tabela_desempenho[1, ]))
(n <- nrow(telefonia))
(acuracia <- sum(tabela_desempenho[1, 1] + tabela_desempenho[2, 2]) / n)

###############################################################
################# Carlos indica��o de analise #################
###############################################################

telefonia$Cancelou <- ifelse(telefonia$Tempo_casa-telefonia$Tempo_safra > 50, 1, 0)

modelo_aula <- glm(resposta ~ Idade_q +
                     Minutos_realizados_T0_q +
                     Tempo_casa_q +
                     Qtd_retencao_6meses_q +
                     Qtd_prod_q + Cancelou,
                   family = binomial(link = "logit"),
                   data = telefonia)
summary(modelo_aula)

telefonia$reg_log_aula <- predict(modelo_aula, newdata = telefonia, type = "response")
summary(telefonia$reg_log_aula)
telefonia$resp_aula <- as.factor(ifelse(telefonia$reg_log_aula >= 0.008685467, 1, 0)) # transforma a probabilidade em vari�vel bin�ria

(tabela_desempenho <- table(telefonia$resposta, telefonia$resp_aula))
(sensibilidade <- tabela_desempenho[2, 2] / sum(tabela_desempenho[2, ]))
(especificidade <- tabela_desempenho[1, 1] / sum(tabela_desempenho[1, ]))
(n <- nrow(telefonia))
(acuracia <- sum(tabela_desempenho[1, 1] + tabela_desempenho[2, 2]) / n)

# **********************************************************
# (e) Rode �rvore de decis�o usando o m�todo CHAID com 3 n�veis. Identifique quais vari�veis foram selecionadas pelo modelo, interprete-as, e avalie o desempenho do modelo pela Tabela de Classifica��o.
# Fun��o 'chaid' nos permite criar uma �rvore de decis�o de acordo com o algoritmo CHAID

library(partykit) # pacote precisa ser instalado previamente para usar o CHAID
# install.packages("CHAID", repos="http://R-Forge.R-project.org")
library(CHAID) # pacote com a fun��o 'chaid'

# Todas as vari�veis como um fator (n�o como num�rico para ser input da �rvore)
telefonia$resposta <- as.factor(telefonia$resposta)
telefonia$Idade_q <- as.factor(telefonia$Idade_q)

# Para a �rvore n�o ficar muito grande, a tr�s n�veis
controle <- chaid_control(maxheight = 3)

# Fun��o 'chaid' nos permite criar uma �rvore de decis�o de acordo com o algoritmo CHAID
(arvore_full <- chaid(resposta ~ 
                        Idade_q +
                        Minutos_realizados_T0_q +
                        Tempo_safra_q +
                        Tempo_casa_q +
                        Qtd_retencao_6meses_q +
                        Qtd_prod_q,
                      data = telefonia,
                      control = controle)) # indicando em qual base o modelo deve ser estimado

# Incluir na base de dados a probabilidade predita pela �rvore de Decis�o
probs<- as.data.frame(predict(arvore_full, newdata = telefonia, type = "p")) # "p" salva o valor predito da probabilidade
names(probs) <- c("P_0", "P_1")
telefonia <- cbind(telefonia, probs) # insere 2 colunas na base com as probabilidades preditas de 0 e 1

# Cria vari�vel resposta predita com base na probabilidade predita pela �rvore de Decis�o
telefonia$predict_AD <- as.factor(ifelse(telefonia$P_1 >= 0.008685467, 1, 0)) # transforma a probabilidade em vari�vel bin�ria

# Mostra a tabela de desempenho: Predito x Resposta observada
(tabela_desempenho <- table(telefonia$resposta, telefonia$predict_AD))

# Calcula as medidas de desempenho: Sensibilidade, Especificidade e Acur�cia
(sensibilidade <- tabela_desempenho[2, 2] / sum(tabela_desempenho[2, ]))
(especificidade <- tabela_desempenho[1, 1] / sum(tabela_desempenho[1, ]))
(n <- nrow(telefonia))
(acuracia <- sum(tabela_desempenho[1, 1] + tabela_desempenho[2, 2])/n)

# Fun��o 'chaid' sem Idade
(arvore_red1 <- chaid(resposta ~ 
                        Minutos_realizados_T0_q +
                        Tempo_safra_q +
                        Tempo_casa_q +
                        Qtd_retencao_6meses_q +
                        Qtd_prod_q,
                      data = telefonia,
                      control = controle)) # indicando em qual base o modelo deve ser estimado

probs <- as.data.frame(predict(arvore_red1, newdata = telefonia, type = "p")) # "p" salva o valor predito da probabilidade
names(probs) <- c("P_0", "P_1")
summary(telefonia)
telefonia <- cbind(telefonia, probs) # insere 2 colunas na base com as probabilidades preditas de 0 e 1
telefonia$predict_AD <- as.factor(ifelse(telefonia$P_1 >= 0.008685467, 1, 0)) # transforma a probabilidade em vari�vel bin�ria

(tabela_desempenho <- table(telefonia$resposta, telefonia$predict_AD))
(sensibilidade <- tabela_desempenho[2, 2] / sum(tabela_desempenho[2, ]))
(especificidade <- tabela_desempenho[1, 1] / sum(tabela_desempenho[1, ]))
(n <- nrow(telefonia))
(acuracia <- sum(tabela_desempenho[1, 1] + tabela_desempenho[2, 2]) / n)

# **********************************************************
# (g) Calcule a �rea abaixo da curva ROC, e avalie seu desempenho.

library(pROC)

# �rea abaixo da curva ROC: Regress�o Log�stica
roc(telefonia$resposta,
    telefonia$reg_log_p1,
    plot = TRUE,
    legacy.axes = TRUE,
    print.auc = TRUE,
    main = "Regress�o Log�stica")

# �rea abaixo da curva ROC: �rvore de Decis�o
roc(telefonia$resposta,
    telefonia$P_1,
    plot = TRUE,
    legacy.axes = TRUE,
    print.auc = TRUE,
    main = "�rvore de Decis�o")

# **********************************************************
# (i) Construa a tabela de probabilidade preditas x resposta observada em VINTIS para Regress�o Log�stica, 
# e obtenha de forma an�loga a tabela de probabilidades por n� x resposta para �rvore de Decis�o.
# Use a planilha do Excel 'An_Desempenho_Exercicio'.

# Tabela de Desempenho: Regress�o Log�stica
# (Use a planilha do Excel 'An_Desempenho_Exercicio')
# Calcular as faixas de vintil
#
# ESTE VALOR 20 SERVER PARA CONSEGUIR OBTER 5% DA BASE PARA ANALISE

telefonia$fx_reg_log <- quantcut(telefonia$reg_log_p1, 20)

# Distribui��o da resposta por faixa de probabilidade

(table(telefonia$fx_reg_log, telefonia$resposta))

# Propens�o dos n�s finais: �rvore de Decis�o
telefonia$node <- predict(arvore_red1, type = "node")

# Tabela de Desempenho: �rvore de Decis�o
(tabela_AD <- (table(telefonia$node, telefonia$resposta)))

# Agrega a base para pegar propens�o do associado ao n�
attach(telefonia)

aggdata <- aggregate(telefonia, by = list(node), FUN = mean)
(DE_PARA <- cbind(aggdata$node, round(aggdata$P_1, 4)))
detach(telefonia)

# Copiar as duas tabelas do excel, junt�-las e ordenar em ordem crescente de probabilidade
