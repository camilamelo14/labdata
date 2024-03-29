#**********************************************************
#FRAMEWORK MODELAGEM - CREDIT SCORE
#**********************************************************
#�rvore de Decis�o - CHAID

#Mapear diret�rio de trabalho
getwd()
#Apontar diret�rio de dados
setwd("C:\\Users\\Camila Melo\\Documents\\Estudos\\LABData Fia\\aula15")

#Leitura da base de dados
credit = read.table("ML_CreditScore.txt", header = TRUE, sep = "\t", dec = ".")
names(credit)
str(credit)s
nrow(credit)

#Sele��o das safras treino, teste e validacao
#Separa��o das bases de acordo com a defini��o dos per�odos
safra123 <- credit[credit$SAFRA == 1 |
                     credit$SAFRA == 2 |
                     credit$SAFRA == 3, ]
#Parti��o dos dados: Treino (70%) e teste (30%)
set.seed(345) #Definimos uma 'semente' para que a amostra seja sempre igual 
ind <- sample(2, nrow(safra123), replace=TRUE, prob=c(0.7,0.3)) #Fun��o sample utilizada para selecionar os registros aleatoriamente
treino <- safra123[ind==1,] #Base de treinamento - 70% do Total
teste <- safra123[ind==2,] #Base de teste - 30% do Total
#Como a sele��o � aleat�ria, a taxa de churn das duas bases deve se manter muito pr�xima da base total
prop.table(table(treino$RESPOSTA))
prop.table(table(teste$RESPOSTA))
validacao <- credit[credit$SAFRA == 4 |
                      credit$SAFRA == 5 |
                      credit$SAFRA == 6, ]
prop.table(table(validacao$RESPOSTA))

#Ajuste da �rvore de Decis�o - CHAID
library(partykit) # pacote precisa ser instalado previamente para usar o CHAID
# install.packages("CHAID", repos="http://R-Forge.R-project.org")
library(CHAID) # pacote com a fun��o 'chaid'

# Todas as vari�veis como um fator (n�o como num�rico para ser input da �rvore)
treino$FX_IDADE <- as.factor(treino$FX_IDADE)
treino$CEP_GRUPO_RISCO <- as.factor(treino$CEP_GRUPO_RISCO)
treino$FX_RENDA <- as.factor(treino$FX_RENDA)
treino$INDICADOR_RESTRITIVO<- as.factor(treino$INDICADOR_RESTRITIVO)
treino$INDICADOR_RESTRICAO_BANCARIA<- as.factor(treino$INDICADOR_RESTRICAO_BANCARIA)
treino$QTD_CHEQUES_CONSULTADOS<- as.factor(treino$QTD_CHEQUES_CONSULTADOS)
treino$QTDE_CONSULTAS_CREDITO<- as.factor(treino$QTDE_CONSULTAS_CREDITO)
treino$RESPOSTA <- as.factor(treino$RESPOSTA)

# Para a �rvore n�o ficar muito grande, a tr�s n�veis
controle <- chaid_control(maxheight = 4)

# Fun��o 'chaid' nos permite criar uma �rvore de decis�o de acordo com o algoritmo CHAID
(arvore_full <- chaid(RESPOSTA ~ FX_IDADE + CEP_GRUPO_RISCO + FX_RENDA  +
                        INDICADOR_RESTRITIVO + INDICADOR_RESTRICAO_BANCARIA + 
                        QTD_CHEQUES_CONSULTADOS + QTDE_CONSULTAS_CREDITO,
                      data = treino,
                      control = controle)) # indicando em qual base o modelo deve ser estimado

# Incluir na base de dados a probabilidade predita pela �rvore de Decis�o
probs<- as.data.frame(predict(arvore_full, newdata = treino, type = "p")) # "p" salva o valor predito da probabilidade
names(probs) <- c("P_0", "P_1")
treino <- cbind(treino, probs) # insere 2 colunas na base com as probabilidades preditas de 0 e 1

# Cria vari�vel RESPOSTA predita com base na probabilidade predita pela �rvore de Decis�o
treino$predict_AD <- as.factor(ifelse(treino$P_1 >= 0.1207, 1, 0)) # transforma a probabilidade em vari�vel bin�ria

# Mostra a tabela de desempenho: Predito x RESPOSTA observada
(tabela_desempenho <- table(treino$RESPOSTA, treino$predict_AD))

# Calcula as medidas de desempenho: Sensibilidade, Especificidade e Acur�cia
(sensibilidade <- tabela_desempenho[2, 2] / sum(tabela_desempenho[2, ]))
(especificidade <- tabela_desempenho[1, 1] / sum(tabela_desempenho[1, ]))
(n <- nrow(treino))
(acuracia <- sum(tabela_desempenho[1, 1] + tabela_desempenho[2, 2])/n)

#Analise do AUC e KS
library(InformationValue)
ks_stat(actuals=treino$RESPOSTA, predictedScores= treino$P_1)
library(pROC)
roc(treino$RESPOSTA,treino$P_1,print.auc = TRUE)


# Todas as vari�veis como um fator (n�o como num�rico para ser input da �rvore)
teste$FX_IDADE <- as.factor(teste$FX_IDADE)
teste$CEP_GRUPO_RISCO <- as.factor(teste$CEP_GRUPO_RISCO)
teste$FX_RENDA <- as.factor(teste$FX_RENDA)
teste$INDICADOR_RESTRITIVO<- as.factor(teste$INDICADOR_RESTRITIVO)
teste$INDICADOR_RESTRICAO_BANCARIA<- as.factor(teste$INDICADOR_RESTRICAO_BANCARIA)
teste$QTD_CHEQUES_CONSULTADOS<- as.factor(teste$QTD_CHEQUES_CONSULTADOS)
teste$QTDE_CONSULTAS_CREDITO<- as.factor(teste$QTDE_CONSULTAS_CREDITO)
teste$RESPOSTA <- as.factor(teste$RESPOSTA)

# Incluir na base de dados a probabilidade predita pela �rvore de Decis�o
probs<- as.data.frame(predict(arvore_full, newdata = teste, type = "p")) # "p" salva o valor predito da probabilidade
names(probs) <- c("P_0", "P_1")
teste <- cbind(teste, probs) # insere 2 colunas na base com as probabilidades preditas de 0 e 1

# Cria vari�vel RESPOSTA predita com base na probabilidade predita pela �rvore de Decis�o
teste$predict_AD <- as.factor(ifelse(teste$P_1 >= 0.1207, 1, 0)) # transforma a probabilidade em vari�vel bin�ria

# Mostra a tabela de desempenho: Predito x RESPOSTA observada
(tabela_desempenho <- table(teste$RESPOSTA, teste$predict_AD))

# Calcula as medidas de desempenho: Sensibilidade, Especificidade e Acur�cia
(sensibilidade <- tabela_desempenho[2, 2] / sum(tabela_desempenho[2, ]))
(especificidade <- tabela_desempenho[1, 1] / sum(tabela_desempenho[1, ]))
(n <- nrow(teste))
(acuracia <- sum(tabela_desempenho[1, 1] + tabela_desempenho[2, 2])/n)

#Analise do AUC e KS
library(InformationValue)
ks_stat(actuals=teste$RESPOSTA, predictedScores= teste$P_1)
library(pROC)
roc(teste$RESPOSTA,teste$P_1,print.auc = TRUE)

# Todas as vari�veis como um fator (n�o como num�rico para ser input da �rvore)
validacao$FX_IDADE <- as.factor(validacao$FX_IDADE)
validacao$CEP_GRUPO_RISCO <- as.factor(validacao$CEP_GRUPO_RISCO)
validacao$FX_RENDA <- as.factor(validacao$FX_RENDA)
validacao$INDICADOR_RESTRITIVO<- as.factor(validacao$INDICADOR_RESTRITIVO)
validacao$INDICADOR_RESTRICAO_BANCARIA<- as.factor(validacao$INDICADOR_RESTRICAO_BANCARIA)
validacao$QTD_CHEQUES_CONSULTADOS<- as.factor(validacao$QTD_CHEQUES_CONSULTADOS)
validacao$QTDE_CONSULTAS_CREDITO<- as.factor(validacao$QTDE_CONSULTAS_CREDITO)
validacao$RESPOSTA <- as.factor(validacao$RESPOSTA)

# Incluir na base de dados a probabilidade predita pela �rvore de Decis�o
probs<- as.data.frame(predict(arvore_full, newdata = validacao, type = "p")) # "p" salva o valor predito da probabilidade
names(probs) <- c("P_0", "P_1")
validacao <- cbind(validacao, probs) # insere 2 colunas na base com as probabilidades preditas de 0 e 1

# Cria vari�vel RESPOSTA predita com base na probabilidade predita pela �rvore de Decis�o
validacao$predict_AD <- as.factor(ifelse(validacao$P_1 >= 0.1207, 1, 0)) # transforma a probabilidade em vari�vel bin�ria

# Mostra a tabela de desempenho: Predito x RESPOSTA observada
(tabela_desempenho <- table(validacao$RESPOSTA, validacao$predict_AD))

# Calcula as medidas de desempenho: Sensibilidade, Especificidade e Acur�cia
(sensibilidade <- tabela_desempenho[2, 2] / sum(tabela_desempenho[2, ]))
(especificidade <- tabela_desempenho[1, 1] / sum(tabela_desempenho[1, ]))
(n <- nrow(validacao))
(acuracia <- sum(tabela_desempenho[1, 1] + tabela_desempenho[2, 2])/n)

#Analise do AUC e KS
library(InformationValue)
ks_stat(actuals=validacao$RESPOSTA, predictedScores= validacao$P_1)
library(pROC)
roc(validacao$RESPOSTA,validacao$P_1,print.auc = TRUE)
