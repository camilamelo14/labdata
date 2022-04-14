#**********************************************************
#FRAMEWORK MODELAGEM - CREDIT SCORE
#**********************************************************
#Árvore de Decisão - CHAID

#Mapear diretório de trabalho
getwd()
#Apontar diretório de dados
setwd("C:\\Users\\Camila Melo\\Documents\\Estudos\\LABData Fia\\aula15")

#Leitura da base de dados
credit = read.table("ML_CreditScore.txt", header = TRUE, sep = "\t", dec = ".")
names(credit)
str(credit)s
nrow(credit)

#Seleção das safras treino, teste e validacao
#Separação das bases de acordo com a definição dos períodos
safra123 <- credit[credit$SAFRA == 1 |
                     credit$SAFRA == 2 |
                     credit$SAFRA == 3, ]
#Partição dos dados: Treino (70%) e teste (30%)
set.seed(345) #Definimos uma 'semente' para que a amostra seja sempre igual 
ind <- sample(2, nrow(safra123), replace=TRUE, prob=c(0.7,0.3)) #Função sample utilizada para selecionar os registros aleatoriamente
treino <- safra123[ind==1,] #Base de treinamento - 70% do Total
teste <- safra123[ind==2,] #Base de teste - 30% do Total
#Como a seleção é aleatória, a taxa de churn das duas bases deve se manter muito próxima da base total
prop.table(table(treino$RESPOSTA))
prop.table(table(teste$RESPOSTA))
validacao <- credit[credit$SAFRA == 4 |
                      credit$SAFRA == 5 |
                      credit$SAFRA == 6, ]
prop.table(table(validacao$RESPOSTA))

#Ajuste da Árvore de Decisão - CHAID
library(partykit) # pacote precisa ser instalado previamente para usar o CHAID
# install.packages("CHAID", repos="http://R-Forge.R-project.org")
library(CHAID) # pacote com a função 'chaid'

# Todas as variáveis como um fator (não como numérico para ser input da Árvore)
treino$FX_IDADE <- as.factor(treino$FX_IDADE)
treino$CEP_GRUPO_RISCO <- as.factor(treino$CEP_GRUPO_RISCO)
treino$FX_RENDA <- as.factor(treino$FX_RENDA)
treino$INDICADOR_RESTRITIVO<- as.factor(treino$INDICADOR_RESTRITIVO)
treino$INDICADOR_RESTRICAO_BANCARIA<- as.factor(treino$INDICADOR_RESTRICAO_BANCARIA)
treino$QTD_CHEQUES_CONSULTADOS<- as.factor(treino$QTD_CHEQUES_CONSULTADOS)
treino$QTDE_CONSULTAS_CREDITO<- as.factor(treino$QTDE_CONSULTAS_CREDITO)
treino$RESPOSTA <- as.factor(treino$RESPOSTA)

# Para a árvore não ficar muito grande, a três níveis
controle <- chaid_control(maxheight = 4)

# Função 'chaid' nos permite criar uma árvore de decisão de acordo com o algoritmo CHAID
(arvore_full <- chaid(RESPOSTA ~ FX_IDADE + CEP_GRUPO_RISCO + FX_RENDA  +
                        INDICADOR_RESTRITIVO + INDICADOR_RESTRICAO_BANCARIA + 
                        QTD_CHEQUES_CONSULTADOS + QTDE_CONSULTAS_CREDITO,
                      data = treino,
                      control = controle)) # indicando em qual base o modelo deve ser estimado

# Incluir na base de dados a probabilidade predita pela Árvore de Decisão
probs<- as.data.frame(predict(arvore_full, newdata = treino, type = "p")) # "p" salva o valor predito da probabilidade
names(probs) <- c("P_0", "P_1")
treino <- cbind(treino, probs) # insere 2 colunas na base com as probabilidades preditas de 0 e 1

# Cria variável RESPOSTA predita com base na probabilidade predita pela Árvore de Decisão
treino$predict_AD <- as.factor(ifelse(treino$P_1 >= 0.1207, 1, 0)) # transforma a probabilidade em variável binária

# Mostra a tabela de desempenho: Predito x RESPOSTA observada
(tabela_desempenho <- table(treino$RESPOSTA, treino$predict_AD))

# Calcula as medidas de desempenho: Sensibilidade, Especificidade e Acurácia
(sensibilidade <- tabela_desempenho[2, 2] / sum(tabela_desempenho[2, ]))
(especificidade <- tabela_desempenho[1, 1] / sum(tabela_desempenho[1, ]))
(n <- nrow(treino))
(acuracia <- sum(tabela_desempenho[1, 1] + tabela_desempenho[2, 2])/n)

#Analise do AUC e KS
library(InformationValue)
ks_stat(actuals=treino$RESPOSTA, predictedScores= treino$P_1)
library(pROC)
roc(treino$RESPOSTA,treino$P_1,print.auc = TRUE)


# Todas as variáveis como um fator (não como numérico para ser input da Árvore)
teste$FX_IDADE <- as.factor(teste$FX_IDADE)
teste$CEP_GRUPO_RISCO <- as.factor(teste$CEP_GRUPO_RISCO)
teste$FX_RENDA <- as.factor(teste$FX_RENDA)
teste$INDICADOR_RESTRITIVO<- as.factor(teste$INDICADOR_RESTRITIVO)
teste$INDICADOR_RESTRICAO_BANCARIA<- as.factor(teste$INDICADOR_RESTRICAO_BANCARIA)
teste$QTD_CHEQUES_CONSULTADOS<- as.factor(teste$QTD_CHEQUES_CONSULTADOS)
teste$QTDE_CONSULTAS_CREDITO<- as.factor(teste$QTDE_CONSULTAS_CREDITO)
teste$RESPOSTA <- as.factor(teste$RESPOSTA)

# Incluir na base de dados a probabilidade predita pela Árvore de Decisão
probs<- as.data.frame(predict(arvore_full, newdata = teste, type = "p")) # "p" salva o valor predito da probabilidade
names(probs) <- c("P_0", "P_1")
teste <- cbind(teste, probs) # insere 2 colunas na base com as probabilidades preditas de 0 e 1

# Cria variável RESPOSTA predita com base na probabilidade predita pela Árvore de Decisão
teste$predict_AD <- as.factor(ifelse(teste$P_1 >= 0.1207, 1, 0)) # transforma a probabilidade em variável binária

# Mostra a tabela de desempenho: Predito x RESPOSTA observada
(tabela_desempenho <- table(teste$RESPOSTA, teste$predict_AD))

# Calcula as medidas de desempenho: Sensibilidade, Especificidade e Acurácia
(sensibilidade <- tabela_desempenho[2, 2] / sum(tabela_desempenho[2, ]))
(especificidade <- tabela_desempenho[1, 1] / sum(tabela_desempenho[1, ]))
(n <- nrow(teste))
(acuracia <- sum(tabela_desempenho[1, 1] + tabela_desempenho[2, 2])/n)

#Analise do AUC e KS
library(InformationValue)
ks_stat(actuals=teste$RESPOSTA, predictedScores= teste$P_1)
library(pROC)
roc(teste$RESPOSTA,teste$P_1,print.auc = TRUE)

# Todas as variáveis como um fator (não como numérico para ser input da Árvore)
validacao$FX_IDADE <- as.factor(validacao$FX_IDADE)
validacao$CEP_GRUPO_RISCO <- as.factor(validacao$CEP_GRUPO_RISCO)
validacao$FX_RENDA <- as.factor(validacao$FX_RENDA)
validacao$INDICADOR_RESTRITIVO<- as.factor(validacao$INDICADOR_RESTRITIVO)
validacao$INDICADOR_RESTRICAO_BANCARIA<- as.factor(validacao$INDICADOR_RESTRICAO_BANCARIA)
validacao$QTD_CHEQUES_CONSULTADOS<- as.factor(validacao$QTD_CHEQUES_CONSULTADOS)
validacao$QTDE_CONSULTAS_CREDITO<- as.factor(validacao$QTDE_CONSULTAS_CREDITO)
validacao$RESPOSTA <- as.factor(validacao$RESPOSTA)

# Incluir na base de dados a probabilidade predita pela Árvore de Decisão
probs<- as.data.frame(predict(arvore_full, newdata = validacao, type = "p")) # "p" salva o valor predito da probabilidade
names(probs) <- c("P_0", "P_1")
validacao <- cbind(validacao, probs) # insere 2 colunas na base com as probabilidades preditas de 0 e 1

# Cria variável RESPOSTA predita com base na probabilidade predita pela Árvore de Decisão
validacao$predict_AD <- as.factor(ifelse(validacao$P_1 >= 0.1207, 1, 0)) # transforma a probabilidade em variável binária

# Mostra a tabela de desempenho: Predito x RESPOSTA observada
(tabela_desempenho <- table(validacao$RESPOSTA, validacao$predict_AD))

# Calcula as medidas de desempenho: Sensibilidade, Especificidade e Acurácia
(sensibilidade <- tabela_desempenho[2, 2] / sum(tabela_desempenho[2, ]))
(especificidade <- tabela_desempenho[1, 1] / sum(tabela_desempenho[1, ]))
(n <- nrow(validacao))
(acuracia <- sum(tabela_desempenho[1, 1] + tabela_desempenho[2, 2])/n)

#Analise do AUC e KS
library(InformationValue)
ks_stat(actuals=validacao$RESPOSTA, predictedScores= validacao$P_1)
library(pROC)
roc(validacao$RESPOSTA,validacao$P_1,print.auc = TRUE)
