#**********************************************************
#FRAMEWORK MODELAGEM - CREDIT SCORE
#**********************************************************
#Gradiente Boosting

#Mapear diretório de trabalho
getwd()

#Apontar diretório de dados
setwd("C:\\Users\\Camila Melo\\Documents\\Estudos\\LABData Fia\\aula15")

#Leitura da base de dados
credit = read.table("ML_CreditScore.txt", header = TRUE, sep = "\t", dec = ".")
names(credit)
str(credit)
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

#Ajuste do Gradient Boosting
treino$FX_IDADE <- as.factor(treino$FX_IDADE)
treino$CEP_GRUPO_RISCO <- as.factor(treino$CEP_GRUPO_RISCO)
treino$FX_RENDA  <- as.factor(treino$FX_RENDA)
treino$QTD_CHEQUES_CONSULTADOS  <- as.factor(treino$QTD_CHEQUES_CONSULTADOS)
treino$QTDE_CONSULTAS_CREDITO  <- as.factor(treino$QTDE_CONSULTAS_CREDITO)

 
library(gbm)
set.seed(102)
modelo_gbm <- gbm(RESPOSTA ~ FX_IDADE + CEP_GRUPO_RISCO + FX_RENDA  +
                    INDICADOR_RESTRITIVO + INDICADOR_RESTRICAO_BANCARIA + 
                    QTD_CHEQUES_CONSULTADOS + QTDE_CONSULTAS_CREDITO,
                  data = treino, distribution="bernoulli", n.trees = 50) 
modelo_gbm
summary(modelo_gbm)

#Desempenho na base de treino
#Analise a sensibilidade, especificidade e acurácia pela tabela de classificação 

# Incluir na base de dados a probabilidade predita pelo modelo
probs <- as.data.frame(predict(modelo_gbm, newdata = treino, type="response")) # "response" para salvar a probabilidade do evento inadimplência na base
names(probs) <- c("P_GB_1")
treino <- cbind(treino, probs) # insere 2 colunas na base com as probabilidades preditas de 0 e 1
treino$P_GB_0 <- (1-treino$P_GB_1)

# Cria variável RESPOSTA predita com base na probabilidade predita pela Árvore de Decisão - 'CART'
treino$predict_GB <- as.factor(ifelse(treino$P_GB_1 >= 0.1207, 1, 0)) # transforma a probabilidade em variável binária

# Mostra a tabela de desempenho: Predito x RESPOSTA observada
(tabela_desempenho <- table(treino$RESPOSTA, treino$predict_GB))

# Calcula as medidas de desempenho: Sensibilidade, Especificidade e Acurácia
(sensibilidade <- tabela_desempenho[2, 2] / sum(tabela_desempenho[2, ]))
(especificidade <- tabela_desempenho[1, 1] / sum(tabela_desempenho[1, ]))
(n <- nrow(treino))
(acuracia <- sum(tabela_desempenho[1, 1] + tabela_desempenho[2, 2])/n)

#Analise do AUC e KS
library(InformationValue)
ks_stat(actuals=treino$RESPOSTA, predictedScores= treino$P_GB_1)
library(pROC)
roc(treino$RESPOSTA,treino$P_GB_1,print.auc = TRUE)

#Desempenho na base de teste 
#Analise a sensibilidade, especificidade e acurácia pela tabela de classificação. 

probs<- as.data.frame(predict(modelo_gbm, newdata = teste, type = "response")) # "response" para salvar a probabilidade do evento inadimplência na base
names(probs) <- c("P_GB_1")
teste <- cbind(teste, probs) # insere 2 colunas na base com as probabilidades preditas de 0 e 1
teste$P_GB_0 <- (1-teste$P_GB_1)

# Cria variável RESPOSTA predita com base na probabilidade predita pela Árvore de Decisão
teste$predict_GB <- as.factor(ifelse(teste$P_GB_1 >= 0.1207, 1, 0)) # transforma a probabilidade em variável binária

# Mostra a tabela de desempenho: Predito x RESPOSTA observada
(tabela_desempenho <- table(teste$RESPOSTA, teste$predict_GB))

# Calcula as medidas de desempenho: Sensibilidade, Especificidade e Acurácia
(sensibilidade <- tabela_desempenho[2, 2] / sum(tabela_desempenho[2, ]))
(especificidade <- tabela_desempenho[1, 1] / sum(tabela_desempenho[1, ]))
(n <- nrow(teste))
(acuracia <- sum(tabela_desempenho[1, 1] + tabela_desempenho[2, 2])/n)

#Analise do AUC e KS
ks_stat(actuals=teste$RESPOSTA, predictedScores= teste$P_GB_1)
roc(teste$RESPOSTA,teste$P_GB_1,print.auc = TRUE)

#Desempenho na base de validacao 
#Analise a sensibilidade, especificidade e acurácia pela tabela de classificação

probs<- as.data.frame(predict(modelo_gbm, newdata = validacao, type = "response")) # "response" para salvar a probabilidade do evento inadimplência na base
names(probs) <- c("P_GB_1")
validacao <- cbind(validacao, probs) # insere 2 colunas na base com as probabilidades preditas de 0 e 1
validacao$P_GB_0 <- (1-validacao$P_GB_1)

# Cria variável RESPOSTA predita com base na probabilidade predita pela Árvore de Decisão
validacao$predict_GB <- as.factor(ifelse(validacao$P_GB_1 >= 0.1207, 1, 0)) # transforma a probabilidade em variável binária

# Mostra a tabela de desempenho: Predito x RESPOSTA observada
(tabela_desempenho <- table(validacao$RESPOSTA, validacao$predict_GB))

# Calcula as medidas de desempenho: Sensibilidade, Especificidade e Acurácia
(sensibilidade <- tabela_desempenho[2, 2] / sum(tabela_desempenho[2, ]))
(especificidade <- tabela_desempenho[1, 1] / sum(tabela_desempenho[1, ]))
(n <- nrow(validacao))
(acuracia <- sum(tabela_desempenho[1, 1] + tabela_desempenho[2, 2])/n)

#Analise do AUC e KS
ks_stat(actuals=validacao$RESPOSTA, predictedScores= validacao$P_GB_1)
roc(validacao$RESPOSTA,validacao$P_GB_1,print.auc = TRUE)

