#**********************************************************
#FRAMEWORK MODELAGEM - CREDIT SCORE
#**********************************************************
#Regress�o Log�stica

#Mapear diret�rio de trabalho
getwd()
#Aten��o: Alterar Diret�rio
setwd("C:\\Users\\Camila Melo\\Documents\\Estudos\\LABData Fia\\aula15")

#Leitura da base de dados
credit = read.table("ML_CreditScore.txt", header = TRUE, sep = "\t", dec = ".")
names(credit)
str(credit)
nrow(credit)

#Frequ�ncia de todas as vari�veis, exceto a vari�vel chave
summary(credit[,-1])

#Percentual da vari�vel resposta
round(prop.table(table(credit$RESPOSTA)),3)

#Percentuald de resposta por Safra
table(credit$SAFRA,credit$RESPOSTA)
round(prop.table(table(credit$SAFRA,credit$RESPOSTA),1),3)

#**********************************************************
#Fa�a a an�lise bivariada das vari�veis explicativas (covari�veis) vs a vari�vel resposta. Quais vari�veis discriminam o evento resposta? Como voc� poderia tratar as categorias com missings values na an�lise bivariada?
#Vamos fazer a tabela cruzada entre as covari�veis e a resposta
table(credit$FX_IDADE,credit$RESPOSTA)
table(credit$CEP_GRUPO_RISCO,credit$RESPOSTA)
table(credit$FX_RENDA,credit$RESPOSTA)
table(credit$INDICADOR_RESTRITIVO,credit$RESPOSTA)
table(credit$INDICADOR_RESTRICAO_BANCARIA,credit$RESPOSTA)
table(credit$QTD_CHEQUES_CONSULTADOS,credit$RESPOSTA)
table(credit$QTDE_CONSULTAS_CREDITO,credit$RESPOSTA)

#Podemos gerar tamb�m as propor��es, sumarizando na categoria de cada covari�vel
round(prop.table(table(credit$FX_IDADE,credit$RESPOSTA),1),2)
round(prop.table(table(credit$CEP_GRUPO_RISCO,credit$RESPOSTA),1),2)
round(prop.table(table(credit$FX_RENDA,credit$RESPOSTA),1),2)
round(prop.table(table(credit$INDICADOR_RESTRITIVO,credit$RESPOSTA),1),2)
round(prop.table(table(credit$INDICADOR_RESTRICAO_BANCARIA,credit$RESPOSTA),1),2)
round(prop.table(table(credit$QTD_CHEQUES_CONSULTADOS,credit$RESPOSTA),1),2)
round(prop.table(table(credit$QTDE_CONSULTAS_CREDITO,credit$RESPOSTA),1),2)

#**********************************************************
#Fa�a a an�lise de multicolinearidade entre as covari�veis. 
library(lsr)#biblioteca para o c�lculo da estat�stica de Cramers'V
cramersV(table(credit$FX_IDADE,credit$CEP_GRUPO_RISCO))
cramersV(table(credit$FX_IDADE,credit$FX_RENDA))
cramersV(table(credit$FX_IDADE,credit$INDICADOR_RESTRITIVO))
cramersV(table(credit$FX_IDADE,credit$INDICADOR_RESTRICAO_BANCARIA))
cramersV(table(credit$FX_IDADE,credit$QTD_CHEQUES_CONSULTADOS))
cramersV(table(credit$FX_IDADE,credit$QTDE_CONSULTAS_CREDITO))

cramersV(table(credit$CEP_GRUPO_RISCO,credit$FX_RENDA))
cramersV(table(credit$CEP_GRUPO_RISCO,credit$INDICADOR_RESTRITIVO))
cramersV(table(credit$CEP_GRUPO_RISCO,credit$INDICADOR_RESTRICAO_BANCARIA))
cramersV(table(credit$CEP_GRUPO_RISCO,credit$QTD_CHEQUES_CONSULTADOS))
cramersV(table(credit$CEP_GRUPO_RISCO,credit$QTDE_CONSULTAS_CREDITO))

cramersV(table(credit$FX_RENDA,credit$INDICADOR_RESTRITIVO))
cramersV(table(credit$FX_RENDA,credit$INDICADOR_RESTRICAO_BANCARIA))
cramersV(table(credit$FX_RENDA,credit$QTD_CHEQUES_CONSULTADOS))
cramersV(table(credit$FX_RENDA,credit$QTDE_CONSULTAS_CREDITO))

cramersV(table(credit$INDICADOR_RESTRITIVO,credit$INDICADOR_RESTRICAO_BANCARIA))
cramersV(table(credit$INDICADOR_RESTRITIVO,credit$QTD_CHEQUES_CONSULTADOS))
cramersV(table(credit$INDICADOR_RESTRITIVO,credit$QTDE_CONSULTAS_CREDITO))

cramersV(table(credit$INDICADOR_RESTRICAO_BANCARIA,credit$QTD_CHEQUES_CONSULTADOS))
cramersV(table(credit$INDICADOR_RESTRITIVO,credit$QTDE_CONSULTAS_CREDITO))

cramersV(table(credit$QTD_CHEQUES_CONSULTADOS,credit$QTDE_CONSULTAS_CREDITO))

#########
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

#Modelo full 
modelo_full_treino <- glm(RESPOSTA ~ FX_IDADE + CEP_GRUPO_RISCO + FX_RENDA +  
                            INDICADOR_RESTRITIVO + INDICADOR_RESTRICAO_BANCARIA + 
                            QTD_CHEQUES_CONSULTADOS + QTDE_CONSULTAS_CREDITO, 
                          family = binomial(link = "logit"), data = treino)
summary(modelo_full_treino)

#Modelo sem FX_RENDA
modelo_red1_treino <- glm(RESPOSTA ~ FX_IDADE + CEP_GRUPO_RISCO +   
                            INDICADOR_RESTRITIVO + INDICADOR_RESTRICAO_BANCARIA + 
                            QTD_CHEQUES_CONSULTADOS + QTDE_CONSULTAS_CREDITO, 
                          family = binomial(link = "logit"), data = treino)
summary(modelo_red1_treino)

#Modelo sem INDICADOR_RESTRICAO_BANCARIA e QTD_CHEQUES_CONSULTADOS
modelo_red1_treino <- glm(RESPOSTA ~ FX_IDADE + CEP_GRUPO_RISCO +   
                            INDICADOR_RESTRITIVO + QTDE_CONSULTAS_CREDITO, 
                          family = binomial(link = "logit"), data = treino)
summary(modelo_red1_treino)

#Desempenho na base de treino
#Analise a sensibilidade, especificidade e acur�cia pela tabela de classifica��o 

#Para o modelo log�stico, com a fun��o 'predict', tendo como par�metro type = 'response' conseguimos obter as probabilidades do modelo para a classifica��o '1'
treino$p1 <- predict(modelo_red1_treino,newdata = treino,type = "response")
#Cria vari�vel resposta predita com base na probabilidade predita pelo modelo
treino$resp_bin1 <- as.factor(ifelse(treino$p1 >= 0.1207,1,0)) #transforma a probabilidade em vari�vel bin�ria

#Mostra a tabela de desempenho: Predito x Resposta observada
(tabela_desempenho<-table(treino$RESPOSTA,treino$resp_bin1 ))
#Calcula as medidas de desempenho: Sensibilidade, Especificidade e Acur�cia
(sensibilidade<-tabela_desempenho[2,2]/sum(tabela_desempenho[2,]))
(especificidade<-tabela_desempenho[1,1]/sum(tabela_desempenho[1,]))
(n<-nrow(treino))
(accuracia<- sum(tabela_desempenho[1,1]+tabela_desempenho[2,2])/n)

#Analise do AUC e KS
library(InformationValue)
ks_stat(actuals=treino$RESPOSTA, predictedScores= treino$p1)
library(pROC)
roc(treino$RESPOSTA,treino$p1,print.auc = TRUE)

#Desempenho na base de teste - PEDIR PARA OS ALUNOS TROCAREM JUNTOS
#Analise a sensibilidade, especificidade e acur�cia pela tabela de classifica��o. 
#Para o modelo log�stico, com a fun��o 'predict', tendo como par�metro type = 'response' conseguimos obter as probabilidades do modelo para a classifica��o '1'
teste$p1 <- predict(modelo_red1_treino,newdata = teste,type = "response")
#Cria vari�vel resposta predita com base na probabilidade predita pelo modelo
teste$resp_bin1 <- as.factor(ifelse(teste$p1 >= 0.1207,1,0)) #transforma a probabilidade em vari�vel bin�ria

#Mostra a tabela de desempenho: Predito x Resposta observada
(tabela_desempenho<-table(teste$RESPOSTA,teste$resp_bin1 ))
#Calcula as medidas de desempenho: Sensibilidade, Especificidade e Acur�cia
(sensibilidade<-tabela_desempenho[2,2]/sum(tabela_desempenho[2,]))
(especificidade<-tabela_desempenho[1,1]/sum(tabela_desempenho[1,]))
(n<-nrow(teste))
(accuracia<- sum(tabela_desempenho[1,1]+tabela_desempenho[2,2])/n)

#Analise do AUC e KS
ks_stat(actuals=teste$RESPOSTA, predictedScores= teste$p1)
roc(teste$RESPOSTA,teste$p1,print.auc = TRUE)

#Desempenho na base de validacao - PEDIR PARA OS ALUNOS TROCAREM JUNTOS
#Analise a sensibilidade, especificidade e acur�cia pela tabela de classifica��o 
#Para o modelo log�stico, com a fun��o 'predict', tendo como par�metro type = 'response' conseguimos obter as probabilidades do modelo para a classifica��o '1'
validacao$p1 <- predict(modelo_red1_treino,newdata = validacao,type = "response")
#Cria vari�vel resposta predita com base na probabilidade predita pela �rvore de Decis�o
validacao$resp_bin1 <- as.factor(ifelse(validacao$p1 >= 0.1207,1,0)) #transforma a probabilidade em vari�vel bin�ria

#Mostra a tabela de desempenho: Predito x Resposta observada
(tabela_desempenho<-table(validacao$RESPOSTA,validacao$resp_bin1 ))
#Calcula as medidas de desempenho: Sensibilidade, Especificidade e Acur�cia
(sensibilidade<-tabela_desempenho[2,2]/sum(tabela_desempenho[2,]))
(especificidade<-tabela_desempenho[1,1]/sum(tabela_desempenho[1,]))
(n<-nrow(validacao))
(accuracia<- sum(tabela_desempenho[1,1]+tabela_desempenho[2,2])/n)

#Analise do AUC e KS
ks_stat(actuals=validacao$RESPOSTA, predictedScores= validacao$p1)
roc(validacao$RESPOSTA,validacao$p1,print.auc = TRUE)

