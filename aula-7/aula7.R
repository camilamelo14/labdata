#**********************************************************
#REGRESS�O LOG�STICA - CREDIT SCORE
#**********************************************************
#**********************************************************

#**********************************************************
#Mapear diret�rio de trabalho
getwd()
#Aten��o: Alterar Diret�rio
setwd("C:\\Users\\Camila Melo\\Documents\\Estudos\\LABData Fia\\aula7")
#**********************************************************


#Uma empresa est� preocupada com os clientes novos que entram em sua carteira e apresentam um 'default' (n�o pagamento da d�vida) 
#ap�s um certo per�odo. Esta an�lise trata da aprova��o de um empr�stimo de dinheiro para novos clientes em uma institui��o financeira. 
#O objetivo � fazer a aprova��o de cr�dito de maneira autom�tica, deixando apenas alguns casos para a an�lise de cr�dito manual. 
#Para a aprova��o ou n�o do cliente na institui��o � utilizado o modelo de Credit Score que fornece a probabilidade do cliente 
#apresentar um 'default' por meio das informa��es cadastrais fornecidas pelo cliente e informa��es restritivas de mercado que a 
#institui��o consulta no momento da an�lise de cr�dito. 

#**********************************************************
#Leitura da base de dados
credit = read.table("CreditScore_r.txt", header = TRUE, sep = "\t", dec = ".")
names(credit)
str(credit)
nrow(credit)

#(a) Fa�a a an�lise explorat�ria univariada e interprete todas as vari�veis do banco de dados. Interprete os resultados na vis�o do neg�cio.
#Frequ�ncia de todas as vari�veis, exceto a vari�vel chave
credit <- dplyr::mutate_if(credit, is.character, as.factor)#converte para factor os caracteres
summary(credit[,-1])

#(b)Fa�a uma an�lise do % de default. 
#Percentual da vari�vel resposta
round(prop.table(table(credit$RESPOSTA)),3)
#**********************************************************
#(c) Fa�a a an�lise bivariada das vari�veis explicativas (covari�veis) vs a vari�vel resposta. Quais vari�veis discriminam o evento resposta? Como voc� poderia tratar as categorias com missings values na an�lise bivariada?
#Vamos fazer a tabela cruzada entre as covari�veis e a resposta
table(credit$FX_IDADE,credit$RESPOSTA)
table(credit$CEP_GRUPO_RISCO,credit$RESPOSTA)
table(credit$FX_RENDA,credit$RESPOSTA)
table(credit$INDICADOR_RESTRITIVO,credit$RESPOSTA)
table(credit$QTDE_CONSULTAS_CREDITO,credit$RESPOSTA)

#Podemos gerar tamb�m as propor��es, sumarizando na categoria de cada covari�vel
round(prop.table(table(credit$FX_IDADE,credit$RESPOSTA),1),2)
round(prop.table(table(credit$CEP_GRUPO_RISCO,credit$RESPOSTA),1),2)
round(prop.table(table(credit$FX_RENDA,credit$RESPOSTA),1),2)
round(prop.table(table(credit$INDICADOR_RESTRITIVO,credit$RESPOSTA),1),2)
round(prop.table(table(credit$QTDE_CONSULTAS_CREDITO,credit$RESPOSTA),1),2)
#**********************************************************
#(d)Rode o modelo de Regress�o Log�stica. 
#Selecione um modelo final no qual a interpreta��o dos par�metros 
#esteja de acordo com a an�lise bivariada.

#Modelo an�lise
modelo_full <- glm(RESPOSTA ~ FX_IDADE + CEP_GRUPO_RISCO + FX_RENDA +
                     INDICADOR_RESTRITIVO + QTDE_CONSULTAS_CREDITO, family = binomial(link = "logit"), data = credit)

summary(modelo_full)

#Modelo ajustado
modelo_01 <- glm(RESPOSTA ~ FX_IDADE + CEP_GRUPO_RISCO +
                     INDICADOR_RESTRITIVO + QTDE_CONSULTAS_CREDITO, family = binomial(link = "logit"), data = credit)


summary(modelo_01)

#**********************************************************
#(e) Fa�a a an�lise de multicolinearidade entre as covari�veis. 
#Reajuste o modelo caso seja necess�rio, garantindo que as estimativas dos 
#par�metros fiquem condizentes com a an�lise explorat�ria bivariada.

library(lsr)#biblioteca para o c�lculo da estat�stica de Cramers'V
cramersV(table(credit$FX_IDADE,credit$CEP_GRUPO_RISCO))
cramersV(table(credit$FX_IDADE,credit$FX_RENDA))
cramersV(table(credit$FX_IDADE,credit$INDICADOR_RESTRITIVO))
cramersV(table(credit$FX_IDADE,credit$QTDE_CONSULTAS_CREDITO))
cramersV(table(credit$CEP_GRUPO_RISCO,credit$FX_RENDA))
cramersV(table(credit$CEP_GRUPO_RISCO,credit$INDICADOR_RESTRITIVO))
cramersV(table(credit$CEP_GRUPO_RISCO,credit$QTDE_CONSULTAS_CREDITO))
cramersV(table(credit$FX_RENDA,credit$INDICADOR_RESTRITIVO))
cramersV(table(credit$FX_RENDA,credit$QTDE_CONSULTAS_CREDITO))
cramersV(table(credit$INDICADOR_RESTRITIVO,credit$QTDE_CONSULTAS_CREDITO))

library(pROC)

################################################
# Modelo Completo
################################################

modelo_full <- glm(RESPOSTA ~ FX_IDADE + CEP_GRUPO_RISCO + FX_RENDA +   
                     INDICADOR_RESTRITIVO + QTDE_CONSULTAS_CREDITO, family = binomial(link = "logit"), data = credit)
summary(modelo_full)

#(g)Analise a sensibilidade, especificidade e acur�cia pela tabela de 
#classifica��o. 

#Para o modelo log�stico, com a fun��o 'predict', tendo como par�metro type = 'response' 
#conseguimos obter as probabilidades do modelo para a classifica��o '1'
credit$p1 <- predict(modelo_full,newdata = credit,type = "response")
summary(credit$p1)

#Cria vari�vel resposta predita com base na probabilidade predita pela �rvore de Decis�o
credit$resp_bin1 <- as.factor(ifelse(credit$p1 >= 0.1206993,1,0)) #transforma a probabilidade em vari�vel bin�ria

#Mostra a tabela de desempenho: Predito x Resposta observada

(tabela_desempenho<-table(credit$RESPOSTA,credit$resp_bin1 ))


#Calcula as medidas de desempenho: Sensibilidade, Especificidade e Acur�cia
(sensibilidade<-tabela_desempenho[2,2]/sum(tabela_desempenho[2,]))
(especificidade<-tabela_desempenho[1,1]/sum(tabela_desempenho[1,]))
(n<-nrow(credit))
(accuracia<- sum(tabela_desempenho[1,1]+tabela_desempenho[2,2])/n)
(ks <- abs(sensibilidade - (1 - especificidade)))

#Porcentagem de resposta
round((prop.table(table(credit$resp_bin1)) * 100),2)

#ROC
# �rea abaixo da curva ROC: Regress�o Log�stica
roc(credit$RESPOSTA,
    credit$p1,
    plot = TRUE,
    legacy.axes = TRUE,
    print.auc = TRUE,
    main = "Regress�o Log�stica")


################################################
# Modelo sem renda
################################################

modelo_red1 <- glm(RESPOSTA ~ FX_IDADE + CEP_GRUPO_RISCO +   
                     INDICADOR_RESTRITIVO + QTDE_CONSULTAS_CREDITO, family = binomial(link = "logit"), data = credit)
summary(modelo_red1)

#(g)Analise a sensibilidade, especificidade e acur�cia pela tabela de 
#classifica��o. 

#Para o modelo log�stico, com a fun��o 'predict', tendo como par�metro type = 'response' 
#conseguimos obter as probabilidades do modelo para a classifica��o '1'
credit$p1 <- predict(modelo_red1,newdata = credit,type = "response")
summary(credit$p1)

#Cria vari�vel resposta predita com base na probabilidade predita pela �rvore de Decis�o
credit$resp_bin1 <- as.factor(ifelse(credit$p1 >= 0.1206993,1,0)) #transforma a probabilidade em vari�vel bin�ria

#Mostra a tabela de desempenho: Predito x Resposta observada

(tabela_desempenho<-table(credit$RESPOSTA,credit$resp_bin1 ))


#Calcula as medidas de desempenho: Sensibilidade, Especificidade e Acur�cia
(sensibilidade<-tabela_desempenho[2,2]/sum(tabela_desempenho[2,]))
(especificidade<-tabela_desempenho[1,1]/sum(tabela_desempenho[1,]))
(n<-nrow(credit))
(accuracia<- sum(tabela_desempenho[1,1]+tabela_desempenho[2,2])/n)
(ks <- abs(sensibilidade - (1 - especificidade)))

#Porcentagem de resposta
round((prop.table(table(credit$resp_bin1)) * 100),2)


# **********************************************************
# (g) Calcule a �rea abaixo da curva ROC, e avalie seu desempenho.

library(pROC)

# �rea abaixo da curva ROC: Regress�o Log�stica
roc(credit$RESPOSTA,
    credit$p1,
    plot = TRUE,
    legacy.axes = TRUE,
    print.auc = TRUE,
    main = "Regress�o Log�stica")


########################################
# Modelo sem idade 
########################################

modelo_ida1 <- glm(RESPOSTA ~ FX_RENDA + CEP_GRUPO_RISCO +   
                     INDICADOR_RESTRITIVO + QTDE_CONSULTAS_CREDITO, family = binomial(link = "logit"), data = credit)
summary(modelo_ida1)

credit$p1 <- predict(modelo_ida1,newdata = credit,type = "response")
summary(credit$p1)
#Cria vari�vel resposta predita com base na probabilidade predita pela �rvore de Decis�o
credit$resp_bin1 <- as.factor(ifelse(credit$p1 >= 0.1206993,1,0)) #transforma a probabilidade em vari�vel bin�ria

#Mostra a tabela de desempenho: Predito x Resposta observada
(tabela_desempenho<-table(credit$RESPOSTA,credit$resp_bin1 ))
#Calcula as medidas de desempenho: Sensibilidade, Especificidade e Acur�cia
(sensibilidade<-tabela_desempenho[2,2]/sum(tabela_desempenho[2,]))
(especificidade<-tabela_desempenho[1,1]/sum(tabela_desempenho[1,]))
(n<-nrow(credit))
(accuracia<- sum(tabela_desempenho[1,1]+tabela_desempenho[2,2])/n)
(ks <- abs(sensibilidade - (1 - especificidade)))

library(pROC)

# �rea abaixo da curva ROC: Regress�o Log�stica
roc(credit$RESPOSTA,
    credit$p1,
    plot = TRUE,
    legacy.axes = TRUE,
    print.auc = TRUE,
    main = "Regress�o Log�stica")

#Porcentagem de resposta
round((prop.table(table(credit$resp_bin1)) * 100),2)

########################################
# Modelo sem idade e faixa de renda
########################################

modelo_red1 <- glm(RESPOSTA ~ CEP_GRUPO_RISCO +   
                     INDICADOR_RESTRITIVO + QTDE_CONSULTAS_CREDITO, family = binomial(link = "logit"), data = credit)
summary(modelo_red1)

credit$p1 <- predict(modelo_red1,newdata = credit,type = "response")
summary(credit$p1)
#Cria vari�vel resposta predita com base na probabilidade predita pela �rvore de Decis�o
credit$resp_bin1 <- as.factor(ifelse(credit$p1 >= 0.1206993,1,0)) #transforma a probabilidade em vari�vel bin�ria

#Mostra a tabela de desempenho: Predito x Resposta observada
(tabela_desempenho<-table(credit$RESPOSTA,credit$resp_bin1 ))
#Calcula as medidas de desempenho: Sensibilidade, Especificidade e Acur�cia
(sensibilidade<-tabela_desempenho[2,2]/sum(tabela_desempenho[2,]))
(especificidade<-tabela_desempenho[1,1]/sum(tabela_desempenho[1,]))
(n<-nrow(credit))
(accuracia<- sum(tabela_desempenho[1,1]+tabela_desempenho[2,2])/n)
(ks <- abs(sensibilidade - (1 - especificidade)))

library(pROC)

# �rea abaixo da curva ROC: Regress�o Log�stica
roc(credit$RESPOSTA,
    credit$p1,
    plot = TRUE,
    legacy.axes = TRUE,
    print.auc = TRUE,
    main = "Regress�o Log�stica")

#Porcentagem de resposta
round((prop.table(table(credit$resp_bin1)) * 100),2)