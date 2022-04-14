#**********************************************************
#REGRESSÃO LOGÍSTICA - CREDIT SCORE
#**********************************************************
#**********************************************************

#**********************************************************
#Mapear diretório de trabalho
getwd()
#Atenção: Alterar Diretório
setwd("C:\\Users\\Camila Melo\\Documents\\Estudos\\LABData Fia\\aula7")
#**********************************************************


#Uma empresa está preocupada com os clientes novos que entram em sua carteira e apresentam um 'default' (não pagamento da dívida) 
#após um certo período. Esta análise trata da aprovação de um empréstimo de dinheiro para novos clientes em uma instituição financeira. 
#O objetivo é fazer a aprovação de crédito de maneira automática, deixando apenas alguns casos para a análise de crédito manual. 
#Para a aprovação ou não do cliente na instituição é utilizado o modelo de Credit Score que fornece a probabilidade do cliente 
#apresentar um 'default' por meio das informações cadastrais fornecidas pelo cliente e informações restritivas de mercado que a 
#instituição consulta no momento da análise de crédito. 

#**********************************************************
#Leitura da base de dados
credit = read.table("CreditScore_r.txt", header = TRUE, sep = "\t", dec = ".")
names(credit)
str(credit)
nrow(credit)

#(a) Faça a análise exploratória univariada e interprete todas as variáveis do banco de dados. Interprete os resultados na visão do negócio.
#Frequência de todas as variáveis, exceto a variável chave
credit <- dplyr::mutate_if(credit, is.character, as.factor)#converte para factor os caracteres
summary(credit[,-1])

#(b)Faça uma análise do % de default. 
#Percentual da variável resposta
round(prop.table(table(credit$RESPOSTA)),3)
#**********************************************************
#(c) Faça a análise bivariada das variáveis explicativas (covariáveis) vs a variável resposta. Quais variáveis discriminam o evento resposta? Como você poderia tratar as categorias com missings values na análise bivariada?
#Vamos fazer a tabela cruzada entre as covariáveis e a resposta
table(credit$FX_IDADE,credit$RESPOSTA)
table(credit$CEP_GRUPO_RISCO,credit$RESPOSTA)
table(credit$FX_RENDA,credit$RESPOSTA)
table(credit$INDICADOR_RESTRITIVO,credit$RESPOSTA)
table(credit$QTDE_CONSULTAS_CREDITO,credit$RESPOSTA)

#Podemos gerar também as proporções, sumarizando na categoria de cada covariável
round(prop.table(table(credit$FX_IDADE,credit$RESPOSTA),1),2)
round(prop.table(table(credit$CEP_GRUPO_RISCO,credit$RESPOSTA),1),2)
round(prop.table(table(credit$FX_RENDA,credit$RESPOSTA),1),2)
round(prop.table(table(credit$INDICADOR_RESTRITIVO,credit$RESPOSTA),1),2)
round(prop.table(table(credit$QTDE_CONSULTAS_CREDITO,credit$RESPOSTA),1),2)
#**********************************************************
#(d)Rode o modelo de Regressão Logística. 
#Selecione um modelo final no qual a interpretação dos parâmetros 
#esteja de acordo com a análise bivariada.

#Modelo análise
modelo_full <- glm(RESPOSTA ~ FX_IDADE + CEP_GRUPO_RISCO + FX_RENDA +
                     INDICADOR_RESTRITIVO + QTDE_CONSULTAS_CREDITO, family = binomial(link = "logit"), data = credit)

summary(modelo_full)

#Modelo ajustado
modelo_01 <- glm(RESPOSTA ~ FX_IDADE + CEP_GRUPO_RISCO +
                     INDICADOR_RESTRITIVO + QTDE_CONSULTAS_CREDITO, family = binomial(link = "logit"), data = credit)


summary(modelo_01)

#**********************************************************
#(e) Faça a análise de multicolinearidade entre as covariáveis. 
#Reajuste o modelo caso seja necessário, garantindo que as estimativas dos 
#parâmetros fiquem condizentes com a análise exploratória bivariada.

library(lsr)#biblioteca para o cálculo da estatística de Cramers'V
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

#(g)Analise a sensibilidade, especificidade e acurácia pela tabela de 
#classificação. 

#Para o modelo logístico, com a função 'predict', tendo como parâmetro type = 'response' 
#conseguimos obter as probabilidades do modelo para a classificação '1'
credit$p1 <- predict(modelo_full,newdata = credit,type = "response")
summary(credit$p1)

#Cria variável resposta predita com base na probabilidade predita pela Árvore de Decisão
credit$resp_bin1 <- as.factor(ifelse(credit$p1 >= 0.1206993,1,0)) #transforma a probabilidade em variável binária

#Mostra a tabela de desempenho: Predito x Resposta observada

(tabela_desempenho<-table(credit$RESPOSTA,credit$resp_bin1 ))


#Calcula as medidas de desempenho: Sensibilidade, Especificidade e Acurácia
(sensibilidade<-tabela_desempenho[2,2]/sum(tabela_desempenho[2,]))
(especificidade<-tabela_desempenho[1,1]/sum(tabela_desempenho[1,]))
(n<-nrow(credit))
(accuracia<- sum(tabela_desempenho[1,1]+tabela_desempenho[2,2])/n)
(ks <- abs(sensibilidade - (1 - especificidade)))

#Porcentagem de resposta
round((prop.table(table(credit$resp_bin1)) * 100),2)

#ROC
# Área abaixo da curva ROC: Regressão Logística
roc(credit$RESPOSTA,
    credit$p1,
    plot = TRUE,
    legacy.axes = TRUE,
    print.auc = TRUE,
    main = "Regressão Logística")


################################################
# Modelo sem renda
################################################

modelo_red1 <- glm(RESPOSTA ~ FX_IDADE + CEP_GRUPO_RISCO +   
                     INDICADOR_RESTRITIVO + QTDE_CONSULTAS_CREDITO, family = binomial(link = "logit"), data = credit)
summary(modelo_red1)

#(g)Analise a sensibilidade, especificidade e acurácia pela tabela de 
#classificação. 

#Para o modelo logístico, com a função 'predict', tendo como parâmetro type = 'response' 
#conseguimos obter as probabilidades do modelo para a classificação '1'
credit$p1 <- predict(modelo_red1,newdata = credit,type = "response")
summary(credit$p1)

#Cria variável resposta predita com base na probabilidade predita pela Árvore de Decisão
credit$resp_bin1 <- as.factor(ifelse(credit$p1 >= 0.1206993,1,0)) #transforma a probabilidade em variável binária

#Mostra a tabela de desempenho: Predito x Resposta observada

(tabela_desempenho<-table(credit$RESPOSTA,credit$resp_bin1 ))


#Calcula as medidas de desempenho: Sensibilidade, Especificidade e Acurácia
(sensibilidade<-tabela_desempenho[2,2]/sum(tabela_desempenho[2,]))
(especificidade<-tabela_desempenho[1,1]/sum(tabela_desempenho[1,]))
(n<-nrow(credit))
(accuracia<- sum(tabela_desempenho[1,1]+tabela_desempenho[2,2])/n)
(ks <- abs(sensibilidade - (1 - especificidade)))

#Porcentagem de resposta
round((prop.table(table(credit$resp_bin1)) * 100),2)


# **********************************************************
# (g) Calcule a área abaixo da curva ROC, e avalie seu desempenho.

library(pROC)

# Área abaixo da curva ROC: Regressão Logística
roc(credit$RESPOSTA,
    credit$p1,
    plot = TRUE,
    legacy.axes = TRUE,
    print.auc = TRUE,
    main = "Regressão Logística")


########################################
# Modelo sem idade 
########################################

modelo_ida1 <- glm(RESPOSTA ~ FX_RENDA + CEP_GRUPO_RISCO +   
                     INDICADOR_RESTRITIVO + QTDE_CONSULTAS_CREDITO, family = binomial(link = "logit"), data = credit)
summary(modelo_ida1)

credit$p1 <- predict(modelo_ida1,newdata = credit,type = "response")
summary(credit$p1)
#Cria variável resposta predita com base na probabilidade predita pela Árvore de Decisão
credit$resp_bin1 <- as.factor(ifelse(credit$p1 >= 0.1206993,1,0)) #transforma a probabilidade em variável binária

#Mostra a tabela de desempenho: Predito x Resposta observada
(tabela_desempenho<-table(credit$RESPOSTA,credit$resp_bin1 ))
#Calcula as medidas de desempenho: Sensibilidade, Especificidade e Acurácia
(sensibilidade<-tabela_desempenho[2,2]/sum(tabela_desempenho[2,]))
(especificidade<-tabela_desempenho[1,1]/sum(tabela_desempenho[1,]))
(n<-nrow(credit))
(accuracia<- sum(tabela_desempenho[1,1]+tabela_desempenho[2,2])/n)
(ks <- abs(sensibilidade - (1 - especificidade)))

library(pROC)

# Área abaixo da curva ROC: Regressão Logística
roc(credit$RESPOSTA,
    credit$p1,
    plot = TRUE,
    legacy.axes = TRUE,
    print.auc = TRUE,
    main = "Regressão Logística")

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
#Cria variável resposta predita com base na probabilidade predita pela Árvore de Decisão
credit$resp_bin1 <- as.factor(ifelse(credit$p1 >= 0.1206993,1,0)) #transforma a probabilidade em variável binária

#Mostra a tabela de desempenho: Predito x Resposta observada
(tabela_desempenho<-table(credit$RESPOSTA,credit$resp_bin1 ))
#Calcula as medidas de desempenho: Sensibilidade, Especificidade e Acurácia
(sensibilidade<-tabela_desempenho[2,2]/sum(tabela_desempenho[2,]))
(especificidade<-tabela_desempenho[1,1]/sum(tabela_desempenho[1,]))
(n<-nrow(credit))
(accuracia<- sum(tabela_desempenho[1,1]+tabela_desempenho[2,2])/n)
(ks <- abs(sensibilidade - (1 - especificidade)))

library(pROC)

# Área abaixo da curva ROC: Regressão Logística
roc(credit$RESPOSTA,
    credit$p1,
    plot = TRUE,
    legacy.axes = TRUE,
    print.auc = TRUE,
    main = "Regressão Logística")

#Porcentagem de resposta
round((prop.table(table(credit$resp_bin1)) * 100),2)