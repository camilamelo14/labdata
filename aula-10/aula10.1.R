#Apontar diret�rio de dados
setwd("C:\\Users\\Camila Melo\\Documents\\Estudos\\LABData Fia\\aula10")
getwd()

#Libs
library(tseries)
library(lmtest)
library(pastecs)
library(moments)
library(forecast)

#Leitura da base de dados
retorno<- read.table("retorno_pred.txt", header = FALSE )

#An�lise Explorat�ria da S�rie
summary(retorno$V1)

#BoxPlots
boxplot(retorno$V1, 
        pch = "*",  # tipo de marcador dos outliers
        col = "darkturquoise", # cor do preenchimento do box plot
        border = "darkgrey", # cor da linha do box plot
        main = "Retorno A��es")

#Assimetria
skewness(retorno$V1)

hist(retorno$V1, freq=TRUE, ylab="Frequencia", xlab = "Retorno A��o", main="Histograma retorno a��es")

#Avalia��o de nulo
stat.desc(retorno, basic = F)

#Gr�fico temporals
ts.plot(retorno$V1, ylab="Retorno A��es", xlab="Tempo")

#Teste de Estacionariedade
adf.test(retorno$V1)

#Gr�fico de Autocorrela��o e Autocorrela��o Parcial da s�rie
par(mfrow=c(1,2))
print(acf(retorno$V1))
print(pacf(retorno$V1))

#Ajuste do modelo: 'order = c(X,0,0)' em que X � a ordem do AR e 
#cada NA indica os par�metros a serem estimados 'fixed = c(NA,NA,NA)'
(modelo <- arima(retorno$V1, order = c(5,0,0), 
                 fixed = c(NA,NA,NA,0,NA,NA)))

#Teste de hip�tese dos par�metros
coeftest(modelo)
par(mfrow=c(1,2))

#Sem intercepto
#Ajuste do modelo: 'order = c(X,0,0)' em que X � a ordem do AR 
# e cada NA indica os par�metros a serem estimados 
# 'fixed = c(NA,NA,NA)'
(modelo <- arima(retorno$V1, order = c(5,0,0), 
                 fixed = c(NA,NA,NA,0,NA,0)))
coeftest(modelo)
par(mfrow=c(1,2))

#Gr�fico de Autocorrela��o e Autocorrela��o Parcial dos res�duos
acf(residuals(modelo))
pacf(residuals(modelo))

#Proje��o N passos para frente
(projecao <- forecast(modelo, h=5))

#Dados observados da pr�xima semana
#0.103906254
#0.188157433
#-0.080349801
#0.11583838
#0.121133841
