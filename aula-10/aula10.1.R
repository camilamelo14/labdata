#Apontar diretório de dados
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

#Análise Exploratória da Série
summary(retorno$V1)

#BoxPlots
boxplot(retorno$V1, 
        pch = "*",  # tipo de marcador dos outliers
        col = "darkturquoise", # cor do preenchimento do box plot
        border = "darkgrey", # cor da linha do box plot
        main = "Retorno Ações")

#Assimetria
skewness(retorno$V1)

hist(retorno$V1, freq=TRUE, ylab="Frequencia", xlab = "Retorno Ação", main="Histograma retorno ações")

#Avaliação de nulo
stat.desc(retorno, basic = F)

#Gráfico temporals
ts.plot(retorno$V1, ylab="Retorno Ações", xlab="Tempo")

#Teste de Estacionariedade
adf.test(retorno$V1)

#Gráfico de Autocorrelação e Autocorrelação Parcial da série
par(mfrow=c(1,2))
print(acf(retorno$V1))
print(pacf(retorno$V1))

#Ajuste do modelo: 'order = c(X,0,0)' em que X é a ordem do AR e 
#cada NA indica os parâmetros a serem estimados 'fixed = c(NA,NA,NA)'
(modelo <- arima(retorno$V1, order = c(5,0,0), 
                 fixed = c(NA,NA,NA,0,NA,NA)))

#Teste de hipótese dos parâmetros
coeftest(modelo)
par(mfrow=c(1,2))

#Sem intercepto
#Ajuste do modelo: 'order = c(X,0,0)' em que X é a ordem do AR 
# e cada NA indica os parâmetros a serem estimados 
# 'fixed = c(NA,NA,NA)'
(modelo <- arima(retorno$V1, order = c(5,0,0), 
                 fixed = c(NA,NA,NA,0,NA,0)))
coeftest(modelo)
par(mfrow=c(1,2))

#Gráfico de Autocorrelação e Autocorrelação Parcial dos resíduos
acf(residuals(modelo))
pacf(residuals(modelo))

#Projeção N passos para frente
(projecao <- forecast(modelo, h=5))

#Dados observados da próxima semana
#0.103906254
#0.188157433
#-0.080349801
#0.11583838
#0.121133841
