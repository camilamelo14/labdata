#Case Call Center

#Apontar diret�rio de dados
setwd("C:\\Users\\Camila Melo\\Documents\\Estudos\\LABData Fia\\aula10")
getwd()

#Leitura da base de dados
chamados_tlmk <- read.table("Operacao_telemarketing.txt", header = FALSE)

#An�lise Explorat�ria da S�rie
summary(chamados_tlmk$V1)

#BoxPlots
boxplot(chamados_tlmk$V1, 
        pch = "*",  # tipo de marcador dos outliers
        col = "darkturquoise", # cor do preenchimento do box plot
        border = "darkgrey", # cor da linha do box plot
        main = "Liga��es CallCenter")

#Gr�fico temporal
ts.plot(chamados_tlmk$V1)

#Teste de Estacionariedade

library(tseries)
adf.test(chamados_tlmk$V1)

#Gr�fico de Autocorrela��o e Autocorrela��o Parcial da s�rie
par(mfrow=c(1,2))
print(acf(chamados_tlmk$V1))
print(pacf(chamados_tlmk$V1))

#Ajuste do modelo: 'order = c(X,0,0)' em que X � a ordem do AR 
# e cada NA indica os par�metros a serem estimados 
# 'fixed = c(NA,NA,NA)'
(modelo <- arima(chamados_tlmk$V1, order = c(2,0,0), fixed = c(NA,NA,NA)))

#Teste de hip�tese dos par�metros
library(lmtest)
coeftest(modelo)
par(mfrow=c(1,2))

#Gr�fico de Autocorrela��o e Autocorrela��o Parcial dos res�duos
acf(residuals(modelo))
pacf(residuals(modelo))

#Proje��o N passos para frente
library(forecast)
(projecao <- forecast(modelo, h=5))

#conta feita na m�o, ver video 22:39
alpha_0 <- 877.396897*(1-0.381796+0.103981)
alpha_0

(previsao_467 <-alpha_0 + 0.3817957*627-0.1039812*711)
(previsao_468 <-alpha_0 + 0.381796*799.1-0.103981*627)

## Teste do modelo retirando 5 dias para avalia��o de performance
n = nrow(chamados_tlmk)-5
n

chamados_tlmk_tmp = head(chamados_tlmk,n)

(modelo <- arima(chamados_tlmk_tmp$V1, order = c(2,0,0), fixed = c(NA,NA,NA)))
(projecao <- forecast(modelo, h=5))

tail(chamados_tlmk,5)

