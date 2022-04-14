#************************ CONFIGURACAO ********************
#Mapear diretorio de trabalho
getwd()
#Aten√ß√£o: Alterar Diretorio
setwd("C:\\Users\\Camila Melo\\Documents\\Estudos\\LABData Fia\\aula4")

#**********************************************************

#Leitura da Base de Dados
dados_rls=read.table("Captacao_Alunos.txt", header = TRUE, sep = "\t", dec = ".")

#Verificar vari√°veis
names(dados_rls)
summary(dados_rls)

#Modelo de Regressao Linear Simples
regressao <- lm(data=dados_rls, Y2016 ~ X2015) #linearmodel - covar - target
summary(regressao)

# 1 ver a inclinacao, ideal != 0 
# X2015         0.8391  -> Se for = 0 nao ha modelo linear aplicavel
# (Intercept)  15.9139  -> Valor constante

# Pr(>|t|)  
# 0.0549   --> boardeline
# 7.36e-08 --> 0,00000000736 + confiavel

# O P Valor deve ser avaliado conforme a confiancia que voce adotou como valido no estudo

# R≤ = Quanto + proximo de 1 melhor o modelo √©

#************************ EXERCICIO ************************ 
#Leitura da Base de Dados
imobiliario <- read.table("Imobiliario.txt", header = TRUE, sep = "\t", dec = ".")

#Verificar variaveis
names(imobiliario)

#Analise ExploratÛia
summary(imobiliario)

#Gr·fico de dispersao
plot(imobiliario$Distancia_metro_Km, imobiliario$Mil_reais_m2)

#plot(imobiliario$ Distancia_metro_Km, imobiliario$Mil_reais_m2, ylab="Pre√ßo (Mil R$/m2)", xlab="Dist√¢ncia (km)", col='darkturquoise', xlim = c(0,3), ylim = c(0,20))
#abline(lm(data=imobiliario, Mil_reais_m2 ~ Distancia_metro_Km), col='blue')

#Correlacao Linear de Pearson
cor(imobiliario$ Distancia_metro_Km, imobiliario$Mil_reais_m2)

#Regress„o Linear Simples
regressao <- lm(data=imobiliario, Mil_reais_m2 ~ Distancia_metro_Km) 
# X = VAR RESPOSTA E Y = VAR COVARIAVEL

summary(regressao)

y <- 18.8154+(-7.2166)*2
y
y*1000*70

#Iremos avaliar se a reta do intercepto e da inclinaÁ„o È != 0, desta forma seguimos com o teste de hipotese
#Para isso observa-se o PValor.
#Quando for igual a 0 tem hipÛtese nula

#************************ REGRESSAO LINEAR MULTIPLA ************************ 
#*
##Analise regressao multipla

summary(regressao)
#Modelo anterior sem Idade
regressao_1 <- lm(data=dados_lim_cred, LimitedoChequeEspecial ~ Salario+LimitedeCreditoImediato)
summary(regressao_1)

#Leitura da Base de Dados
dados_lim_cred_esc=read.table("Limite_Credito_Escolaridade.txt", header = TRUE, sep = "\t", dec = ".")
names(dados_lim_cred_esc)

# Matriz de Gr·fico de Dispers„o
#Matriz de Scatter Plot
library(GGally)
ggpairs(dados_lim_cred_esc, title="correlogram with ggpairs()") 

regressao_2 <- lm(data=dados_lim_cred_esc, LimitedoChequeEspecial ~ Idade+Salario+LimitedeCreditoImediato+Escolaridade)
summary(regressao_2)

regressao_3 <- lm(data=dados_lim_cred_esc, LimitedoChequeEspecial ~ Salario+LimitedeCreditoImediato+Escolaridade)
summary(regressao_3)

regressao_3 <- lm(data=dados_lim_cred_esc, LimitedoChequeEspecial ~ Salario+LimitedeCreditoImediato+Escolaridade)
summary(regressao_3)
