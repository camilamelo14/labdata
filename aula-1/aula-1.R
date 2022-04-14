######################################################
#              Operacoes mat. basicas
a=1
a

(a<-1)

# Criar uma operação (+, -, / ou *, **)
a<-2
b<-3

(c<-a+b)
(c<-a-b)
(c<-a*b) 

round((c<-a/b),2)
(c<-b**3)
######################################################

setwd('/home/camilamelo/Documentos/ProjetosGit/labdata/aula-1/')
getwd() #mostra o diretório onde está apontado

#df = read.csv('data/Companhia_MB.csv', sep = ';', header = TRUE)
#summary(df)

# Leitura da base de dados
dados <- read.table("data/Companhia_MB.txt", header = TRUE, sep = "\t", dec = ".") #R lê como ponto decimais

#Abrir base de dados no R - Tipo de arquivo .csv
#dados <- read.csv("Companhia_MB.csv",sep = ";",dec = ",")

#Visão geral: descritiva dos campos
summary(dados)

#####################################
## Análise Exploratória Univariada ##
#####################################

#Tabela de frequencias: grau de instrução
table(dados$grau_instrucao)                #Frequencia absoluta
prop.table(table(dados$grau_instrucao))    #Frequencia proporcional
#Alternativa
install.packages("summarytools")
library(summarytools)
freq(dados$grau_instrucao)

#Gráfico de Barras: grau de instrução
barplot(table(dados$grau_instrucao))

#Versão formatada do gráfico
barplot(
  table(dados$grau_instrucao),
  ylab = "Frequência",
  cex.names = 0.7,
  names.arg = c("Fundamental","Médio", "Superior"),
  col = "darkturquoise",
  border  =NA,
  main = "Gráfico de barras: Grau de instrução",
  axes = TRUE,
  ylim = c(0,20))

#Gráfico de Pizza ou setores: grau de instrução
pie(table(dados$grau_instrucao))
#Versão formatada do gráfico
aux1<-table(dados$grau_instrucao)
aux2<-prop.table(table(dados$grau_instrucao))
colors = c("darkturquoise", "cyan", "grey") #escolha de cores
labs<-paste(1:3,"(",aux1,"; ",round(aux2,1),"%)",sep="") #colocar rótulos
pie(table(dados$grau_instrucao),labels=labs,col=colors,radius = 0.8,cex=0.7, main="Gráfico de setores: Grau de instrução")
legend(-1.1,-0.85,legend=c("1-Fundamental, 2-Médio, 3-Superior"),border=NA,box.col=NA,cex=0.7)

#Box Plot: salário
boxplot(dados$salario)
#Versão formatada do gráfico
boxplot(dados$salario, 
        pch = "*",  # tipo de marcador dos outliers
        col = "darkturquoise", # cor do preenchimento do box plot
        border = "darkgrey", # cor da linha do box plot
        main = "Box plot: Salários Mínimo")
#Vesão ggplot()
library(tidyverse) #para usar ggplot()
ggplot(dados) + geom_boxplot(aes(y = salario), fill = "darkturquoise")

#Medidas resumo: min, Q1, Q2, média, Q3 e max - salário
summary(dados$salario)
mean(dados$salario)
median(dados$salario)
quantile(dados$salario, probs = 0.5)
var(dados$salario)
sd(dados$salario)
quantile(dados$salario, probs = c(0.01,0.25,0.5,0.75,0.99))

#Histograma: salário
hist(dados$salario)
#Versão formatada do gráfico
hist(dados$salario, xlab = "salário mínimo", ylab = "Frequencia Absoluta", 
     col = "darkturquoise", # cor do preenchimento do box plot
     border = "darkgrey", # cor da linha 
     main = "Histograma: Salários Mínimo")
#Versão ggplot()
ggplot(dados) + geom_histogram(aes(x = salario), bins = 10, 
                               fill = "darkturquoise", colour = "white")

#Saída gráfica: medidas resumo em tabela e gráficos
library(summarytools)
summarytools::dfSummary(dados) %>%
  summarytools::view()
#Para uma variável apenas
summarytools::dfSummary(dados$idade_anos) %>%
  summarytools::view()

#Coeficiente de assimetria no R: salário
library(moments)
skewness(dados$salario)

#Identificar indivíduos com salário maior ou igual a 15, c(1,5) seleciona colula 1 e 5
salario_alto <- as.data.frame(dados[dados$salario >= 15, c(1,5)])
summary(salario_alto[,2]) #mostra a apenas os valores da coluna 2

#Análise de missing
summary(dados$n_filhos)
mean(dados$n_filhos) #quando existe missing, não calcula a média
mean(dados$n_filhos, na.rm = TRUE) #na.rm=TRUE remove os missings e calcula a média
median(dados$n_filhos, na.rm = TRUE) #mediana
quantile(dados$n_filhos, probs = 0.5,na.rm = TRUE) #quantil (nao quartil)
var(dados$n_filhos,na.rm = TRUE) #variancia
sd(dados$n_filhos,na.rm = TRUE) #desvio padrao
quantile(dados$n_filhos, probs = c(0.01,0.25,0.5,0.75,0.99),na.rm = TRUE) #mais de um quartil

#####################################
## Análise Exploratória Bivariada  ##
#####################################
# Tabela bidimensional: VALOR RELATIVO 100% da coluna = parâmetro 2, se quisesse 100% na linha, usar 1.
(tabela_perc = 100*prop.table(table(dados$grau_instrucao, dados$estado_civil), 2))
#1 linhas
# Gráfico de barras empilhadas: VALOR RELATIVO 100% da coluna = parâmetro 2, se quisesse 100% na linha, usar 1.
p <- barplot(tabela_perc)
#Versão Formatada
p = barplot(tabela_perc, col = c("cyan1","azure2", "darkturquoise")) 


#Escreve os valores no gráfico
text(p, tabela_perc[1,]/2, labels=paste0(round(tabela_perc[1,],2),"%"), col="black")
text(p, tabela_perc[1,]+tabela_perc[2,]/2, labels=paste0(round(tabela_perc[2,],2),"%"), col="black")
text(p, tabela_perc[1,]+tabela_perc[2,]+tabela_perc[3,]/2, labels=paste0(round(tabela_perc[3,],2),"%"), col="white")
#Insere a legenda no gráfico
legend("topright", legend = c("Fund","Med", "Sup"),  fill = c("cyan1", "azure2", "darkturquoise"),cex = 0.5)

#Alternativa, usando CrossTable da library(descr)
library(descr)   
CrossTable(dados$estado_civil,dados$grau_instrucao,
           prop.r=TRUE,    # Se TRUE, entao retorna as proporções nas linhas
           prop.c=FALSE,    # Se TRUE, entao retorna as proporções nas colunas
           prop.t=FALSE,    # Se TRUE, entao retorna as proporções em relação ao total
           prop.chisq=FALSE # Se TRUE, entao retorna a contribuição de cada casela para a estatística de Qui-quadrado
)
#Alternativa, usando ctable da library(summarytools)
library(summarytools)
ctable(dados$grau_instrucao, dados$estado_civil, prop = "r")

#Quanti x Quali: salário x grau de instrução
#Box plot de grau de instrução por salário
ggplot(dados,aes(grau_instrucao,salario)) + geom_boxplot(fill = "darkturquoise", colour = "grey")

#Medidas resumo do Box Plot anterior
#Análise de salário para grupo '1-ensino fundamental' 
dados_aux <- dados[dados$grau_instrucao =='1-ensino fundamental',]
summary(dados_aux$salario)
round(quantile(dados_aux$salario, probs=c(0.01,0.25,0.5,0.75,0.99)),2)
#Análise de salário para grupo '2-ensino medio' 
dados_aux <- dados[dados$grau_instrucao =='2-ensino medio',]
summary(dados_aux$salario)
round(quantile(dados_aux$salario, probs=c(0.01,0.25,0.5,0.75,0.99)),2)
#Análise de salário para grupo '3-superior' 
dados_aux <- dados[dados$grau_instrucao =='3-superior',]
summary(dados_aux$salario)
round(quantile(dados_aux$salario, probs=c(0.01,0.25,0.5,0.75,0.99)),2)
#Versão alternativa usando skim 
library(skimr)
skim(group_by(dados, grau_instrucao), salario)
skim(group_by(dados, grau_instrucao)) #Cruza com as quantitativas da base

#Quanti x Quanti: Salário x Idade
#Gráfico de dispesão
plot(dados$salario,dados$idade_anos)
#Versão formatada do gráfico
plot(dados$salario,dados$idade_anos,pch=20, col="darkturquoise",
     xlab="Sários Mínimos",ylab="Idade", main = 'Salário x Idade', cex=1)
#Versão ggplot
ggplot(dados,aes(idade_anos,salario)) + geom_point(aes(x = idade_anos, y = salario),
                                                   colour = "darkturquoise", size = 2)


