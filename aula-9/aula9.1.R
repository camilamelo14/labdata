#**********************************************************
#ANÁLISE DE CLUSTER - CARTÃO DE CRÉDITO
#**********************************************************

#**********************************************************
#Mapear diretório de trabalho
getwd()
#Atenção: Alterar Diretório
setwd("C:\\Users\\Camila Melo\\Documents\\Estudos\\LABData Fia\\aula9")
#**********************************************************
#Retirar notação científica
options(scipen=999)

#Leitura da Base de cartao
library(readxl)
cartao = read_excel("Cartao_Credito_BusinessCase.xlsx", sheet="Base de Dados")

#Estutura da base de dados
nrow(cartao)
ncol(cartao)
str(cartao)

#(a)Realize a análise exploratória univariada. 
#Calcule as medidas resumos e construa boxplots e histogramas para todas as variáveis. 
#Analise os resultados.

summary(cartao[,-1])
apply(cartao[,-1] , 2 , sd, na.rm = TRUE ) #Desvio Padrão

par(mfrow=c(2,6)) #Matriz de gráficos
#Boxplot
boxplot(cartao$LIMITE_DISP_T0, col="darkturquoise", main="LIMITE_DISP_T0")
boxplot(cartao$LIMITE_TOTAL_T0, col="darkturquoise", main="LIMITE_TOTAL_T0")
boxplot(cartao$PERC_USO_LIMITE_T0, col="darkturquoise", main="PERC_USO_LIMITE_T0")
boxplot(cartao$PERC_FAT_CARTAO_12M, col="darkturquoise", main="PERC_USO_CARTAO_12M")
boxplot(cartao$QTDE_TRANSACAO_T0, col="darkturquoise", main="QTDE_TRANSACAO_T0")
boxplot(cartao$VALOR_FATURA_T0, col="darkturquoise", main="VALOR_FATURA_T0")
#Histograma
hist(cartao$LIMITE_DISP_T0, col="darkturquoise", main="LIMITE_DISP_T0")
hist(cartao$LIMITE_TOTAL_T0, col="darkturquoise", main="LIMITE_TOTAL_T0")
hist(cartao$PERC_USO_LIMITE_T0, col="darkturquoise", main="PERC_USO_LIMITE_T0")
hist(cartao$PERC_FAT_CARTAO_12M, col="darkturquoise", main="PERC_USO_CARTAO_12M")
hist(cartao$QTDE_TRANSACAO_T0, col="darkturquoise", main="QTDE_TRANSACAO_T0")
hist(cartao$VALOR_FATURA_T0, col="darkturquoise", main="VALOR_FATURA_T0")

#(b) Avalie a presença e quantifique os missings para cada variável. 
#Seria possível tratar os missings?  
#Se sim, trate-os segundo o contexto do negócio.

#Os missings poderiam ser substituídos por ZERO uma vez que o sistema não registra
#quando não há transações ou fatura de cartão de crédito:
cartao$QTDE_TRANSACAO_T0 <- ifelse(is.na(cartao$QTDE_TRANSACAO_T0), 0,cartao$QTDE_TRANSACAO_T0)
cartao$VALOR_FATURA_T0 <- ifelse(is.na(cartao$VALOR_FATURA_T0), 0,cartao$VALOR_FATURA_T0)

#Analisando novamente as descritivas da base com o tratamento de missings
summary(cartao[,-1])
apply(cartao[,-1] , 2 , sd) #Desvio Padrão

par(mfrow=c(2,6)) #Matriz de gráficos
#Boxplot
boxplot(cartao$LIMITE_DISP_T0, col="darkturquoise", main="LIMITE_DISP_T0")
boxplot(cartao$LIMITE_TOTAL_T0, col="darkturquoise", main="LIMITE_TOTAL_T0")
boxplot(cartao$PERC_USO_LIMITE_T0, col="darkturquoise", main="PERC_USO_LIMITE_T0")
boxplot(cartao$PERC_FAT_CARTAO_12M, col="darkturquoise", main="PERC_USO_CARTAO_12M")
boxplot(cartao$QTDE_TRANSACAO_T0, col="darkturquoise", main="QTDE_TRANSACAO_T0")
boxplot(cartao$VALOR_FATURA_T0, col="darkturquoise", main="VALOR_FATURA_T0")
#Histograma
hist(cartao$LIMITE_DISP_T0, col="darkturquoise", main="LIMITE_DISP_T0")
hist(cartao$LIMITE_TOTAL_T0, col="darkturquoise", main="LIMITE_TOTAL_T0")
hist(cartao$PERC_USO_LIMITE_T0, col="darkturquoise", main="PERC_USO_LIMITE_T0")
hist(cartao$PERC_FAT_CARTAO_12M, col="darkturquoise", main="PERC_USO_CARTAO_12M")
hist(cartao$QTDE_TRANSACAO_T0, col="darkturquoise", main="QTDE_TRANSACAO_T0")
hist(cartao$VALOR_FATURA_T0, col="darkturquoise", main="VALOR_FATURA_T0")

#(c)Avalie a presença e quantifique os outliers para cada variável. 
#Seria possível tratar os outliers?  
#Se sim, trate-os segundo o contexto do negócio.

#Percentil 1 e 99
round(apply(cartao[,-1] , 2 , quantile , probs = c(0.01,0.99)),2)

#Substituindo por P1
cartao$LIMITE_DISP_T0 <- ifelse(cartao$LIMITE_DISP_T0 < quantile(cartao$LIMITE_DISP_T0,.01), quantile(cartao$LIMITE_DISP_T0,.01),cartao$LIMITE_DISP_T0)
cartao$LIMITE_TOTAL_T0 <- ifelse(cartao$LIMITE_TOTAL_T0 < quantile(cartao$LIMITE_TOTAL_T0,.01), quantile(cartao$LIMITE_TOTAL_T0,.01),cartao$LIMITE_TOTAL_T0)
cartao$PERC_USO_LIMITE_T0 <- ifelse(cartao$PERC_USO_LIMITE_T0 < quantile(cartao$PERC_USO_LIMITE_T0,.01), quantile(cartao$PERC_USO_LIMITE_T0,.01),cartao$PERC_USO_LIMITE_T0)
cartao$PERC_FAT_CARTAO_12M <- ifelse(cartao$PERC_FAT_CARTAO_12M < quantile(cartao$PERC_FAT_CARTAO_12M,.01), quantile(cartao$PERC_FAT_CARTAO_12M,.01),cartao$PERC_FAT_CARTAO_12M)
cartao$QTDE_TRANSACAO_T0 <- ifelse(cartao$QTDE_TRANSACAO_T0 < quantile(cartao$QTDE_TRANSACAO_T0,.01), quantile(cartao$QTDE_TRANSACAO_T0,.01),cartao$QTDE_TRANSACAO_T0)
cartao$VALOR_FATURA_T0 <- ifelse(cartao$VALOR_FATURA_T0 < quantile(cartao$VALOR_FATURA_T0,.01), quantile(cartao$VALOR_FATURA_T0,.01),cartao$VALOR_FATURA_T0)

#Substituindo por P99
cartao$LIMITE_DISP_T0 <- ifelse(cartao$LIMITE_DISP_T0 > quantile(cartao$LIMITE_DISP_T0,.99), quantile(cartao$LIMITE_DISP_T0,.99),cartao$LIMITE_DISP_T0)
cartao$LIMITE_TOTAL_T0 <- ifelse(cartao$LIMITE_TOTAL_T0 > quantile(cartao$LIMITE_TOTAL_T0,.99), quantile(cartao$LIMITE_TOTAL_T0,.99),cartao$LIMITE_TOTAL_T0)
cartao$PERC_USO_LIMITE_T0 <- ifelse(cartao$PERC_USO_LIMITE_T0 > quantile(cartao$PERC_USO_LIMITE_T0,.99), quantile(cartao$PERC_USO_LIMITE_T0,.99),cartao$PERC_USO_LIMITE_T0)
cartao$PERC_FAT_CARTAO_12M <- ifelse(cartao$PERC_FAT_CARTAO_12M > quantile(cartao$PERC_FAT_CARTAO_12M,.99), quantile(cartao$PERC_FAT_CARTAO_12M,.99),cartao$PERC_FAT_CARTAO_12M)
cartao$QTDE_TRANSACAO_T0 <- ifelse(cartao$QTDE_TRANSACAO_T0 > quantile(cartao$QTDE_TRANSACAO_T0,.99), quantile(cartao$QTDE_TRANSACAO_T0,.99),cartao$QTDE_TRANSACAO_T0)
cartao$VALOR_FATURA_T0 <- ifelse(cartao$VALOR_FATURA_T0 > quantile(cartao$VALOR_FATURA_T0,.99), quantile(cartao$VALOR_FATURA_T0,.99),cartao$VALOR_FATURA_T0)

#Rever os boxplots e histogramas com valores extremos tratados
par(mfrow=c(2,6))

#Boxplot
boxplot(cartao$LIMITE_DISP_T0, col="darkturquoise", main="LIMITE_DISP_T0")
boxplot(cartao$LIMITE_TOTAL_T0, col="darkturquoise", main="LIMITE_TOTAL_T0")
boxplot(cartao$PERC_USO_LIMITE_T0, col="darkturquoise", main="PERC_USO_LIMITE_T0")
boxplot(cartao$PERC_FAT_CARTAO_12M, col="darkturquoise", main="PERC_FAT_CARTAO_12M")
boxplot(cartao$QTDE_TRANSACAO_T0, col="darkturquoise", main="QTDE_TRANSACAO_T0")
boxplot(cartao$VALOR_FATURA_T0, col="darkturquoise", main="VALOR_FATURA_T0")
#Histograma
hist(cartao$LIMITE_DISP_T0, col="darkturquoise", main="LIMITE_DISP_T0")
hist(cartao$LIMITE_TOTAL_T0, col="darkturquoise", main="LIMITE_TOTAL_T0")
hist(cartao$PERC_USO_LIMITE_T0, col="darkturquoise", main="PERC_USO_LIMITE_T0")
hist(cartao$PERC_FAT_CARTAO_12M, col="darkturquoise", main="PERC_FAT_CARTAO_12M")
hist(cartao$QTDE_TRANSACAO_T0, col="darkturquoise", main="QTDE_TRANSACAO_T0")
hist(cartao$VALOR_FATURA_T0, col="darkturquoise", main="VALOR_FATURA_T0")

#**********************************************************
#(d)Para a base "tratada" nos itens (b) e (c), realize a análise exploratória univariada 
#novamente. Calcule as medidas resumos e construa boxplots e histogramas para todas 
#as variáveis. Analise os resultados.

#**********************************************************
#(e)Avalie a correlação entre as variáveis por meio Correlação de Pearson. 
#Discuta a relação entre as variáveis e decida quais variáveis serão utilizadas 
#para agrupar os clientes. 
cor(cartao[,-1])

#Escolha das variáveis: "PERC_USO_LIMITE_T0","QTDE_TRANSACAO_T0","VALOR_FATURA_T0"

#**********************************************************
#(f)Realize a padronização das variáveis.

#Avaliar variabilidade dos dados
round(apply(cartao[,-1],2,sd),2)

#Aplicar padronização Z-Score das variáveis (Z-Score: (x-média)/dp)
aux_padr <- as.data.frame(apply(cartao[,-1],2,scale))
cartao_padr <- cbind(cartao[,"Cod_Cliente"],aux_padr)
colnames(cartao_padr)[1] <- "Cod_Cliente"

#Verificar média e desvio-padrão das variáveis padronizadas: (0,1)
round(apply(cartao_padr[,-1],2,mean),2)
round(apply(cartao_padr[,-1],2,sd),2)

#Mantendo apenas as variáveis de interesse:
cartao_padr <- cartao_padr[,c("Cod_Cliente","PERC_USO_LIMITE_T0",
                              "QTDE_TRANSACAO_T0","VALOR_FATURA_T0")]

#**********************************************************
#(g)Dado as características da base de dados, qual método de agrupamento você adotaria?
#Discuta com a sala.

#**********************************************************
#(f) Realize a análise de agrupamento com os 5 métodos hierárquicos, 
#selecionando aleatoriamente 1.000 observações, pela análise do dendrograma, 
#escolha um dos métodos e defina a quantidade de grupos.

#(Utilize os comando do R: cartao_h <- sample_n(cartao_padr, 1000), 
#sendo cartão_h o objeto que guardará a base amostral.
library(dplyr) #biblioteca para tirar amostra aleatória usando sample_n
set.seed(5) #para garantir sempre a mesma amostra 
cartao_h <- sample_n(cartao_padr, 1000) #amostra de 1000 casos

#Cálculo da distância entre os elementos: comparação entre os métodos
distancia <- dist(cartao_h[,-1], method="euclidean") #Cálculo das distâncias euclidianas

par(mfrow=c(1,2))
clust_h <- hclust(distancia, method="single") #Criação do cluster
plot(clust_h, main="Single", hang=-1,labels=FALSE) #Plot do Dendograma - representação visual do agrupamento
clust_h <- hclust(distancia, method="complete") #Criação do cluster
plot(clust_h, main="Complete", hang=-1,labels=FALSE) #Plot do Dendograma - representação visual do agrupamento

#Propondo 3 grupos para o cluster criado a partir do método escolhido
rect.hclust(clust_h, k=5, border=1:5) #Plot do Dendograma, com auxílio visual a partir da indicação do número de grupos

#(i)Realize a análise de agrupamento pelo método hierárquico K-médias e defina 
#a quantidade de grupos.
set.seed(4)
library(factoextra)
#Método Elbow (Critério: Soma de quadrados dentro)
fviz_nbclust(cartao_h[,-1], kmeans, method = "wss")

#(j)Compare a quantidade de grupos encontrados pelos métodos hierárquicos e K-médias.

#(k)Considerando que a base de dados é "grande", realize o agrupamento dos clientes 
#pelo K-médias utilizando k definido no item (j).
#Dado a escolha de k=5, realizar o K-médias
set.seed(5)
clust_km <- kmeans(cartao_padr[,-1], 3) #considerando o número de clusters igual a 4

# Gráfico dos clusters
fviz_cluster(clust_km , data = cartao_padr[,-1])

#Marcar toda as observações com os clusters gerados
#cartao$cartao_kmeans_agrup <- cbind(cartao, cluster_km = as.factor(clust_km$cluster))
cartao$cluster_kmeans <- as.factor(clust_km$cluster)

#Tamanho dos Clusters 
clust_km$size 
round(prop.table(clust_km$size),2)

#(l)Descreva as personas e justifique para área de negócios porque o agrupamento
#formado é adequado para implementar estratégias de atendimento e relacionamento 
#diferenciados.

#Matriz de boxplots
library(tidyverse)
library(ggplot2)
#Faz BoxPlot para cada variável ORIGINAL e compara por cluster
#Distribuição das variáveis por cluster: 
cartao[,c(-1,-2,-3,-5)] %>% #só com as variáveis do cluster
  gather(var, valor, -cluster_kmeans) %>% 
  ggplot(aes(x = cluster_kmeans, y = valor, fill = var)) +
  geom_boxplot() +
  facet_wrap(~var, scales = "free") +
  theme(legend.position = "none")

#Descritiva com todas as variáveis
cartao[,-1] %>% 
  gather(var, valor, -cluster_kmeans) %>% 
  ggplot(aes(x = cluster_kmeans, y = valor, fill = var)) +
  geom_boxplot() +
  facet_wrap(~var, scales = "free") +
  theme(legend.position = "none")

