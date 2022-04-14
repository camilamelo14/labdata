#************************ CONFIGURACAO ********************
#Mapear diretorio de trabalho
getwd()
#Atenção: Alterar Diretorio
setwd("C:\\Users\\Camila Melo\\Documents\\Estudos\\LABData Fia\\aula5")

#Libs
library(GGally)
#******************* LEITURA DOS DADOS ********************

consumo <- read.table("Consumo_Alimentos.txt", header = TRUE)
nrow(consumo)
ncol(consumo)

#**********************************************************

#(b) Faça uma análise exploratória completa da base de dados. Comente sobre a variabilidade dos dados.

#Medidas resumo principais de todas as variávei, exceto a primeira, que é o pais

summary(consumo[,-1]) #Min, Q1, Q2, Q3 e Max
apply(consumo[,-1] , 2 , sd) #Desvio Padrão

#Descritiva em formato de tabela #Frequencia, Min, Max, Media, Desvio Padraõ, IIQ e CV
library(magrittr)
summarytools::dfSummary(consumo) %>% 
  summarytools::view()

#Matriz de boxplots
library(tidyverse)
library(ggplot2)
consumo %>%
  select(-Pais) %>% #Desconsiderando país e selecionar as demais variáveis 
  gather(var, valor) %>% 
  ggplot(aes(y = valor, fill = var)) +
  geom_boxplot() +
  facet_wrap(~var, scales = "free") + # "facet_wrap(~var, scales = "free") +" para deixar escala livre
  theme(legend.position = "none")

bp <- ggplot(consumo, aes(y=carboidratos, x= carne_branca)) + geom_boxplot()
ggplotly(bp)

#**********************************************************

#**********************************************************
#(c) Calcule a matriz de distâncias euclidianas entre os 25 países.

#Cálculo da distância euclidiana entre os elementos
distancia <- dist(consumo[,-1], method="euclidean") #Cálculo das distâncias euclidianas
distancia
#Vizualiação por mapa de calor, usando pacote factoextra
library(factoextra)
fviz_dist(distancia)

#(d) Faça a análise de agrupamento usando o método os 5 métodos 
#apresentados e sugira a quantidade de grupos após a análise do 
#Dendrograma, e comente seus aspectos.

clust_single <- hclust(distancia, method="single") 
fviz_dend(clust_single, main = "Método Single")
#fviz_dend(clust_single, k = 4, main = "Método Single")
#print(clust_single$height)

clust_complete <- hclust(distancia, method="complete")
fviz_dend(clust_complete, main = "Método Complete")
fviz_dend(clust_complete, k = 3, main = "Método Complete")
print(clust_complete$height)

#(e)Analise as características de cada grupo.
#Atribui a cada país o cluster a qual ele pertence pela variável cluster
consumo$cluster <- as.factor(cutree(clust_complete, k=3))

#Tamanho dos Clusters
table(consumo$cluster)

#Faz BoxPlot para cada variável e compara por cluster
#Distribuição das variáveis por cluster
consumo[,-1] %>% 
  gather(var, valor, -cluster) %>% 
  ggplot(aes(x = cluster, y = valor, fill = var)) +
  geom_boxplot() +
  facet_wrap(~var, scales = "free") +
  theme(legend.position = "none")

############################################################################
########################## PADRONIZACAO VARIAVEIS ########################## 
############################################################################ 

#(f)Padronize as variáveis pelo Z-escore, refaça o item (d) e compare os resultados.
consumo <- read.table("Consumo_Alimentos.txt", header = TRUE)

#O comando Scale padroniza os dados
#Padronizar variáveis para análise (Z-Score: (x-média)/dp)
#retira a primeira coluna no -1 @CamilaMelo
consumo_z<-as.data.frame(scale(consumo[,-1]))

#Retirar notação científica no R
options(scipen = 999)

#Verificar média e desvio-padrão das variáveis padronizadas: (0,1)
# O valor 2 aqui representa que é a media da coluna e não do registro @CamilaMelo
apply(consumo_z,2,mean)
apply(consumo_z,2,sd)

#Descritiva em formato de tabela #Frequencia, Min, Max, Media, Desvio Padraõ, IIQ e CV
summarytools::dfSummary(consumo_z) %>% 
  summarytools::view()

#Matriz de boxplots
library(tidyverse)
library(ggplot2)
# Matriz de grÃ¡ficos de dimensÃ£o 3 linhas x 3 colunas
consumo_z %>% 
  gather(var, valor) %>% 
  ggplot(aes(y = valor, fill = var)) +
  geom_boxplot() +
  facet_wrap(~var) +
  theme(legend.position = "none")

#Informa o ID do registro como nome do pais
rownames(consumo_z) <- consumo$Pais

#Calcule a matriz de distâncias euclidianas entre os 25 países.
#Cálculo da distância euclidiana entre os elementos
distancia_z <- dist(consumo_z, method="euclidean") #Cálculo das distâncias euclidianas
distancia_z
#Vizualiação por mapa de calor, usando pacote factoextra
library(factoextra)
fviz_dist(distancia_z)

#Faça a análise de agrupamento usando o método os 5 métodos 
#apresentados e sugira a quantidade de grupos após a análise do 
#Dendrograma, e comente seus aspectos.

clust_single_z <- hclust(distancia_z, method="single") 
fviz_dend(clust_single_z, main = "Método Single")
#fviz_dend(clust_single, k = 3, main = "Método Single")
print(clust_single_z$height)

clust_complete_z <- hclust(distancia_z, method="complete")
fviz_dend(clust_complete_z, main = "Método Complete")
fviz_dend(clust_complete_z, k = 3, main = "Método Complete")
#print(clust_complete$height)

#Analise as características de cada grupo.
#Atribui a cada país o cluster a qual ele pertence pela variável cluster
consumo$cluster_z <- as.factor(cutree(clust_complete_z, k=3))

#Tamanho dos Clusters
#O ID do grupo é encontrado aqui @CamilaMelo
table(consumo$cluster_z)

#Faz BoxPlot para cada variável e compara por cluster
#Distribuição das variáveis por cluster
consumo[,-1] %>% 
  gather(var, valor, -cluster_z) %>% 
  ggplot(aes(x = cluster_z, y = valor, fill = var)) +
  geom_boxplot() +
  facet_wrap(~var, scales = "free") +
  theme(legend.position = "none")

########################3 K-MEAS ########################

# Item g ------------------------------------------------------------------
# Rode o método K-médias, utilizando o k encontrado no item (f).
rm(list=ls())
consumo <- read.table("Consumo_Alimentos.txt", header = TRUE)
consumo_z <- as.data.frame(scale(consumo[,-1]))
row.names(consumo_z) <- consumo$Pais

set.seed(59) # Atribuir a semente para deixar o processo aleatório igual, todas as vezes que processarmos o programa
consumo_km <- kmeans(consumo_z, centers = 3) # Centers sao os k grupos 
consumo_km #Apresenta os centróides / médias

# Gráfico dos clusters
fviz_cluster(consumo_km, data = consumo_z)

# Marcar todas as observações com os clusters gerados
consumo$cluster_km <- as.factor(consumo_km$cluster)

# Tamanho dos Clusters
table(consumo$cluster_km)

#Faz BoxPlot para cada variável e compara por cluster
#Distribuição das variáveis por cluster
consumo[,-1] %>% 
  gather(var, valor, -cluster_km) %>% 
  ggplot(aes(x = cluster_km, y = valor, fill = var)) +
  geom_boxplot() +
  facet_wrap(~var, scales = "free") +
  theme(legend.position = "none")
