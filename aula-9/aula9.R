#**********************************************************
#ANÁLISE DE CLUSTER - ENTREGAS
#**********************************************************

#**********************************************************
#Atenção: Alterar Diretório
setwd("C:\\Users\\Camila Melo\\Documents\\Estudos\\LABData Fia\\aula9")

#Retirar notação científica no R
options(scipen = 999)
#**********************************************************

#**********************************************************
#Leitura da Base 
library(readxl)
entregas <- read_excel("Servico_Entregas-Complemento.xlsx", sheet = 'Base de Dados')
nrow(entregas)
ncol(entregas)
#**********************************************************


#**********************************************************
# Faça uma análise exploratória da base de dados 
# (obtenha as medidas de posição e dispersão). 
summary(entregas[,-1]) #Min, Q1, Q2, Q3 e Max
apply(entregas[,-1] , 2 , sd) #Desvio Padrão

#**********************************************************
#Padronize as variáveis.
entregas_z<-scale(entregas[,-1])

set.seed(123)
library(factoextra)
#Método Elbow (Critério: Soma de quadrados dentro)
fviz_nbclust(entregas_z, kmeans, method = "wss")

set.seed(123)
clust_km <- kmeans(entregas_z, 3) #considerando o número de clusters igual a 4 ou 5
clust_km

# Gráfico dos clusters
fviz_cluster(clust_km , data = entregas_z)
