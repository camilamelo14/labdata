#**********************************************************
#�RVORE DE DECIS�O - TELEFONIA FIXA
#**********************************************************

getwd()
setwd("C:\\Users\\Camila Melo\\Documents\\Estudos\\LABData Fia\\aula6")

##################### CONFIGURACAO #######################

library(gtools) #pacote com a fun��o 'quantcut'
#Fun��o 'chaid' nos permite criar uma �rvore de decis�o de acordo com o algoritmo CHAID
library(partykit)#pacote precisa ser instalado previamente para usar o CHAID
#install.packages("CHAID", repos="http://R-Forge.R-project.org")
library(CHAID) #pacote com a fun��o 'chaid'

##########################################################

#(a) Utilize o banco de dados 'base_telefonia_reduzida.sav'.
telefonia <- read.table("Telefonia_AD.txt", header = TRUE, sep = "\t", dec = ".")

#(b) Fa�a a an�lise explorat�ria  univariada das vari�veis 'Idade' at� 'Resposta'.
summary(telefonia[,-1]) 

#Tabela de frequ�ncia para a vari�vel 'resposta'
(resposta_a<-table(telefonia$resposta))
(resposta_p<-prop.table(resposta_a)*100)

prop_esperada = (resposta_p<-prop.table(resposta_a)*100)[2] # Observa o Evento 1

prop_esperada

telefonia$Minutos_realizados_T0 <- ifelse(is.na(telefonia$Minutos_realizados_T0),0,telefonia$Minutos_realizados_T0)

#Para rodar a �rvore pelo m�todo CHAID, a vari�vel resposta deve estar no formato fator (vari�vel qualitativa)
telefonia$resposta <- as.factor(telefonia$resposta)

#Segmentar a Idade em quartil
telefonia$Minutos_realizados_T0_q <- quantcut(telefonia$Minutos_realizados_T0,4)
telefonia$Tempo_casa_q <- quantcut(telefonia$Tempo_casa,4)
telefonia$Qtd_retencao_6meses_q <- quantcut(telefonia$Qtd_retencao_6meses,4)
telefonia$Qtd_prod_q <- quantcut(telefonia$Qtd_prod,4)

#Tabela Bidimensional: covari�vel x resposta
Minutos_table<-table(telefonia$Minutos_realizados_T0_q,telefonia$resposta)
Tempo_table<-table(telefonia$Tempo_casa_q,telefonia$resposta)
Qtd_retencao_table<-table(telefonia$Qtd_retencao_6meses_q,telefonia$resposta)
Qtd_prod_table<-table(telefonia$Qtd_prod_q,telefonia$resposta)

round(prop.table(Minutos_table,1)*100,2)
round(prop.table(Tempo_table,1)*100,2)
round(prop.table(Qtd_retencao_table,1)*100,2)
round(prop.table(Qtd_prod_table,1)*100,2)

#Teste Qui-quadrado
chisq.test(Minutos_table)
chisq.test(Tempo_table)
chisq.test(Qtd_retencao_table)
chisq.test(Qtd_prod_table)

#Para a �rvore n�o ficar muito grande, a tr�s n�veis
controle <- chaid_control(maxheight = 3)

#Fun��o 'chaid' nos permite criar uma �rvore de decis�o de acordo com o algoritmo CHAID
(arvore <- chaid(resposta ~ 
                   Minutos_realizados_T0_q +
                   Tempo_casa_q +
                   Qtd_retencao_6meses_q +
                   Qtd_prod_q, data = telefonia, control=controle)) #indicando em qual base o modelo deve ser estimado
plot(arvore,gp = gpar(col = "darkturquoise",cex=0.6),type='simple')

#Vamos avaliar a propens�o dos n�s finais
#atribui o n�mero do n� para uma vari�vel nova a cada indiv�duo
telefonia$node <- predict(arvore, type = "node")

#Frequ�ncia Absoluta dos n�s
table(telefonia$node)
round(prop.table(table(telefonia$node)),3)*100

#Cruza os n�s finais com a vari�vel resposta
pred_resp<-table(telefonia$node,telefonia$resposta)
round(prop.table(pred_resp,1)*100,2) 

#Incluir na base de dados a probabilidade predita pela �rvore de Decis�o
probs<- as.data.frame(predict(arvore, newdata = telefonia, type = "p")) #"p" salva o valor predito da probabilidade
summary(probs) 
names(probs) <- c("P_0","P_1")
telefonia <- cbind(telefonia,probs)#insere 2 colunas na base com as probabilidade preditas de 0 e 1

#Cria vari�vel resposta predita com base na probabilidade predita pela �rvore de Decis�o
telefonia$predict <- as.factor(ifelse(telefonia$P_1 >= 0.008685467,1,0)) 

#Mostra a tabela de desempenho: Predito x Resposta observada
(tabela_desempenho<-table(telefonia$resposta,telefonia$predict))
#(tabela_desempenho<-round(prop.table(tabela_desempenho)*100))

#Calcula as medidas de desempenho: Sensibilidade, Especificidade e Acur�cia
(sensibilidade<-tabela_desempenho[2,2]/sum(tabela_desempenho[2,]))
(especificidade<-tabela_desempenho[1,1]/sum(tabela_desempenho[1,]))
(n<-nrow(telefonia))
(accuracia<- sum(tabela_desempenho[1,1]+tabela_desempenho[2,2])/n)
