library(tidyverse)

#Leitura da base de dados
credit = read.table("00-Bases/CreditScore_r.txt", header = TRUE, sep = "\t", dec = ".")
names(credit)
str(credit)
nrow(credit)

#(a) Faça a análise exploratória univariada e interprete todas as variáveis do banco de dados. Interprete os resultados na visão do negócio.
#Frequência de todas as variáveis, exceto a variável chave
summary(credit[,-1])

#(b)Faça uma análise do % de default. 
#Percentual da variável resposta
round(prop.table(table(credit$RESPOSTA)),3)

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

#(d)Rode o modelo de Regressão Logística. 
#Selecione um modelo final no qual a interpretação dos parâmetros 
#esteja de acordo com a análise bivariada.
modelo_full <- glm(RESPOSTA ~ FX_IDADE + CEP_GRUPO_RISCO + FX_RENDA +  
                     INDICADOR_RESTRITIVO + QTDE_CONSULTAS_CREDITO, family = binomial(link = "logit"), data = credit)
summary(modelo_full)


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

#Modelo sem renda
modelo_red1 <- glm(RESPOSTA ~ FX_IDADE + CEP_GRUPO_RISCO +   
                     INDICADOR_RESTRITIVO + QTDE_CONSULTAS_CREDITO, family = binomial(link = "logit"), data = credit)
summary(modelo_red1)

#(g)Analise a sensibilidade, especificidade e acurácia pela tabela de 
#classificação. 

#Para o modelo logístico, com a função 'predict', tendo como parâmetro type = 'response' conseguimos obter as probabilidades do modelo para a classificação '1'
credit$prob_reglog <- predict(modelo_red1,newdata = credit,type = "response")
summary(credit$prob_reglog)
#Cria variável resposta predita com base na probabilidade predita pela Árvore de Decisão
credit$resp_bin_reglog <- as.factor(ifelse(credit$prob_reglog >= 0.1206993,1,0)) #transforma a probabilidade em variável binária

#Mostra a tabela de desempenho: Predito x Resposta observada
(tabela_desempenho<-table(credit$RESPOSTA,credit$resp_bin_reglog))

#Calcula as medidas de desempenho: Sensibilidade, Especificidade e Acurácia
(sensibilidade<-tabela_desempenho[2,2]/sum(tabela_desempenho[2,]))
(especificidade<-tabela_desempenho[1,1]/sum(tabela_desempenho[1,]))
(n<-nrow(credit))
(accuracia<- sum(tabela_desempenho[1,1]+tabela_desempenho[2,2])/n)



# Árvore de decisão -------------------------------------------------------

library(partykit)
library(CHAID)

# Transformando a variável resposta em fator
credit$RESPOSTA_ARVORE <- as.factor(credit$RESPOSTA)
credit$FX_IDADE <- as.factor(credit$FX_IDADE)
credit$CEP_GRUPO_RISCO <- as.factor(credit$CEP_GRUPO_RISCO)
credit$QTDE_CONSULTAS_CREDITO <- as.factor(credit$QTDE_CONSULTAS_CREDITO)
credit$INDICADOR_RESTRITIVO <- as.factor(credit$INDICADOR_RESTRITIVO)

#Para a árvore não ficar muito grande, a dois níveis
controle <- chaid_control(maxheight = 4)

#Função 'chaid' nos permite criar uma árvore de decisão de acordo com o algoritmo CHAID
(arvore <- chaid(RESPOSTA_ARVORE ~ FX_IDADE + CEP_GRUPO_RISCO +   
                   INDICADOR_RESTRITIVO + QTDE_CONSULTAS_CREDITO, 
                 data = credit, control=controle)) #indicando em qual base o modelo deve ser estimado
plot(arvore,gp = gpar(cex=0.6),type='simple')

#Vamos avaliar a propensão dos nós finais
credit$arvore_node <- predict(arvore, type = "node")
credit$arvore_prob <- predict(arvore, type = "prob")[,2]
credit$resp_bin_arvore <- as.factor(ifelse(credit$arvore_prob >= 0.1206993,1,0))


#Mostra a tabela de desempenho: Predito x Resposta observada
(tabela_desempenho_arvore<-table(credit$RESPOSTA,credit$resp_bin_arvore))

#Calcula as medidas de desempenho: Sensibilidade, Especificidade e Acurácia
(sensibilidade<-tabela_desempenho_arvore[2,2]/sum(tabela_desempenho_arvore[2,]))
(especificidade<-tabela_desempenho_arvore[1,1]/sum(tabela_desempenho_arvore[1,]))
(n<-nrow(credit))
(accuracia<- sum(tabela_desempenho_arvore[1,1]+tabela_desempenho_arvore[2,2])/n)

# Olhando as variáveis criadas
credit %>% 
  select(CLIENTE_NUMBER,
         FX_IDADE,
         CEP_GRUPO_RISCO, 
         INDICADOR_RESTRITIVO,
         QTDE_CONSULTAS_CREDITO,
         prob_reglog,
         resp_bin_reglog,
         arvore_node,
         arvore_prob,
         resp_bin_arvore) %>% View()
