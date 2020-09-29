#Chama o pacote qcc usado para controle estatistico de processos
library(qcc)


#GRÁFICO DE CONTROLE PARA VARIÁVEL-------------------------------------------------------------------------------------------------------------------
#Os anéis de pistão de um motor automotivo são produzidos por um processo de forjamento.
#O diâmetro interno dos anéis fabricados pelo processo é medido em 25 amostras, cada uma
#de tamanho 5, para a fase de controle I, quando amostras preliminares de um
#processo sendo considerado 'em controle' são utilizadas para construir gráficos de controle.
# Em seguida, mais 15 amostras, novamente cada uma de tamanho 5, são obtidas para a fase II.

####Usando a base de dados pistonrings disponivel no pacote qcc
data("pistonrings")
attach(pistonrings) #Fixa o conjunto de dados na moméria 
?pistonrings

#Ajustando a tabela para construcao do grafico XR
d = qcc.groups(diameter, sample) #d=diametro

#Gerando gráfico com dados da Fase1 com 25 primeiras e acrescentando dados da Fase 2 com 15 amostras 
grafico_Xbar =qcc(d[1:25,], type = "xbar", newdata = d[26:40,])
grafico_R =qcc(d[1:25,], type = "R", newdata = d[26:40,])

#Separando os 2 graficos
par(mfrow=c(2,1))
plot(grafico_Xbar, restore =F)
plot(grafico_R, chart.all =F)

# Colocando um aviso quando ultrapassar 2 desvios padrao (média)
qcc(d2[1:25,], type="xbar", newdata=d2[26:40,], plot=FALSE)
(warn.limits <- limits.xbar(q$center, q$std.dev, q$sizes, 2))
plot(q, restore.par = FALSE)
abline(h = warn.limits, lty = 3, col = "chocolate")

#Colocando o numero de sigmas pra 2 Média
qcc(d2[1:25,], type = "xbar", newdata = d2[26:40,], nsigmas = 2)

#Colocando o numero de sigmas pra 2 Amplitude
qcc(d2[1:25,], type = "R", newdata = d2[26:40,], nsigmas = 2)

#Aumentando o intervalo de confianca para 99%
qcc(d[1:25,], type = "xbar", newdata = d[26:40,], confidence.level = 0.99)

detach(pistonrings)

#GRÁFICO DE CONTROLE PARA ATRIBUTO-------------------------------------------------------------------------------------------------------------------

#Em uma fábrica de acabamento têxtil, o tecido tingido é inspecionado para o
#ocorrência de defeitos por 50 metros quadrados. Os dados em dez

data("dyedcloth")
attach(dyedcloth) #Fixa o conjunto de dados na moméria 
?dyedcloth
qcc(x, sizes=size, type="u")
detach(pcmanufact)

#Capabilidade do Processo-------------------------------------------------------------------------------------------------------------------
data("pistonrings")
attach(pistonrings)#Fixa o conjunto de dados na moméria

#Ajustando a tabela para construcao do grafico XR
d = qcc.groups(diameter, sample) #d=diametro

capacidade = qcc(d[1:25,], type="xbar", nsigmas=3, plot=f)
process.capability(capacidade, spec.limits=c(73.95,74.05))

#Medindo a curva 
beta <- oc.curves.xbar(qcc(diametro, type="xbar", nsigmas=3, plot=FALSE))

detach(pistonrings)



