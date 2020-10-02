#####Comando para limpar a memoria 
rm(list=ls())
gc(reset = TRUE)
####Fim da limpeza


#Chama o pacote qcc usado para controle estatistico de processos
library(qcc)
# para visualizar

#Capabilidade do Processo-------------------------------------------------------------------------------------------------------------------
data("pistonrings")
attach(pistonrings)#Fixa o conjunto de dados na mom?ria

#Ajustando a tabela para construcao do grafico XR
diametro = qcc.groups(diameter, sample) #d=diametro
qcc(diametro[1:25,], type="xbar", nsigmas=3)

capacidade = qcc(diametro[1:25,], type="xbar", nsigmas=3, plot=FALSE)

capacidade$limits

process.capability(capacidade, spec.limits=c(73.95,74.05)) #limite de especificação inferior (LSL) e limite de especificação superior (USL)

?process.capability
