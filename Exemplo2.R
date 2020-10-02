#####Comando para limpar a memoria 
rm(list=ls())
gc(reset = TRUE)
####Fim da limpeza


#Chama o pacote qcc usado para controle estatistico de processos
library(qcc)
# para visualizar

View(qcc)
#socorro
?qcc

# cartas de controle para atributos
data("dyedcloth")
?dyedcloth
View(dyedcloth)

attach(dyedcloth) #Fixa o conjunto de dados na momória 
#detach(dyedcloth) # tira o conjunto de dados na memória 

carta_u = qcc(x, sizes=size, type="u")


##### ex2

data(circuit)
?circuit

attach(circuit)
View(circuit)
fase1 = qcc(x[trial], sizes = size[trial], type = "u")

fase1_2 = qcc(x[trial], sizes = size[trial], type = "u",newdata = x[!trial], newsizes = size[!trial], plot = FALSE)
  
fase2 = plot(fase1_2, chart.all = FALSE) 


inc = setdiff(which(trial), c(6,20))

fase1_fase2_novas = qcc(x[inc], sizes = size[inc], type = "u", labels = inc,
    newdata = x[!trial], newsizes = size[!trial], newlabels = which(!trial))

detach(circuit) # tira o conjunto de dados na memória 


