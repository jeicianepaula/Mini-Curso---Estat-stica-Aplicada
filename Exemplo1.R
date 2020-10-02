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

#carta para variavel (Continuous data)

data("pistonrings")
?pistonrings
View(pistonrings)

attach(pistonrings) #Fixa o conjunto de dados na memória 
#detach(pistonrings) # tira o conjunto de dados na memória 

#Ajustando a tabela para construcao do grafico XR
diametro = qcc.groups(diameter, sample) 
View(diametro)

#######################################
##grafico da média dos valores calibrados e criação dos limites
X_bar = qcc(diametro[1:25,], type = "xbar")
View(X_bar)
X_bar$limits

#observe que a média dos grupos das amostras é o valor correspondente ao valor central
mean(diametro[1:25, ])

#fase I e II
X_bar_fI_fII = qcc(diametro[1:25,], type = "xbar", newdata = diametro[26:40,])

#carta X_barra só pra fase 2
X_bar_fI_fII = qcc(diametro[1:25,], type = "xbar", newdata = diametro[26:40,],plot=FALSE)
X_bar_fII = plot(X_bar_fI_fII, chart.all = FALSE) 


#
#grafico considerando os limites de alerta para ou outro 99,73 = 6sigmas% de confiança sigmas (altera os limites)
qcc(diametro[1:25,], type = "xbar", newdata = diametro[26:40,], confidence.level = 0.9973)

#acrecentado limites de alertas considerando 1 desvio padrão (68%) --> 2 sigmas;  e 2 desvios padrao (95%) --> 4 sigmas para fases 1 e 2
warn.limits1 = limits.xbar(X_bar_fI_fII$center, X_bar_fI_fII$std.dev, X_bar_fI_fII$sizes, 1)
warn.limits2 = limits.xbar(X_bar_fI_fII$center, X_bar_fI_fII$std.dev, X_bar_fI_fII$sizes, 2)
warn.limits3 = limits.xbar(X_bar_fI_fII$center, X_bar_fI_fII$std.dev, X_bar_fI_fII$sizes, 3)


plot(X_bar_fI_fII, restore.par = FALSE)
abline(h = warn.limits1, lty = 3, col = "chocolate") #1 desvio pra cada lado -> 2 sigmas (68%)
abline(h = warn.limits2, lty = 3, col = "royalblue") #2 desvios pra cada lado --> 4 sigmas (95%)
abline(h = warn.limits3, lty = 3, col = "green") #3 desvios pra cada lado --> 6 sigmas (99,73%)

#grafico considerando os limites de alerta para 2 sigmas (68%) (altera os limites)
qcc(diametro[1:25,], type = "xbar", newdata = diametro[26:40,], nsigmas = 1)

#grafico considerando os limites de alerta para 4 sigmas (95%) (altera os limites)
qcc(diametro[1:25,], type = "xbar", newdata = diametro[26:40,], nsigmas = 2)



###################################################
# Gráfico da amplitude

#Fase 1 - Calibração
grafico_R_f1 = qcc(diametro[1:25,], type = "R") 

# Fase 1 e 2
grafico_R_f1_f2 =qcc(diametro[1:25,], type = "R", newdata = diametro[26:40,])

#Fase 2
#carta X_barra só pra fase 2
grafico_R_f1_f2 =qcc(diametro[1:25,], type = "R", newdata = diametro[26:40,], plot=FALSE)
grafico_R_f2 = plot(grafico_R_f1_f2, chart.all = FALSE) 

####################
#variable control limits intervalos com amostras de tamnhos diferentes

out = c(9,10,30,35,45,64,65,74,75,85,99,100)
diametro = qcc.groups(pistonrings$diameter[-out], sample[-out])
View(diametro)

qcc(diametro[1:25,], type = "xbar")
qcc(diametro[1:25,], type = "xbar", newdata = diametro[26:40,])



#Tipos de cartas
qcc(diametro[1:25,], type = "xbar")
qcc(diametro[1:25,], type = "R")
qcc(diametro[1:25,], type = "S")

detach(pistonrings) # tira o conjunto de dados na memória 
