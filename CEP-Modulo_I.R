#Instalando as e ativando as biblioteca 
install.packages("qcc")
install.packages("nortest")
install.packages("ggthemes")
library(nortest)
library(qcc)
library(ggplot2)
library(ggthemes)
library(xlsx)

#Os anéis de pistão de um motor automotivo são produzidos por um processo de forjamento.
#O diâmetro interno dos anéis fabricados pelo processo é medido em 25 amostras, cada uma
#de tamanho 5, para a fase de controle I, quando amostras preliminares de um
#processo sendo considerado 'em controle' são utilizadas para construir gráficos de controle.
# Em seguida, mais 15 amostras, novamente cada uma de tamanho 5, são obtidas para a fase II.

#Definindo as Variáveis
data(pistonrings)
dados= pistonrings
write.xlsx(dados, "cep.xlsx")
wdados
attach(pistonrings)
fase1= pistonrings[1:125,]
fase2 = pistonrings[125:200,]
?pistonrings
class()

codigo = c(1,2,3)
nome
nome= c("Jeiciane", "Samuel", "Victoria")
nome
matriz= matrix(c(1,2,3,"Jeiciane", "Samual", "Victoria"), nrow = 3, ncol = 2)
matriz
Data.frame = data.frame(codigo, nome)
Data.frame
#Medida de Posição e Dispersão
mean(pistonrings$diameter)
sd(pistonrings[1:125,]$diameter)
summary(pistonrings)
summary(fase1$diameter)

#HISTOGRAMA
#Histograma-Rbase---------------------------------------------------------------------------------------------------

hist(lote1$diameter, 
     main = "Histograma", #função base do R
     xlab = "Diametro", 
     col = "lightblue", 
     freq = F, 
     breaks = 5, ylim = c(0,40))
curve(dnorm(x,mean = mean(lote1$diameter), 
            sd = sd(lote1$diameter)), add = T)

#Histograma-ggplo2-por-lote---------------------------------------------------------------------------------------------------

ggplot(data = pistonrings[1:125,],aes(x= diameter)) + 
  geom_histogram(fill = 'turquoise2', 
                 bins = 30,
                 col= "black",
                 alpha = 0.5,
                 aes(y=..density..)) +                  #ggplot2 
  labs(title = "Histograma", y = "Densidade", x = "Diâmetro") +
  theme_classic(base_size = 18)+
  stat_function(fun= dnorm, 
                args = list(mean = mean(pistonrings[1:125,]$diameter), 
                            sd = sd(pistonrings[1:125,]$diameter)))

#Histograma-ggplo2-dois-lote----------------------------------------------------------------------------------------------------

ggplot(pistonrings, aes (x= diameter, fill = trial)) +
  geom_histogram(bins = 30, color = "white", 
                 position = "dodge") + 
  theme_classic(base_size = 18) + labs(title = "Histograma")+ 
  xlab("Diametro") + ylab("Frequência")


#BOXPLOT
#Boxplot -Rbase---------------------------------------------------------------------------------------

boxplot(diameter ~ sample, 
        main = "Boxplot", #função base do R
        xlab = "Amostra",
        ylab = "Diametro",
        breaks = 5) #Por Amostra

boxplot(diameter ~ trial, 
        main = "Boxplot", #função base do R
        ylab = "Diametro",
        breaks = 5,
        col= c("gold4","seagreen4")) #Por Fase

#Boxplot-ggplo2---------------------------------------------------------------------------------------

ggplot(pistonrings, aes (y=diameter, x= factor(sample),fill = trial))+ 
       xlab("Amostra") + ylab("Diametro")+
       geom_boxplot(show.legend = F, alpha = 0.5, outlier.color = "red")+
        theme_economist(base_size = 18)

ggplot(pistonrings, aes (x = factor(trial), y=diameter, fill = trial))+
      geom_boxplot() + geom_boxplot(show.legend = T, 
                                    alpha = 0.5, outlier.color = "red")+
                                      theme_excel_new (base_size = 18)


#NORMALIDADE
#Teste de normalidade---------------------------------------------------------------------------------------

#Inferência
Sh = shapiro.test(pistonrings$diameter) # Shapiro-Wilk
An = ad.test(pistonrings$diameter) # Anderson-Darling
print(Sh)
print(An)

# Tabela de resultados
testes = c(Sh$method, An$method)
estt = as.numeric(c(Sh$statistic, An$statistic))
valorp <- c(Sh$p.value, An$p.value)
resultados <- cbind(estt, valorp)
rownames(resultados) <- testes
colnames(resultados) <- c("Estatística", "p")
print(resultados, digits = 4)

#Gráfico de probabilidade(QQ)---------------------------------------------------------------------------------------

qqnorm(pistonrings$diameter, main = "", 
       xlab = "Quantis teóricos N(0,1)", pch = 20,
       ylab = "Diametro")
qqline(pistonrings$diameter, col = "red")

#Gráfico de densidade de probabilidade(QQ)---------------------------------------------------------------------------------------

ggplot(pistonrings, aes(x = diameter, fill = trial)) +
  geom_density(alpha = 0.5) +
  theme_clean(base_size = 18) +
  xlab("Diametro") + ylab("Densidade")

Dados2 = dyedcloth
write.xlsx(Dados2,"Cep2.xlsx")
