
# LEITURA DOS DADOS

library(readxl)
base <- read_excel("/home/willmeira/Documentos/Git/Disciplinas/06%20Semestre/CE225%20-%20Modelo%20Lineares%20Generalizados/Trabalho/Contagem/Base_simone.xlsx
")
summary(base)
str(base)
base<- na.omit(base)

# 2.2 BOXPLOT

x11()
par(mfrow=c(3,3))
boxplot(base$Obitos, xlab = '', ylab = '', main = 'Obitos ', las=1)
boxplot(base$Renda, xlab = '', ylab = '', main = 'Renda ', las=1)
boxplot(base$DensDem, xlab = '', ylab = '', main = 'Densidade Demografica ', las=1)
boxplot(base$Pop, xlab = '', ylab = '', main = 'Popula??o ', las=1)
boxplot(base$Alfab, xlab = '', ylab = '', main = 'Alfabetiza??o ', las=1)
boxplot(base$Frota, xlab = '', ylab = '', main = 'Frota ', las=1)
boxplot(base$PopUrb, xlab = '', ylab = '', main = 'Popula??o Urbana ', las=1)

#ou HISTOGRAMA

library(ggplot2)
library(gridExtra)

g1 <- ggplot(base, aes(x=DensDem)) + geom_histogram()+ xlab('Densidade Demogr?fica')+ ylab('')
g2 <- ggplot(base, aes(x=Obitos)) + geom_histogram()+ xlab('Obitos')+ ylab('')
g3 <- ggplot(base, aes(x=Renda)) + geom_histogram()+ xlab('Renda')+ ylab('')
g4 <- ggplot(base, aes(x=Pop)) + geom_histogram()+ xlab('Popula??o')+ ylab('')
g5 <- ggplot(base, aes(x=Alfab)) + geom_histogram()+ xlab('Alfabetiza??o')+ ylab('')
g6 <- ggplot(base, aes(x=Frota)) + geom_histogram()+ xlab('Frota')+ ylab('')
g7 <- ggplot(base, aes(x=PopUrb)) + geom_histogram()+ xlab('PopUrb')+ ylab('')

grid.arrange(g1,g2,g3,g4,g5,g6,g7,ncol=3, nrow=3)


#Vamos aplicar uma transforma??o logaritmica nas vari?veis apontadas como mais 
#assim?tricas na an?lise descritiva:


base$Obitos <- log(base$Obitos)
base$Frota<- log(base$Frota)
base$DensDem<- log(base$DensDem)
base$Renda<- log(base$Renda)
base$Pop<- log(base$Pop)

g9 <- ggplot(base, aes(x=DensDem)) + geom_histogram()+ xlab('Densidade Demogr?fica')+ ylab('')
g10 <- ggplot(base, aes(x=Obitos)) + geom_histogram()+ xlab('Obitos')+ ylab('')
g11 <- ggplot(base, aes(x=Renda)) + geom_histogram()+ xlab('Renda')+ ylab('')
g12 <- ggplot(base, aes(x=Pop)) + geom_histogram()+ xlab('Popula??o')+ ylab('')
g13 <- ggplot(base, aes(x=Alfab)) + geom_histogram()+ xlab('Alfabetiza??o')+ ylab('')
g14 <- ggplot(base, aes(x=Frota)) + geom_histogram()+ xlab('Frota')+ ylab('')

grid.arrange(g9,g10,g11,g12,g13,g14, ncol=3, nrow=3)



#CORRELA??O
View(base)
cor <- cor(base[ , c(2,3,4,5,6,7,8)], use = "na.or.complete")
library(corrplot)
corrplot.mixed(cor, upper = "ellipse")



  #DISPERS?O
library(carData)
scatterplotMatrix(base[ , c(2,4,5,6,8,9)],col = c('Red', 'Gray', 'pink'),lwd = 3)
str(base)

#MODELAGEM

m1 <- glm(Obitos ~ Renda + DensDem + Pop + Alfab + PopUrb + Frota,
              data = base, family = 'poisson')
m1



library(MASS)
library(foreign)

m2 <- glm.nb(Obitos ~ Desemp + Renda + BaxRen + IDH + DensDem + Pop + Alfab + PopUrb,
             data = base)
m2


ajuste = c('m1', 'm2')
aic    = c(AIC(m1), AIC(m2))
vero = c(logLik(m1),logLik(m2))
data.frame(ajuste, aic, vero)


library(hnp)

#pontos fora
hnp(m1, xlab = 'Percentil da N(0,1)', ylab = 'Res?duos', 
    main = 'Gr?fico Normal de Probabilidades')

#satisfatorio
hnp(m2, xlab = 'Percentil da N(0,1)', ylab = 'Res?duos', 
    main = 'Gr?fico Normal de Probabilidades')

