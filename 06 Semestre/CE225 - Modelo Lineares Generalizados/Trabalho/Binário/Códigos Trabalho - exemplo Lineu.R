library(gridExtra)
library(corrplot)
library(car)
library(statmod)
library(effects)
library(gplots)
library(gtools)
library(gdata)
library(ROCR)
library(faraway)
library(ggplot2)
library(hnp)
#*********************************

##Modelos de Regressão para Dados Binários
Lais
Simone
Willian
Yasmin

Prof. Cesar Augusto Taconeli

Modelos Lineares Generalizados (CE225)

Novembro/2018

Base de dados com todas as tentativas de Field Goal da temporada regular de 2008 da NFL

1. Dados
Os dados estão disponíveis no site da Universidade da Flórida, no 
link http://users.stat.ufl.edu/~winner/datasets.html


#*********************************
bd <- read.csv('nfl2008_fga.csv',header = TRUE,sep = ',')
head(bd)

dados <-bd[,c(4,9,10,14,15,16,21)]
names(dados)<-c('qtr','down','togo','dist','home','diff','good')

dados$qtr<-as.factor(dados$qtr)
dados$down<-as.factor(dados$down)
dados$home<-as.factor(dados$home)
dados$good<-as.factor(dados$good)

head(dados)
summary(dados)
str(dados)
[,c(4,9,10,14,15,16,21)]

ajuste0<-glm(good~.,family = binomial(link='logit'),data = dados)
summary(ajuste0)
#Verificamos que podemos retirar as covaríaveis "kickteam", "def" e "kicker"

head(dados)
summary(dados)
str(dados)

ajuste1<-glm(good~.,family = binomial(link='logit'),data = dados1)

ajuste2<-glm(good~.,family = binomial(link='probit'),data = dados1)

ajuste3<-glm(good~.,family = binomial(link='cloglog'),data = dados1)

ajuste4<-glm(good~.,family = binomial(link='cauchit'),data = dados1)

selec <- data.frame(ajuste=c('logito', 'probito', 'cloglog', 'cauchy'),
                    aic=c(AIC(ajuste1), AIC(ajuste2), AIC(ajuste3), AIC(ajuste4)),
                    logLik=c(logLik(ajuste1),logLik(ajuste2),logLik(ajuste3),logLik(ajuste4)))

selec

ajuste2.1 <- step(ajuste2, direction = "both")
ajuste2.2 <- step(ajuste2, direction = "forward")
ajuste2.3 <- step(ajuste2, direction = "backward")
summary(ajuste3.2)

selec2<- data.frame(ajuste=c('ajuste2.1', 'ajuste2.2', 'ajuste2.3'),
                    aic=c(AIC(ajuste2.1), AIC(ajuste2.2), AIC(ajuste2.3)),
                    logLik=c(logLik(ajuste2.1),logLik(ajuste2.2),logLik(ajuste2.3)),
                    devresidual=c(deviance(ajuste2.1),deviance(ajuste2.2),deviance(ajuste2.3)))

selec2

summary(ajuste2.1)


dados1<-dados[,c(4,5,7)]
  
head(dados1)
summary(dados1)
str(dados1)


  
  

data(wbca)
dados <- wbca
sum(dados$Class==1)
dados$Class <- ifelse(dados$Class == 0, 1, 0)
sum(dados$Class==1)
head(dados)
summary(dados[ , 2:10])
par(mfrow=c(2,5))
boxplot(dados$Adhes, xlab = '', ylab = '', main = 'Adhes', las=1)
boxplot(dados$BNucl, xlab = '', ylab = '', main = 'BNucl', las=1)
boxplot(dados$Chrom, xlab = '', ylab = '', main = 'Chrom', las=1)
boxplot(dados$Epith, xlab = '', ylab = '', main = 'Epith', las=1)
boxplot(dados$Mitos, xlab = '', ylab = '', main = 'Mitos', las=1)
boxplot(dados$NNucl, xlab = '', ylab = '', main = 'NNucl', las=1)
boxplot(dados$Thick, xlab = '', ylab = '', main = 'Thick', las=1)
boxplot(dados$UShap, xlab = '', ylab = '', main = 'UShap', las=1)
boxplot(dados$USize, xlab = '', ylab = '', main = 'USize', las=1)
mtext(side=2,cex=1.3,line=-1.5,text="Nota na Avaliação Médica",outer=TRUE)

par(mfrow=c(3,3), las=1)
hist(dados$Adhes, xlab = '', ylab = '', main = 'Adhes')
hist(dados$BNucl, xlab = '', ylab = '', main = 'BNucl')
hist(dados$Chrom, xlab = '', ylab = '', main = 'Chrom')
hist(dados$Epith, xlab = '', ylab = '', main = 'Epith')
hist(dados$Mitos, xlab = '', ylab = '', main = 'Mitos')
hist(dados$NNucl, xlab = '', ylab = '', main = 'NNucl')
hist(dados$Thick, xlab = '', ylab = '', main = 'Thick')
hist(dados$UShap, xlab = '', ylab = '', main = 'UShap')
hist(dados$USize, xlab = '', ylab = '', main = 'USize')

g1<-ggplot(dados, aes(x=factor(Class), y=Adhes, color=factor(Class))) + 
  geom_boxplot()+ xlab('Adhes')+ ylab('') +
  theme(legend.title=element_blank())

g2<-ggplot(dados, aes(x=factor(Class), y=BNucl, color=factor(Class))) + 
  geom_boxplot()+ xlab('BNucl')+ ylab('') +
  theme(legend.title=element_blank())

g3<-ggplot(dados, aes(x=factor(Class), y=Chrom, color=factor(Class))) + 
  geom_boxplot()+ xlab('Chrom')+ ylab('') +
  theme(legend.title=element_blank())

g4<-ggplot(dados, aes(x=factor(Class), y=Epith, color=factor(Class))) + 
  geom_boxplot()+ xlab('Epith')+ ylab('') +
  theme(legend.title=element_blank())

g5<-ggplot(dados, aes(x=factor(Class), y=Mitos, color=factor(Class))) + 
  geom_boxplot()+ xlab('Mitos')+ ylab('') +
  theme(legend.title=element_blank())

g6<-ggplot(dados, aes(x=factor(Class), y=NNucl, color=factor(Class))) + 
  geom_boxplot()+ xlab('NNucl')+ ylab('') +
  theme(legend.title=element_blank())

g7<-ggplot(dados, aes(x=factor(Class), y=Thick, color=factor(Class))) + 
  geom_boxplot()+ xlab('Thick')+ ylab('') +
  theme(legend.title=element_blank())

g8<-ggplot(dados, aes(x=factor(Class), y=UShap, color=factor(Class))) + 
  geom_boxplot()+ xlab('UShap')+ ylab('') +
  theme(legend.title=element_blank())

g9<-ggplot(dados, aes(x=factor(Class), y=USize, color=factor(Class))) + 
  geom_boxplot()+ xlab('USize')+ ylab('') +
  theme(legend.title=element_blank())

grid.arrange(g1, g2, g3, g4, g5, g6, g7,g8 ,g9, ncol=3, nrow=3)


cor <- cor(dados[ , 2:10])
x11()
corrplot.mixed(cor, upper = "ellipse")
x11()
scatterplotMatrix(dados[ , 2:10], col = c('Red', 'Red', 'Gray'), lwd = 3)

#******************************
#teste de cores
scatterplotMatrix(dados[ , 2:10], 
                  lwd = 3, col = c(method=lm, lty=1, lwd=2, col=carPalette())
                  )
#******************************
ajuste1 <- glm(Class ~ .,family=binomial(link='logit'),data = dados)
ajuste2 <- glm(Class ~ .,family=binomial(link = 'probit'),data = dados)
ajuste3 <- glm(Class ~ .,family=binomial(link='cloglog'),data = dados)
ajuste4 <- glm(Class ~ .,family=binomial(link='cauchit'),data = dados)

selec <- data.frame(ajuste=c('logito', 'probito', 'cloglog', 'cauchy'),
                    aic=c(AIC(ajuste1), AIC(ajuste2), AIC(ajuste3), AIC(ajuste4)),
                    logLik=c(logLik(ajuste1),logLik(ajuste2),logLik(ajuste3),logLik(ajuste4)))

selec

summary(ajuste2)

ajuste2.1 <- step(ajuste2, direction = "both")
summary(ajuste2.1)

anova(ajuste2, ajuste2.1, test = 'Chisq')

par(mfrow=c(2,2))
plot(ajuste2.1, 1:4)

influenceIndexPlot(ajuste2.1, vars=c("Cook"), main="Distância de Cook")
influenceIndexPlot(ajuste2.1, vars=c("Studentized"), main="Resíduos Padronizados")

par(mfrow=c(1,2))

res <- qresiduals(ajuste2.1)

plot(res)

residuos <- qresiduals(ajuste2.1)
qqnorm(residuos)
qqline(residuos, col = 2)


envelope=function(modelo){
  dados=na.omit(modelo$data)
  nsim=100
  n=modelo$df.null+1
  r1=sort(rstandard(modelo,type='deviance'))
  m1=matrix(0,nrow=n,ncol=nsim)
  a2=simulate(modelo,nsim=nsim)
  
  for (i in 1:nsim){
    dados$y=a2[,i]
    aj=update(modelo,y~.,data=dados)
    m1[,i]=sort(rstandard(aj,type='deviance'))}
  
  li=apply(m1,1,quantile,0.025)
  m=apply(m1,1,quantile,0.5)
  ls=apply(m1,1,quantile,0.975)
  
  quantis=qnorm((1:n-0.5)/n)
  
  plot(rep(quantis,2),c(li,ls),type='n',xlab='Percentil da N(0,1)',ylab='Resíduos')
  title('Gráfico Normal de Probabilidades')
  lines(quantis,li,type='l')
  lines(quantis,m,type='l',lty=2)
  lines(quantis,ls,type='l')
  points(quantis,r1,pch=16,cex=0.75)
}

envelope(ajuste2.1)
hnp(ajuste2.1)

plot(allEffects(ajuste2.1), type = 'response', main = '')

perfis <- data.frame(Adhes = c(6,  3), 
                     BNucl = c(9,  1),
                     Chrom = c(3,  7),
                     Mitos = c(9,  1),
                     NNucl = c(8,  3),
                     Thick = c(2,  4),
                     UShap = c(6,  5)
)

predict(ajuste2.1, interval = 'prediction', newdata = perfis, type = 'response')

set.seed(1909)
indices <- sample(1:680, size = 477) 
dadosajuste <- dados[indices,]
dadosvalid <- dados[-indices,]

pred <- predict(ajuste2.1, newdata = dadosvalid, type = 'response')
corte <- ifelse(pred > 0.5, 'maligno', 'benigno')

tabela <- table(corte, dadosvalid$Class)
colnames(tabela)<- c('benigno', 'maligno')
tabela

sens <- tabela[2,2]/sum(tabela[,2])
sens 

espec <- tabela[1,1]/sum(tabela[,1])
espec

pred <- prediction(ajuste2.1$fitted.values, dados$Class)
perf <- performance(pred,"tpr","fpr")
x11()
plot(perf, xlab = '1 - Especificidade', ylab = 'Sensibilidade', 
     main = "Curva ROC", col = "blue", lwd = 2)

area <- performance(pred,"auc")
area@y.values