# pacotes necessÃ¡rios
library(DAAG)
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
## Carregando e ajustando a base de dados
## 1. Descrição dos dados
#*********************************
data("nassCDS")
dados <- nassCDS
dados<-subset(dados,yearacc==2002 & yearVeh == 2000)
#selecionando covariÃ¡veis
dados <-dados[,c(-2,-9,-10,-11,-15)]

#ajustando as variÃ¡veis
dados$dvcat <- as.ordered(ifelse(dados$dvcat == '1-9km/h','01-09 mph',
               ifelse(dados$dvcat == '10-24','10-24 mph',
               ifelse(dados$dvcat == '25-39','25-39 mph',
               ifelse(dados$dvcat == '40-54','40-54 mph','55+ mph')))))
dados$dead <- as.factor(ifelse(dados$dead == 'alive','Sim','NÃ£o'))
dados$airbag <- as.factor(ifelse(dados$airbag == 'airbag','Sim','NÃ£o'))
dados$seatbelt <- as.factor(ifelse(dados$seatbelt == 'belted','Sim','NÃ£o'))
dados$frontal <- as.factor(ifelse(dados$frontal == 1,'Sim','NÃ£o'))
dados$sex <- as.factor(ifelse(dados$sex == 'm', 'Masc','Fem'))
dados$occRole <- as.factor(ifelse(dados$occRole == 'driver','Driver','Pass'))
dados$deploy<-as.factor(ifelse(dados$deploy == 1,'Sim','NÃ£o'))
dados$injSeverity<-as.ordered(dados$injSeverity)

names(dados)<-c('veloc','sobrev','airbag','cinto','frontal','sexo','idade','ocupantes','abfunc','grav')
str(dados)

#*********************************
## AnÃ¡lise Descritiva
#**2.1 Medidas de Resumo**
#*********************************
head(dados,10)
summary(dados)
summary(dados[ , c(1:8,10)])
sum(dados$abfunc == 1)

x11()
#par(mfrow=c(2,5))
#avaliar com professor


#*********************************
#**2.3 Histogramas**
#*********************************

par(mfrow=c(4,3), las=1)
plot(dados$abfunc, xlab = '', ylab = '', main = 'AB Funcionou')
plot(dados$veloc, xlab = '', ylab = '', main = 'Velocidade')
plot(dados$sobrev, xlab = '', ylab = '', main = 'Sobrevivente')
plot(dados$airbag, xlab = '', ylab = '', main = 'Airbag')
plot(dados$cinto, xlab = '', ylab = '', main = 'Cinto')
plot(dados$frontal, xlab = '', ylab = '', main = 'Frontal')
plot(dados$sexo, xlab = '', ylab = '', main = 'Sexo')
#plot(dados$idade, xlab = '', ylab = '', main = 'Idade') 
boxplot(dados$idade~dados$abfunc,xlab = '', ylab = 'Idade', main = 'Idade') 
plot(dados$ocupantes, xlab = '', ylab = '', main = 'Ocupante')
plot(dados$grav, xlab = '', ylab = '', main = 'Gravidade') #variavel numerica 
mtext(side=2,cex=1.3,line=-1.5,text="Proporção de respostas",outer=TRUE)

################################################### até aqui no momento



################################################### tabelas para grÃ¡ficos de setor
#não utilizadas no relatório
x11()
par(mfrow=c(4,3), las=1)
pie(table(dados$abfunc), main = 'AB Funcionou')
pie(table(dados$veloc), main = 'Velocidade')
aux<-paste0(round((table(dados$veloc)/sum(table(dados$veloc)))*100,1),'%')
pie(table(dados$veloc),labels = paste0(names(table(dados$veloc)),' - ',aux))

pie(table(dados$sobrev), main = 'Sobrevivente')
pie(table(dados$airbag), main = 'Airbag')
pie(table(dados$cinto), main = 'Cinto')
pie(table(dados$frontal), main = 'Frontal')
pie(table(dados$sexo), main = 'Sexo')
pie(table(dados$idade), main = 'Idade')
pie(table(dados$ocupantes), main = 'Ocupante')
pie(table(dados$grav), main = 'Gravidade')

###################################################
# duplicado


pie(table(dados$abfunc), 
    main="GrÃ¡fico de setores: Grau de InstruÃ§Ã£o") 
par(mfrow = c(3,3))
plot(dados$abfunc, xlab = '', ylab = '', main = 'AB Funcionou')
plot(dados$veloc, xlab = '', ylab = '', main = 'Velocidade')
plot(dados$sobrev, xlab = '', ylab = '', main = 'Sobrevivente')
plot(dados$airbag, xlab = '', ylab = '', main = 'Airbag')
plot(dados$cinto, xlab = '', ylab = '', main = 'Cinto')
plot(dados$frontal, xlab = '', ylab = '', main = 'Frontal')
plot(dados$sexo, xlab = '', ylab = '', main = 'Sexo')
#plot(dados$idade, xlab = '', ylab = '', main = 'Idade') ### ******* inverteu eixo
plot(dados$ocupantes, xlab = '', ylab = '', main = 'Ocupante')
plot(dados$grav, xlab = '', ylab = '', main = 'Gravidade')


#*********************************
#**não foi incluido no relatório**
#*********************************
plot(abfunc~veloc, data = dados)
x11()
plot(abfunc~veloc, data = dados, xlab = 'Taxa', ylab = 'Consumo')
abline(lm(abfunc~veloc, data = dados), col=2, lwd = 2)

plot(cons~licen, data = dados, xlab = 'LicenÃ§a', ylab = 'Consumo')
abline(lm(cons~licen, data = dados), col = 2, lwd = 2)

plot(cons~renda, data = dados, xlab = 'Renda', ylab = 'Consumo')
abline(lm(cons~renda, data = dados), col = 2, lwd = 2)

plot(cons~estr, data = dados, xlab = 'Estrada', ylab = 'Consumo')
abline(lm(cons~estr, data = dados), col = 2, lwd = 2)


#*********************************
#**não foi incluido no relatório**
#*********************************

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

#*********************************
#**não foi incluido no relatório, função não roda - somente em dados numéricos**
#*********************************

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
ajuste1 <- glm(abfunc ~ .,family=binomial(link='logit'),data = dados)
ajuste2 <- glm(abfunc ~ .,family=binomial(link = 'probit'),data = dados)
ajuste3 <- glm(abfunc ~ .,family=binomial(link='cloglog'),data = dados)
ajuste4 <- glm(abfunc ~ .,family=binomial(link='cauchit'),data = dados)

summary(ajuste1)
summary(ajuste2)
summary(ajuste3)
summary(ajuste4)


selec <- data.frame(ajuste=c('logito', 'probito', 'cloglog', 'cauchy'),
                    aic=c(AIC(ajuste1), AIC(ajuste2), AIC(ajuste3), AIC(ajuste4)),
                    logLik=c(logLik(ajuste1),logLik(ajuste2),logLik(ajuste3),logLik(ajuste4)))

selec
selec2

summary(ajuste2)

ajuste2.1 <- step(ajuste2, direction = "both")
summary(ajuste2.1)

anova(ajuste2, ajuste2.1, test = 'Chisq')

par(mfrow=c(2,2))
plot(ajuste2.1, 1:4)

influenceIndexPlot(ajuste2.1, vars=c("Cook"), main="DistÃ¢ncia de Cook")
influenceIndexPlot(ajuste2.1, vars=c("Studentized"), main="ResÃ­duos Padronizados")

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
  
  plot(rep(quantis,2),c(li,ls),type='n',xlab='Percentil da N(0,1)',ylab='ResÃ­duos')
  title('GrÃ¡fico Normal de Probabilidades')
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