#
# Leitura dos dados
#
dados=read.table('http://people.ufpr.br/~lucambio/CE075/1S2010/exemplo04.txt',h=T)
#
attach(dados)
names(dados)
#
# Estudo descritivo
#
plot(distancia~idade,type='n',pch=19,ylab='Distancia',xlab='Idades',
     main='Distancia entre pituitaria e fissura pterigomaxilar')
#
for(i in 1:11){ 
  lines(distancia[Id==i] ~ idade[Id==i],col=1,lty=1)
  points(distancia[Id==i] ~ idade[Id==i],col=1,cex=0.5) }
#
for(i in 12:27){ 
  lines(distancia[Id==i] ~ idade[Id==i],col=2,lty=3)
  points(distancia[Id==i] ~ idade[Id==i],col=2,cex=0.5) }
#
legend(8,31,legend=c('Meninas','Meninos'),lty=c(1,3),col=c(1,2))
#
# Uma outra forma
#
library(lattice)
#
Sexo=factor(sexo,labels=c('Meninas','Meninos'))
#
xyplot(distancia ~ factor(idade)|Sexo,groups=Id, ,ylab='Distancia',xlab='Idades',
       main='Distancia entre pituitaria e fissura pterigomaxilar',type='l')
#
# Embora em ambos os casos (meninos e meninas) percebemos crescimento desta distancia
# e mais acentuado o crescimento no caso dos meninos, isso deve implicar na significancia
# das covariaveis tempo e sexo no estudo.
#
# Percebemos tambem que o crescimento dessa distancia e mais homogeneo no caso das 
# meninas.
#
# Estimando as matrizes de covariancas e correlacoes
#
# Para as meninas (sexo=0)
#
dist08=dados[idade==8 & sexo==0,4]
dist10=dados[idade==10 & sexo==0,4]
dist12=dados[idade==12 & sexo==0,4]
dist14=dados[idade==14 & sexo==0,4]
#
Corr1=cor(cbind(dist08,dist10,dist12,dist14))
#
Corr1=matrix(Corr1, nrow=4,ncol=4,dimnames=list(
  c('8 anos','10 anos','12 anos','14 anos'),
  c('8 anos','10 anos','12 anos','14 anos')))
#
Corr1
#
# Para os meninos (sexo=1)
#
dist08=dados[idade==8 & sexo==1,4]
dist10=dados[idade==10 & sexo==1,4]
dist12=dados[idade==12 & sexo==1,4]
dist14=dados[idade==14 & sexo==1,4]
#
Corr2=cor(cbind(dist08,dist10,dist12,dist14))
#
Corr2=matrix(Corr2, nrow=4,ncol=4,dimnames=list(
  c('8 anos','10 anos','12 anos','14 anos'),
  c('8 anos','10 anos','12 anos','14 anos')))
#
Corr2
#
# Correlacao global entre idades
#
dist08=dados[idade==8,4]
dist10=dados[idade==10,4]
dist12=dados[idade==12,4]
dist14=dados[idade==14,4]
#
Corr3=cor(cbind(dist08,dist10,dist12,dist14))
#
Corr3=matrix(Corr3, nrow=4,ncol=4,dimnames=list(
  c('8 anos','10 anos','12 anos','14 anos'),
  c('8 anos','10 anos','12 anos','14 anos')))
#
Corr3
#
# Para melhor visualizar a matriz de correlacoes
#
library(graphics)
mosaicplot(Corr3,main='Correlacoes')
#
library("ellipse")
#
plotcorr(Corr3,main='Correlacoes',col=colors())
#
Dist=matrix(cbind(dist08,dist10,dist12,dist14),ncol=4)
colnames(Dist)=c('8 anos','10 anos','12 anos','14 anos')
#
library(psych)
#
pairs(Dist)
#
# Percebemos que as correlacoes entre as visitas de periodos de anos consecutivos 
# sao mais fortes e que esta correlacao e constante no tempo.
#
# Atualizado em: Segunda-feira 22/04/2013 (Fernando Lucambio, lucambio@ufpr.br)
