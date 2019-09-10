#
# Leitura dos dados
#
dados=read.table('http://people.ufpr.br/~lucambio/CE075/1S2010/exemplo02.txt',h=T)
#
attach(dados)
names(dados)
#
# Estudo descritivo
#
# Graficos de perfil
#
par(mar=c(5,4,2,2), cex=0.8)
plot(ataques ~ periodo,type='n',pch=19,main='Progabide x Placebo',
     ylab='No. de ataques epilepticos ',xlab='Periodo')
#
for(i in 1:28){
  lines(ataques[paciente==i] ~ periodo[paciente==i],col=1,lty=1)
  points(ataques[paciente==i] ~ periodo[paciente==i],col=1,cex=0.5) }
#
for(i in 29:59){
  lines(ataques[paciente==i] ~ periodo[paciente==i],col=2,lty=3)
  points(ataques[paciente==i] ~ periodo[paciente==i],col=2,cex=0.5) }
#
legend(3,150,legend=c('Placebo','Progabide'),lty=c(1,3),col=c(1,2))
#
# Outra forma de fazer os gaficos de perfil
#
require(lattice)
#
Tratamento=factor(tratamento,labels=c('Placebo','Progabide'))
#
xyplot(ataques ~ periodo|Tratamento,groups=paciente,type='l',
       ylab='No. de ataques epilépticos',xlab='Período')
#
# Graficos de Box-Plot
#
library(ggplot2)
#
# Box-plot para o grupo de individuos que recebeu placebo (tratamento=0)
#
# Estes graficos nao consideram a dependencia na resposta entre os instantes 
# de tempo, semanas neste caso.
#
qplot(factor(periodo)[tratamento==0],ataques[tratamento==0],ylim=c(1,150),
      geom='boxplot',xlab='Periodo',ylab='No. de ataques epilépticos',
      main='Comportamento dos indiví?duos que receberam placebo')
#
# Box-plot para o grupo de individuos que recebeu progabide (tratamento=1)
#
qplot(factor(periodo)[tratamento==1],ataques[tratamento==1],ylim=c(1,150),
      geom='boxplot',xlab='Periodo',ylab='No. de ataques epilépticos',
      main='Comportamento dos indiví?duos que receberam progabide')
#
# Percebemos uma diminuicao no numero de ataques epilepticos muito maior 
# no grupo tratado com a droga Progabide, a qual e objetivo do estudo. Devemos
# observar que o numero de semanas nos periodos de estudo são diferentes o qual 
# implica que a taxa de ataques por periodo e diferente do que simplesmente
# apresentar a resposta.
#
# Cosntruimos entao uma nova resposta Taxa=No. de ataques/semanas
#
Taxa=ataques/semanas
#
# Graficos de perfil
#
plot(Taxa ~ periodo,type='n',pch=19,main='Progabide x Placebo',
     ylab='Taxa de ataques epilépticos ',xlab='Período')
#
for(i in 1:28){
  lines(Taxa[paciente==i] ~ periodo[paciente==i],col=1,lty=1)
  points(Taxa[paciente==i] ~ periodo[paciente==i],col=1,cex=0.5) }
#
for(i in 29:59){
  lines(Taxa[paciente==i] ~ periodo[paciente==i],col=2,lty=3)
  points(Taxa[paciente==i] ~ periodo[paciente==i],col=2,cex=0.5) }
#
legend(3,50,legend=c('Placebo','Progabide'),lty=c(1,3),col=c(1,2))
#
# Outra forma de fazer os graficos de perfil
#
xyplot(Taxa ~ periodo|Tratamento,groups=paciente,type='l',
       ylab='Taxa de ataques epilépticos',xlab='Período')
#
# Graficos de Box-Plot
#
qplot(factor(periodo)[tratamento==0],Taxa[tratamento==0],ylim=c(1,20),
      geom='boxplot',xlab='Periodo',ylab='Taxa de ataques epilépticos',
      main='Comportamento dos indiví?duos que receberam placebo')
#
# Box-plot para o grupo de individuos que recebeu progabide (tratamento=1)
#
qplot(factor(periodo)[tratamento==1],Taxa[tratamento==1],ylim=c(1,20),
      geom='boxplot',xlab='Periodo',ylab='Taxa de ataques epilépticos',
      main='Comportamento dos indiví?duos que receberam progabide')
#
# Percebe-se que em media nao houve uma diminuicao clara do no. de ataques epilepticos
# porem os individuos que tomaram Progabide apresentam uma variabilidade menor. Isto 
# significa que nao devemos esperar surtos destes ataques enquanto tomarem a droga
# anti-epileptica.
#
# Estimacao da matriz de correlacao
#
# Construimos variaveis contendo todos os dados de cada Bloco
#
# Estimando as matrizes de covariancas e correlacoes
#
# Para o grupo de inviduos que recebeu placebo (tratamento=0)
#
A1=dados[periodo==1 & tratamento==0,4]
A2=dados[periodo==2 & tratamento==0,4]
A3=dados[periodo==3 & tratamento==0,4]
A4=dados[periodo==4 & tratamento==0,4]
A5=dados[periodo==5 & tratamento==0,4]
#
cor(cbind(A1,A2,A3,A4,A5))
#
# Para o grupo de inviduos que recebeu progabide (tratamento=1)
#
B1=dados[periodo==1 & tratamento==1,4]
B2=dados[periodo==2 & tratamento==1,4]
B3=dados[periodo==3 & tratamento==1,4]
B4=dados[periodo==4 & tratamento==1,4]
B5=dados[periodo==5 & tratamento==1,4]
#
cor(cbind(B1,B2,B3,B4,B5))
#
# Destas duas matrizes podemos perceber que no caso dos pacientes que tomaram a 
# droga a correlacao entre os periodos de observacao permanece estavel.
#
# Nestes modelos trataremos, em geral, a matriz de correlacao como unica para as
# diferentes situacoes. Isto e uma restricao da maioria dos modelos longitudinais.
#
C1=dados[periodo==1,4]
C2=dados[periodo==2,4]
C3=dados[periodo==3,4]
C4=dados[periodo==4,4]
C5=dados[periodo==5,4]
#
Corr=cor(cbind(C1,C2,C3,C4,C5))
#
Corr=matrix(Corr, nrow=5,ncol=5,dimnames=list(
  c('Periodo 1','Periodo 2','Periodo 3','Periodo 4','Periodo 5'),
  c('Periodo 1','Periodo 2','Periodo 3','Periodo 4','Periodo 5')))
#
Corr
#
# Para melhor visualizar a matriz de correlacoes
#
require(graphics)
mosaicplot(Corr,main='Correlacoes')
#
library(package="ellipse")
#
plotcorr(Corr,main='Correlacoes',col=colors())
#
# Percebemos que as correlacoes entre os periodos sao constantes e fortemente
# positivas.
#
# Atualizado em: Sabado 20/04/2013 (Fernando Lucambio, lucambio@ufpr.br)