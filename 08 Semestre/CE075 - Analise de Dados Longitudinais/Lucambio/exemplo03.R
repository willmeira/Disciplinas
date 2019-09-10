#
# Leitura dos dados
#
dados=read.table('http://people.ufpr.br/~lucambio/CE075/1S2010/exemplo03.txt',h=T)
#
attach(dados)
names(dados)
#
# Estudo descritivo
#
require(lattice)
#
xyplot(factor(condivisitas,labels=c('Boa','Ruim')) ~ factor(visita)|paciente,
       type='l',xlab="Visita",ylab="Condição respiratória", 
       main='Pacientes submetidos a placebo sem pre-existencia \n de problemas respiratórios', 
       data=dados[base==0 & tratamento==1,])
#
xyplot(factor(condivisitas,labels=c('Boa','Ruim')) ~ factor(visita)|paciente,
       type='l',xlab="Visita",ylab="Condição respiratória", 
       main='Pacientes submetidos a droga ativa sem pre-existencia \n de problemas respiratórios', 
       data=dados[base==0 & tratamento==0,])
#
xyplot(factor(condivisitas,labels=c('Boa','Ruim')) ~ factor(visita)|paciente,
       type='l',xlab="Visita",ylab="Condição respiratória", 
       main='Pacientes submetidos a placebo com pre-existencia \n de problemas respiratórios', 
       data=dados[base==1 & tratamento==1,])
#
xyplot(factor(condivisitas,labels=c('Boa','Ruim')) ~ factor(visita)|paciente,
       type='l',xlab="Visita",ylab="Condição respiratória", 
       main='Pacientes submetidos a droga ativa com pre-existencia \n de problemas respiratórios', 
       data=dados[base==1 & tratamento==0,])
#
# Uma primeira observacao e que nao podemos utilizar a informacao de pre-existencia
# de problemas respiratorios no estudo, devido a termos uma unica observacao numa 
# das situacoes. Podemos observar tambem que no caso placebo nao houve mudancas na 
# condicao respiratoria: se era ruim permaneceu assim e se boa tambem permaneceu. Isso
# e o esperado para esse grupo de pacientes.
#
# Vejamos agora a resposta segundo o sexo e tratamento.
#
xyplot(factor(condivisitas,labels=c('Boa','Ruim')) ~ factor(visita)|paciente,
       type='l',xlab="Visita",ylab="Condição respiratória", 
       main='Pacientes do sexo feminino submetidos a placebo', 
       data=dados[sexo==0 & tratamento==1,])
#
xyplot(factor(condivisitas,labels=c('Boa','Ruim')) ~ factor(visita)|paciente,
       type='l',xlab="Visita",ylab="Condição respiratória", 
       main='Pacientes do sexo feminino submetidos a droga ativa', 
       data=dados[sexo==0 & tratamento==0,])
#
xyplot(factor(condivisitas,labels=c('Boa','Ruim')) ~ factor(visita)|paciente,
       type='l',xlab="Visita",ylab="Condição respiratória", 
       main='Pacientes do sexo masculino submetidos a placebo', 
       data=dados[sexo==1 & tratamento==1,])
#
xyplot(factor(condivisitas,labels=c('Boa','Ruim')) ~ factor(visita)|paciente,
       type='l',xlab="Visita",ylab="Condição respiratória", 
       main='Pacientes do sexo masculino submetidos a droga ativa', 
       data=dados[sexo==1 & tratamento==0,])
#
# Sexo tambem nao deve ser considerado no estudo, somente duas mulheres foram 
# observadas do tomando a droga ativa, embora as duas melehoraram.
#
# Quanto a idade, do fato de ser uma variavel com muitos valores possiveis o melhor
# e utilizarmos graficos de Box-Plot.
#
par(mfrow=c(2,2),cex.main=0.9,cex.lab=0.7,cex.axis=0.7,
    mar=c(3,4,1,1),pch=16,font.main=3)
#
boxplot(dados[tratamento==0 & sexo==0,4],ylim=c(10,70),col=2,
        main='Mulheres submetidas a droga ativa')
boxplot(dados[tratamento==1 & sexo==0,4],ylim=c(10,70),col=4,
        main='Mulheres submetidas a placebo')
boxplot(dados[tratamento==0 & sexo==1,4],ylim=c(10,70),col=3,
        main='Homes submetidos a droga ativa')
boxplot(dados[tratamento==1 & sexo==1,4],ylim=c(10,70),col=5,
        main='Homes submetidos a placebo')
#
# Percebemos que a idade nao muda entre os diferentes perfis dos individuos
# amostrado.
#
# Estimacao da matriz de correlacao
#
C1=dados[visita==1,6]
C2=dados[visita==2,6]
C3=dados[visita==3,6]
C4=dados[visita==4,6]
#
Corr=cor(cbind(C1,C2,C3,C4))
#
Corr=matrix(Corr, nrow=4,ncol=4,dimnames=list(
  c('Visita 1','Visita 2','Visita 3','Visita 4'),
  c('Visita 1','Visita 2','Visita 3','Visita 4')))
#
Corr
#
# Para melhor visualizar a matriz de correlacoes
#
library(graphics)
par(mfrow=c(1,1),cex.main=0.9,cex.lab=0.7,cex.axis=0.7,
    mar=c(3,4,1,1),pch=16,font.main=3)
mosaicplot(Corr,main='Correlacoes')
#
library("ellipse")
#
plotcorr(Corr,main='Correlacoes',col=colors())
#
# Percebemos que as correlacoes entre as visitas sao baixas. Isto pode ser devido 
# a resposta ser discreta dicotomica.
#
# Atualizado em: Segunda-feira 22/04/2013 (Fernando Lucambio, lucambio@ufpr.br)