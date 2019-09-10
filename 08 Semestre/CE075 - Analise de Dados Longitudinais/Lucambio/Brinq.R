#
# LEITURA DE DADOS
#
dados = read.table('http://people.ufpr.br/~lucambio/CE075/1S2014/brinq.txt', h=T)
#
attach(dados)
names(dados)
#
library(lattice)
#
# ESTUDO DESCRITIVO
#
X11()
xyplot(Resp ~ Dia | Produto, groups=Turno, type='l', 
       main='Proporcao de nao-conformidade na linha L1',
       auto.key=list(title='Turno', points=F, lines=T, space='right'),
       data=dados[Linha=='L1',])
#
X11()
xyplot(Resp ~ Dia | Produto, groups=Turno, type='l', 
       main='Proporcao de nao-conformidade na linha L2',
       auto.key=list(title='Turno', points=F, lines=T, space='right'),
       data=dados[Linha=='L2',])
#
# Observamos que na fabricacao dos barquinos (BA) houve menores taxas 
# de nao-conformidade tanto na linha L1 quanto na linha L2. Tambem pode ser
# percebido que nao houve um turno que se destacasse com menores taxas de 
# nao conformidade sempre.
#
Resp1=Resp[Dia==1]
Resp2=Resp[Dia==2]
Resp3=Resp[Dia==3]
Resp4=Resp[Dia==4]
Resp5=Resp[Dia==5]
#
Corr=cor(cbind(Resp1,Resp2,Resp3,Resp4,Resp5))
#      
colnames(Corr)=c('Dia 1','Dia 2','Dia 3','Dia 4','Dia 5')
rownames(Corr)=c('Dia 1','Dia 2','Dia 3','Dia 4','Dia 5')
#
Corr
#
# Grafico de dispersao mostrando a correlacao 
#
library("ellipse")
#
X11()
par(mar=c(5,4,1,1),pch=19,cex.axis=0.6)
plotcorr(Corr,main=NULL,col=colors())
#
# Das diferentes possiveis escolhas de matrizes de correlacao, a mais adequada
# considero seja a permutavel.
#
# MODELOS
#
library(geepack)
#
mod1 = geeglm(Resp ~ Dia+Produto+Turno+Linha+offset(log(Pcao)), id=Dia,
              family=poisson, corstr='ex', data=dados, scale.fix=T)
#
summary(mod1)
#
mod2 = geeglm(Resp ~ Dia+Produto+Linha+offset(log(Pcao)), id=Dia,
              family=poisson, corstr='ex', data=dados, scale.fix=T)
#
summary(mod2)
#
mod3 = geeglm(Resp ~ Dia+Produto+Turno+Linha+offset(log(Pcao)), id=Dia,
              family=poisson, corstr='ex', data=dados, scale.fix=F)
#
summary(mod3)
#
mod4 = geeglm(Resp ~ Dia+Produto+Turno+Linha+offset(log(Pcao)), id=Dia,
              family=poisson, corstr='ar1', data=dados, scale.fix=T)
#
mod5 = geeglm(Resp ~ Dia+Produto+Turno+Linha+offset(log(Pcao)), id=Dia,
              family=poisson, corstr='ar1', data=dados, scale.fix=T)
#
# Escolha da estrutura de correlacao segundo AIC proposto por
# Akaike's Information Criterion in Generalized Estimating Equations
# Wei Pan, Biometrics, Vol. 57, No. 1. (Mar., 2001), pp. 120-125.
#
library(MuMIn)
#
model.sel(mod1, mod4, rank = QIC)
#
# Dado que o valor do QIC nao permite escolha, decido pela estrutura de correlacao 
# observada no estudo descritivo.
#
anova(mod1)
#
# Primeiro eliminamos o Turno
#
anova(mod2)
#
# Consideramos este como modelo final
#
# Atualizado em 02/04/2014