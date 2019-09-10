#
# Leitura dos dados
#
dados=read.table('http://people.ufpr.br/~lucambio/CE075/1S2011/exemplo09.txt',h=T)
#
attach(dados)
names(dados)
#
# Estudo descritivo
#
X11()
plot(PAM~Momento,type='n',pch=19,main='Indometacina x Nifedipina',ylab='Pressao arterial media (PAM) medida em mmHg')
#
for(i in 1:12){ 
lines(PAM[Cao==i]~Momento[Cao==i],col=1,lty=1)
points(PAM[Cao==i]~Momento[Cao==i],col=1,cex=0.5) }
#
for(i in 13:24){ 
lines(PAM[Cao==i]~Momento[Cao==i],col=2,lty=3)
points(PAM[Cao==i]~Momento[Cao==i],col=2,cex=0.5) }
#
legend(0.5,70,legend=c('Indometacina','Nifedipina'),lty=c(1,3),col=c(1,2))
#
library(ggplot2)
#
# Box-plot segundo o grupo antes da aplicacao de MgSO4
#
X11()
qplot(Grupo[Momento=='Antes'],PAM[Momento=='Antes'],geom='boxplot',xlab='Grupo',ylab='Pressao arterial media (PAM) medida em mmHg',
main='Comportamento dos indivíduos antes da aplicacao de MgSO4')
#
# Box-plot segundo o grupo depois da aplicacao de MgSO4
#
X11()
qplot(Grupo[Momento=='Depois'],PAM[Momento=='Depois'],geom='boxplot',xlab='Grupo',ylab='Pressao arterial media (PAM) medida em mmHg',
main='Comportamento dos indivíduos depois da aplicacao de MgSO4')
#
# Box-plot para o efeito de tomar MgSO4
#
Diff = rep(0,dim(dados)[1])
#
Diff[Grupo=='indometacina'] = PAM[Momento=='Antes' & Grupo=='indometacina']-PAM[Momento=='Depois' & Grupo=='indometacina']
Diff[Grupo=='nifedipina'] = PAM[Momento=='Antes' & Grupo=='nifedipina']-PAM[Momento=='Depois' & Grupo=='nifedipina']
#
X11()
qplot(Grupo,Diff,geom='boxplot',xlab='Grupo',ylab='Diferenca na PAM antes e depois de tomar MgSO4',
main='Comportamento dos indivíduos que antes e depois de tomar MgSO4')
#
# Outra forma
#
library(lattice)
#
X11()
bwplot(PAM~Momento|Grupo)
#
X11()
bwplot(PAM~Grupo|Momento)
#
# Estimando as matrizes de covariancas e correlacoes
#
# Para o grupo que tomou indometacina
#
PAM.Antes=dados[Momento=='Antes' & Grupo=='indometacina',4]
PAM.Depois=dados[Momento=='Depois' & Grupo=='indometacina',4]
#
cov(cbind(PAM.Antes,PAM.Depois))
cor(cbind(PAM.Antes,PAM.Depois))
#
# Para o grupo que tomou nifedipina
#
PAM.Antes=dados[Momento=='Antes' & Grupo=='nifedipina',4]
PAM.Depois=dados[Momento=='Depois' & Grupo=='nifedipina',4]
#
cov(cbind(PAM.Antes,PAM.Depois))
cor(cbind(PAM.Antes,PAM.Depois))
#
# Geral
#
PAM.Antes=dados[Momento=='Antes',4]
PAM.Depois=dados[Momento=='Depois',4]
#
cov(cbind(PAM.Antes,PAM.Depois))
cor(cbind(PAM.Antes,PAM.Depois))
#