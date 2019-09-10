#
# Leitura dos dados
#
dados = read.table('http://people.ufpr.br/~lucambio/CE075/1S2011/exemplo08.txt',h=T)
#
attach(dados)
names(dados)
#
# Estudo descritivo
#
library(lattice)
#
X11()
xyplot(pH ~ Tempo | Periodo, groups=Voluntario, ylab='pH', xlab='Minutos',type='l')
#
# Em ambos os grupos percebemos uma diminuicao do pH da placa bacteriana dentaria
# conforme o decorrer do tempo. O detalhe e que no grupo avaliado Antes da utilizacao 
# da solucao a variabilidade no pH e muito grande e em alguns individuos e quase 
# nula a diminuicao do pH. No grupo avaliado depois da aplicacao da solucao percebe-se
# uma diminuicao do pH no tempo em todos os individuos avaliados, alem disso a 
# variabilidade da resposta e pequena comparada ao outro grupo de individuos.
#
# Estimando a matriz de correlacoes
#
M0=dados[Tempo==0,4]
M1=dados[Tempo==5,4]
M2=dados[Tempo==10,4]
M3=dados[Tempo==15,4]
M4=dados[Tempo==30,4]
M5=dados[Tempo==45,4]
M6=dados[Tempo==60,4]
#
Corr=cor(cbind(M0,M1,M2,M3,M4,M5,M6))
#      
colnames(Corr)=c('0 min','5 min','10 min','15 min','30 min','45 min','60 min')
rownames(Corr)=c('0 min','5 min','10 min','15 min','30 min','45 min','60 min')
#
Corr
#
# Grafico de dispersao segundo Mostrando a correlacao 
#
library(car)
#
scatterplotMatrix(Corr,main=NULL,smooth=F,reg.line=F,
    var.labels=c('0 minutos','5 minutos','10 minutos','15 minutos','30 minutos',
                               '45 minutos','60 minutos'))
#
library(psych)
#
X11()
pairs(Corr)
#
library(graphics)
#
X11()
mosaicplot(Corr, main=NULL)
#
library("ellipse")
#
X11()
plotcorr(Corr,main=NULL,col=colors())
#
# Percebemos que existe uma forte correlacao na resposta entre os tempos de avaliacao
# e que esta correlacao diminui conforme transcorre o tempo.
#
# Grafico da correlacao em funcao da defasagem
#
X11()
plot(Corr[1,], main='Correlacao em funcao da defasagem', type='l',
     ylab='Correlacao', xlab='Defasagem')
#
for(i in 2:7) lines(Corr[i,i:7], lty=i, col=i)
#
# Atualizado em: Quarta-feira 19/02/2013 (Fernando Lucambio, lucambio@ufpr.br)
