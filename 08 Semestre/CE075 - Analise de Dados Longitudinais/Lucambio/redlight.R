#
# A legislatura do estado de Minnesota, em 2001, considera a instalacao de cameras em 
# cruzamentos com semaoros que fotografam carros que ultrapassam a luz vermelha. Os 
# legisladores estao preocupados que o numero de acidentes causados por carros furando
# as luzes vermelhas esta se tornando um grave problema de saude publica.
#
# Outra cidade realizou recentemente um estudo piloto para testar se a instalacao de 
# cameras ou aumentar a frequencia das patrulhas policiais reduz o numero de acidentes 
# que ocorrem em cruzamentos da cidade. O estudo envolveu 40 intersecoes, em cada um 
# dos quais o numero de acidentes foi registado semanalmente durante cinco semanas 
# consecutivas. 20 dos cruzamentos tinham cameras instaladas enquanto 20 nao o fizeram.
# Alem disso, 20 dos 40 tiveram policiamento extra e 20 nao. (Nao houve aleatorizacao,
# nao se sabe como foi decidido que cruzamentos tiveram qual dos "tratamentos").
# 
# Caracteristicas de cada intersecao tambem foram registrados: (1) numero medio de 
# carros/hora atravessando o cruzamento na hora do rush, e (2) se a intersecao tinha 
# designadas somente as pistas.
#
# O resultado e o numero de acidentes para cada cruzamento. Questoes de interesse sao:
# a patrulha diminui o numero de acidentes, as cameras diminuem o numero de acidentes, 
# a designacao das pistas diminui o numero de acidentes.
#
# dados = read.table('http://www.biostat.umn.edu/~lynn/ph7430/data/redlight.dat',h=T)
#
dados = read.table('redlight.txt',h=T)
attach(dados)
#
# Estudo descritivo
#
library(lattice)
#
patrol = factor(patrol, levels=c(0,1), labels=c('Sem policiamento extra','Com policiamento extra'))
#
camera = factor(camera, levels=c(0,1), labels=c('Sem cameras','Com cameras'))
#
turnlane = factor(turnlane, levels=c(0,1), labels=c('Sem designacao das pistas','Designacao das pistas'))
#
dados1 = data.frame(id,accidents,week,patrol,camera,cars,turnlane)
#
X11()
xyplot(accidents ~ week|turnlane*camera, groups=id, type='l', ylim=c(0,max(accidents)),
       ylab='Numero de acidentes', main='Sem policiamento extra',
       data=dados1[patrol=='Sem policiamento extra',])
#
X11()
xyplot(accidents ~ week|turnlane*camera, groups=id, type='l', ylim=c(0,max(accidents)), 
       ylab='Numero de acidentes', main='Com policiamento extra',
       data=dados1[patrol=='Com policiamento extra',])
#
# Percebemos que nos cruzamentos com policiamento extra houve uma diminuicao drastica
# do numero de acidentes, nao importando se as pistas estabam devidamente indicadas e 
# se existissem radares ou nao.
#
# Devemos lembrar que nem todos os cruzamentos tem o mesmo transito, para controlar 
# isso foi observado tambem o numero medio de carros/hora. Esta informacao sera 
# considerada como offset.
#
# Estrutura de correlacao
#
sem1 = dados1[week==1,]$accidents
sem2 = dados1[week==2,]$accidents
sem3 = dados1[week==3,]$accidents
sem4 = dados1[week==4,]$accidents
sem5 = dados1[week==5,]$accidents
#
corr = cor(cbind(sem1,sem2,sem3,sem4,sem5))
colnames(corr)=c('Semana 1','Semana 2','Semana 3','Semana 4','Semana 5')
rownames(corr)=c('Semana 1','Semana 2','Semana 3','Semana 4','Semana 5')
#
library(graphics)
#
X11()
mosaicplot(corr,main='Correlacoes')
#
library("ellipse")
#
X11()
plotcorr(corr,main='Correlacoes',col=colors())
#
# Indicam estes graficos que a estrutura mais adequada deve ser a constante.
# Decidiu-se considerar tambem a estrutura independente.
#
# Modelo Poisson
#
library(gcmr)
#
mod00.ind = gcmr( accidents ~ factor(week) + patrol*camera*turnlane, 
                  offset = log(cars), marginal = poisson.marg, 
                  cormat = cluster.cormat(id, type = "ind"),
                  options = list(seed = 123, nrep = c(30, 100)), data=dados)
#
mod00.exc = gcmr( accidents ~ factor(week) + patrol*camera*turnlane, 
                  offset = log(cars), marginal = poisson.marg, 
                  cormat = cluster.cormat(id, type = "exc"),
                  options = list(seed = 123, nrep = c(30, 100)), data=dados)
#
AIC(mod00.exc,mod00.ind)
#
# Estrutura de correlacao mais adequada: constante
#
summary.gcmr = function(adj){
  coefs = coef(adj)
  desvio = se(adj, type = "cluster")
  tval = coefs/desvio
  rdf = adj$n-length(coefs)
  #
  results = cbind( coefs, desvio, tval, 2*pt(abs(tval), rdf, lower.tail = FALSE))
  colnames(results) = c("estimates", "std.err.", "t value", "Pr(>|t|)")
  results}
#
summary.gcmr(mod00.exc)
#
# Modelo com somente efeitos principais
#
mod01.exc= update(mod00.exc, formula=accidents ~ factor(week) +patrol + camera + turnlane)
#
summary.gcmr(mod01.exc)
#
# Eliminando a variavel nao significativa turnlane
#
mod02.exc= update(mod01.exc, formula=accidents ~ .-turnlane)
#
summary.gcmr(mod02.exc)
#
# Podemos perceber que quando ha patrulhamento extra o numero de acidentes diminui,
# assim como quando colocado radar.
#
# Analise de residuos
#
library(ggplot2)
#
residuos = residuals(mod02.exc)
#
X11()
ggplot(data=as.data.frame(qqnorm( residuos , plot=F)), mapping=aes(x=x, y=y)) + 
  geom_point() + geom_smooth(method="lm", se=FALSE)
#
shapiro.test(residuos)
#
# Normalidade aceitada
#
# Resultados
#
preditor.linear = mod02.exc$x%*%coef(mod02.exc)[1:7]+log(cars)
media = exp(preditor.linear)
#
# Uma maneira de aferir a qualidade do modelo e observando a relacao entre observados
# e preditos
#
X11()
qplot(accidents,media, xlim=c(0,12), ylim=c(0,12),xlab='No. acidentes observados',
      ylab='No. acidentes preditos')
#
# Percebemos que o modelo subestima o numero de acidentes e nao considera a 
# possibilidade de ausencia de acidentes.
#
# Na tabela observamos que nao foram registrados acidentes em 33 instantes 
# de observacao
#
table(accidents)
#
# Contudo, nosso modelo nao registrou a possibilidade de zero acidentes
#
table(round(media))
#
# Deveriamos esperar valores somente na diagonal principal da seguinte matriz
#
table(accidents,round(media))
#
# Com as colunas comecando em zero e terminando em 12. Lamentavelmente, isso nao 
# acontece. Uma possibilidade seria considerar modelos com excesso de zeros.
#
# TAREFA PARA OUTRA SITUACAO!
#
# Apresentando os resultados segundo as covariaveis
#
dados2 = data.frame(id,accidents,media,week,patrol,camera,cars)
#
X11()
xyplot(media/cars~week|camera*patrol, groups=id, type='l', 
       ylab='Taxa de acidentes', data=dados2)
#
# Neste grafico mostramos a taxa de indicencia estimada de acidentes nos sinaleiros
# segundo as covariaveis patrol e camera. Podemos perceber que, no caso extremo, com 
# policiamento extra e camera a taxa de acidentes decresce e sempre apresenta os 
# menores indices de acidentes. Em geral, quando colocadas cameras a taxa de acidentes
# e menor mas, menor ainda e quando existe policiamento extra.
#
# A diminuicao observada na taxa de acidentes supomos deva-se ao fato da cidade estar
# sob policiamento extra durante o periodo de estudo.
#
# Atualizado em 09/11/2015
