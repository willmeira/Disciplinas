#
# Leitura dos dados
#
dados = read.table('http://people.ufpr.br/~lucambio/CE075/1S2011/exemplo10.txt',h=T)
#
attach(dados)
names(dados)
#
# ESTUDO DESCRITIVO
#
library(lattice)
#
Teste=factor(Teste,levels=c('Pre','Pos'))
#
X11()
xyplot(Resistencia ~ Teste | Gas, groups=Rato, ylab='Resistencia', 
       main='Resistencia segundo o tratamento', xlab='Teste',type='b')
#
# No grupo controle (C) percebemos que a resposta media no teste Pre e Pos sao 
# parecidas, embora alguns individuos apresentaram comportamento atipico. No caso 
# da mistura de helio e oxigenio, que representa o tratamento de interesse a resposta 
# media quando aplicado a mistira de gases diminui.
#
# Observemos o comportamento de cada individuo
#
X11()
xyplot(Resistencia[Gas=='T'] ~ Teste[Gas=='T'] | Rato[Gas=='T'], layout=c(5,3),
       type='b', main='Resistencia no tratamento', xlab='Teste', ylab='Resistencia')
#
X11()
xyplot(Resistencia[Gas=='C'] ~ Teste[Gas=='C'] | Rato[Gas=='C'], layout=c(5,3),
       type='b', main='Resistencia no controle', xlab='Teste', ylab='Resistencia')
#
# Evidentemente a influencia do individuo e muito forte.
#
# Estimando a matriz de correlacoes
#
Res.Pre=dados[Teste=='Pre',4]
Res.Pos=dados[Teste=='Pos',4]
#
Corr=cor(cbind(Res.Pre,Res.Pos))
#      
colnames(Corr)=c('Pre','Pos')
rownames(Corr)=c('Pre','Pos')
#
Corr
#
# Percebemos que existe uma correlacao fraca na resposta entre os teste Pre e Pos.
#
# MODELO
#
library(nlme)
#
adj1 = lme(Resistencia ~ Teste+Gas, random=~Rato|Rato,data=dados)
adj2 = update(adj1, correlation = corCAR1(form=~Teste|Rato))
#
anova(adj1,adj2)
#
adj3 = lme(Resistencia ~ Teste+Gas, random=~Gas|Rato,data=dados)
adj4 = update(adj3, correlation = corCAR1(form=~Teste|Rato))
#
anova(adj3,adj4)
#
adj5 = lme(Resistencia ~ Teste+Gas, random=~1|Rato,data=dados)
adj6 = update(adj5, correlation = corCAR1(form=~1|Rato))
#
anova(adj5,adj6)
#
adj7 = lme(Resistencia ~ Teste*Gas, random=~1|Rato,data=dados)
adj8 = update(adj7, correlation = corCAR1(form=~1|Rato))
#
anova(adj7,adj8)
#
# Interacao nao significativa
#
# Observamos nos ANOVA que a estrutura de correlacao nao e necessaria.
#
AIC(adj1,adj3,adj5)
#
# O AIC e menor no modelo em adj5, logo este sera o escolhido.
#
# RESIDUOS
#
X11()
plot(adj5)
#
X11()
plot(adj5,Rato~resid(.))
#
X11()
plot(adj5,Resistencia~resid(.))
#
# Os residuos nao apresentam aberracoes, entao coinsideramos adequado o ajuste.
#
# Modelo ajustado
#
summary(adj5)
#
# Observemos os efeitos fixos
#
fixed.effects(adj5)
#
# e os efeitos aleatorios
#
random.effects(adj5)
#
# Apresentando os resultados
#
X11()
xyplot(predict(adj5) ~ Teste | Gas, groups=Rato, ylab='Resistencia estimada', 
       main='Resistencia segundo o tratamento', xlab='Teste',type='b')
#
# Concluimos que houve efeito do Gas o do Teste. Significa que o simples fato de 
# submeter os ratos a ventilacao com ar sintetico diminui a resistencia respiratoria e
# quando acrescentamos uma mistura de helio e oxigenio (T) diminuimos ainda mais 
# a resistencia respiratoria.
#
# Atualizado em: Segunda-feira 10/03/2014 (Fernando Lucambio, lucambio@ufpr.br)
