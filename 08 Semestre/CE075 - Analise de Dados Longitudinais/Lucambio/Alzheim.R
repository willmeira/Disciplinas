#
# Alzheim Data
#
# dados=read.table('http://www.biostat.umn.edu/~lynn/ph7430/data/alzheim.dat',h=T)
dados=read.table('Alzheim.dat',h=T)
#
# The results of an unpublished Alzheimer's study are reported in Hand 
# and Talor (1987). There are two groups of patients, one receiving a 
# placebo and one receiving a drug called lecithin. Each patient was 
# asked to memorize a list of words. The outcome was number of words
# correctly recalled. This "test" was administered at each of five time 
# points: 0, 1, 2, 4, and 6 months. There are 48 patients, 22 on drug 
# and 26 on placebo. The words used at each testing occasion were 
# exactly the same as on previous testing occasions.
#
# It is expected that any treatment effects may be small and slow to
# appear because of the nature of dementias. The administration of the
# memorization "test" on multiple occasions also introduces a "practice
# effect," that is, a subject may improve over time merely because of
# carryover effects from one occasion to the next. This may be due to
# repeated exposure to the same words and/or repeated exposure to the 
# testing situation.
#
# Questions of interest are:
#  
#  -- Is there a time trend in recall ability across the five testing
# occasions?  Is it, for example, decreasing?
# -- Does any time trend differ by treatment?
#
# The data set contains treatment in column 1 (0 for placebo and 1 for lecithin) 
# and test results in columns 2 through 6. There are no other covariates available. 
#
# Data source: Hand and Taylor (1987). Multivariate analysis of variance
# and repeated measures. London: Chapman and Hall, Inc.
#
attach(dados)
head(dados)
#
# Fator do tratamento aplicado
#
Tratamento = factor(group, levels=c(0,1), labels=c('Placebo','Lecithin'))
Tratamento = rep(Tratamento, each=5)
#
# Resposta
#
N.Palabras = matrix(cbind(time0,time1,time2,time4,time6),ncol=48,byrow=T)
N.Palabras = c(N.Palabras)
#
# Controle do tempo de observacao
#
Meses = factor(rep(c(0,1,2,4,6),48))
#
# Identificador da unidade amostral
#
ID = rep(1:48, each=5)
#
# Estudo descritivo
#
library(lattice)
#
X11()
xyplot(N.Palabras~Meses|Tratamento, groups=ID, type='l',
       ylab='No. de palabras corretamente lembradas')
#
# Estrutura de correlacao
#
corr = cor(cbind(time0,time1,time2,time4,time6))
colnames(corr)=c('Inicio','Mes 1','Mes 2','Mes 4','Mes 6')
rownames(corr)=c('Inicio','Mes 1','Mes 2','Mes 4','Mes 6')
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
# Indicam estes graficos que a estrutura mais adequada deve ser constante
#
# Modelo Poisson
#
library(gcmr)
#
mod00.exc = gcmr( N.Palabras ~ Meses + Tratamento,
                marginal = poisson.marg, cormat = cluster.cormat(ID, type = "exc"))
#
summary(mod00.exc)
#
# Analise de residuos
#
library(ggplot2)
#
residuos = residuals(mod00.exc)
#
X11()
ggplot(data=as.data.frame(qqnorm( residuos , plot=F)), mapping=aes(x=x, y=y)) + 
  geom_point() + geom_smooth(method="lm", se=FALSE)
#
shapiro.test(residuos)
#
# Atualizado em: 11/Novembro/2015