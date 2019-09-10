#
# O metodo mais difundido para analise de dados longitudinais via regressao marginal e
# certamente o metodo de equacoes de estimacao generalizadas (Liang e Zeger 1986 GEE).
#
# O pacote R geepack fornece uma implementacao de equacoes de estimacao generalizadas. 
#
# O pacote tambem contem o conjunto de dados ohio, um subconjunto de dados de um estudo 
# longitudinal em seis cidades com relacao ao efeito na saude da poluicao atmosferica.
# Este subconjunto inclui informacoes sobre a presenca de chiado na respiracao em criancas 
# observadas anualmente a partir de 7 anos de idade ate 10 anos de idade e se a mae estava 
# fumando ou nao no primeiro ano do estudo:
#
library("geepack")
library("gcmr")
data("ohio")
names(ohio)
#
# Comecamos pela analise simples por GEEs com matriz de correlacao independente e a presenca
# de chiado modelado em termos de idade das criancas, o status de fumo da mãe e sua interacao.
#
gee.ind = geeglm(resp ~ age * smoke, data = ohio, id = id, family = binomial)
summary(gee.ind)
#
# Estimacao por modelos de regressao copula
#
fit.ind = gcmr( cbind(resp, 1-resp) ~ age * smoke, data = ohio,
                  marginal = binomial.marg, cormat = cluster.cormat(id, type = "ind"))
#
summary(fit.ind)
#
# Sob a hipotese de independencia os resultados por GEEs sao equivalentes aos em GCMR.
#
# GEEs e GCMR podem levar a diferentes resultados sob diferentes estruturas de correlacao. 
# No entanto, este nao e o caso para os dados em ohio. Na verdade, os dois metodos de 
# estimacao fornecem conclusoes inferenciais semelhantes sobre os parametros da regressao, 
# mesmo em estruturas de correlacao diferentes. 
#
# Analise de GEE com correlacao AR(1) produz as seguintes estimativas
#
gee.ar1 = geeglm(resp ~ age * smoke, family = binomial, data = ohio, id = id, corstr = "ar1")
summary(gee.ar1)
#
fit.ar1 = gcmr( cbind(resp, 1-resp) ~ age * smoke, data = ohio,
                 marginal = binomial.marg, cormat = cluster.cormat(id, type = "ar1"))
#
summary(fit.ar1)
#
# Uma vantagem da analise de dados longitudinais via copula com respeito ao GEE e que os 
# criterios de informacao padrao podem ser utilizados para seleccionar o melhor o modelo. 
#
AIC(fit.ar1,fit.ind)
#
# O AIC sugere que a estrutura de correlacao mais apropriada e AR(1). 
#
# Analise de residuos
#
library(ggplot2)
#
residuos = residuals(fit.ar1)
#
X11()
ggplot(data=as.data.frame(qqnorm( residuos , plot=F)), mapping=aes(x=x, y=y)) + 
  geom_point() + geom_smooth(method="lm", se=FALSE)
#
# As conclusoes sao que a idade esta negativamente associada com o status de chiado, enquanto a
# associacao com o fumo e muito fraco e a interacao entre idade e fumo nao e significantivo.
#