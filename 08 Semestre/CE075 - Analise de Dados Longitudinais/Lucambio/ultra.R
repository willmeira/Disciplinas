#
# Estes dados sao apresentados em:
# Vonesh, E.F. and Chinchilli, V.M. (1997). Linear and nonlinear models 
# for the analysis of repeated measurements. New York: Marcel Dekker.
#
# Dialisadores de baixo fluxo sao usados para tratar pacientes com doenca 
# renal terminal para remover o excesso de fluido e residuos em seu sangue. 
# Na hemodialise de baixo fluxo, a taxa de ultrafiltracao, variavel "rate" 
# medida em ml/hr, na qual o liquido e removido e pensada para seguir uma 
# relacao linear com o pressao transmembrana, variavel "pressao" mediada 
# em mmHg, aplicada atraves da membrana do dialisador. 
#
# Foi realizado um estudo para comparar a taxa media de ultrafiltracao 
# (a resposta) de dialisadores em tres centros de dialise, onde sao utilizados 
# em pacientes. Um total de 41 dialisadores (unidades) estavam envolvidos. 
# O experimento envolveu o registro da taxa de ultrafiltracao na pressao 
# em diversas transmembranas para cada dialisador.
#
# Variaveis:
#
# Id: indicador do individuo
# pressao: pressao aplicada 
# rate: taxa de ultrafiltracao (resposta)
# centro: centro no qual foi realizado o estudo
#
dados=read.table('ultra.txt',h=T)
attach(dados)
#
# ESTUDO DESCRITIVO
#
library(lattice)
#
par(mar=c(5,4,1,1),pch=19,cex.axis=0.6)
xyplot(rate~pressao|centro,type='b',groups=Id, 
       ylab='Taxa de ultrafiltração (ml/hr)',
       xlab='Pressão transmembrana (mmHg)')
#
Visitas=max(Id)
for(j in 1:max(Id)) Visitas[j]=length(pressao[Id==j])
Visitas
#
# Nesta situacao nao calculamos vetores de medias e matrizes de covariancias 
# para os diferentes instantes de observacao porque os instantes nao dependem 
# de momentos de tempo equidistantes e sim de valores diferentes da pressao 
# aplicada.
#
# MODELOS
#
library(gcmr)
#
ajd1 = gcmr(rate~pressao+factor(centro), 
            marginal = gaussian.marg(link='identity'), 
            cormat = cluster.cormat( id = Id, type = "ar1" ),
            options = list( seed = 71271, nrep = 100 ))
#
ajd2 = gcmr(rate~pressao+factor(centro),
            marginal = gaussian.marg(link='identity'), 
            cormat = cluster.cormat( id = Id, type = "ex" ),
            options = list( seed = 71271, nrep = 100 ))
#
ajd3 = gcmr(rate~pressao+factor(centro),
            marginal = gaussian.marg(link='identity'), 
            cormat = cluster.cormat( id = Id, type = "ma1" ),
            options = list( seed = 71271, nrep = 100 ))
#
ajd4 = gcmr(rate~pressao+factor(centro),
            marginal = gaussian.marg(link='identity'), 
            cormat = cluster.cormat( id = Id, type = "ind" ),
            options = list( seed = 71271, nrep = 100 ))
#
AIC(ajd1,ajd2,ajd3,ajd4)
#
# Observemos os residuos
#
res1 = residuals(ajd1, type=c("conditional"), method=c("mid"))
#
# Normal probability plot
#
library(car)
#
X11()
par(pch=19, cex=0.6)
qqPlot(res1, ylab='Residuos')
#
# Verificando a normalidade
#
shapiro.test(res1)
#
# Aceitamos a normalidade.
#
X11()
par(pch=19, cex=0.6)
xyplot(res1 ~ pressao | centro, groups=Id, type='p', ylab='', 
       xlab='Pressao transmembrana (mmHg)',
       main='Residuos do modelo em ajd1')
#
# Mostrando os resultados (summary)
#
resultados = cbind( coef(ajd1), se(ajd1, type = "hessian") )
colnames(resultados) = c("estimates", "std.err.")
resultados          
#
# Verificando a significancia dos coeficientes
#
X11()
par(mfrow=c(1,3),pch=19, cex=0.6)
profile(ajd1, which = 2)
profile(ajd1, which = 3)
profile(ajd1, which = 4)
#
# Percebemos que todas as vaariaveis sao significativas.
#
# Valores preditos
#
preditos = ajd1$x%*%coef(ajd1)[1:4]
#
# Comportamento dos preditos e observados
#
mini = min(ajd1$y,preditos)
maxi = max(ajd1$y,preditos)
#
# Mostrando a qualidade dos valores estimados ou preditos
#
X11()
par(pch=19, cex=0.6)
plot(ajd1$y,preditos, xlim=c(mini,maxi), ylim=c(mini,maxi),
     xlab='Observados', ylab='Preditos')
#
# Percebemos uma excelente qualidade dos valores preditos
#
# Atualizado em: 28/05/2014
