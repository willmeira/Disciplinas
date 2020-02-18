local <- rep(1:5, each = 4) 
concentra <- c(170.6,154.7,136.4,153.1,
               68.0,66.4,70.3,71.1,
               151.5,138.0,128.4,118.1,
               153.9,149.1,147.5,103.8,
               83.9,101.4,117.3,114.1)
simDD <- 3 + concentra + rnorm(length(concentra), sd=0.5)
plot(simDD ~ concentra)#, type="n", xlim=c(0, 20),ylim=c(0,180))
abline(h = mean(simDD), lty=2)

points(concentra, pch=19, cex=0.5)


dados <- data.frame(local,concentra)

media <- aggregate(concentra~local,FUN = "mean",data = dados)
desvio <- aggregate(concentra~local,FUN = "sd",data = dados)
minimo <- aggregate(concentra~local,FUN = "min",data = dados)
mediana <- aggregate(concentra~local,FUN = "median",data = dados)
maximo <- aggregate(concentra~local,FUN = "max",data = dados)

cbind(media,desvio$concentra,minimo$concentra,mediana$concentra,maximo$concentra)

plot(concentra~local)

library(lattice)

xyplot(concentra~local,data = base)

m0 <- lm(local~concentra,data = base)
par(mfrow=c(2,2))
plot(m0)
anova(m0)
summary(m0)


## Outros métodos vistos antes que obtem uma f(x) suave
df <- data.frame(x=concentra, y=local)

## polinomial EXPERIMENTAR DIFERENTES GRAUS DO POLINÔMIO
regPol <- lm(y ~ poly(x, degree=8), data=df)
anova(regPol)

## regressão por splines (naturais) EXPERIMENTAR DIFERENTES GRAUS DE LIBERDADE
regS <- lm(y ~ splines:::ns(x, df=8), data=df)
## polinômios locais ## EXPERIMENTAR DIFERENTES LARGURAS DE BANDA (f)
with(df, lines(lowess(y ~ x, f=1/4), col=2))


