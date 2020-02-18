##
##
##
df <- list()
(df$x <- sort(round(runif(25, 0, 20), dig=1)))
ndf <- data.frame(x = seq(0, 20, l=2001))
ndf$x[ndf$x == 10] <- NA
err <- round(rnorm(25, m=0, sd=3), dig=1)
##
## Exemplo 1: modelo de uma média
##
df$y1 <- 50 + err
plot(y1 ~  x, data=df)

(fit1 <- lm(y1 ~ 1, data=df))
mean(df$y1)

abline(fit1)

ndf$y1 <- predict(fit1, newdata=ndf)
lines(y1 ~  x, data=ndf, type="l", col=2)

##
## Exemplo 2: modelo de duas médias
##
df$x10 <- ifelse(df$x >= 10, 1, 0)
ndf$x10 <- ifelse(ndf$x >= 10, 1, 0)
df <- transform(df, y2 = 50 - 5 * x10 + err)
plot(y2 ~  x, data=df)

(fit2 <- lm(y2 ~ x10, data=df))
aggregate(y2 ~ x10, data=df, mean)
sum(coef(fit2))

ndf$y2 <- predict(fit2, newdata=ndf)
lines(y2 ~ x, data=ndf, type="l", col=2)

## Obs 1: fazer para 3 ou mais médias

##
## Exemplo 3: regressão linear simples
##
df <- transform(df, y3 = 5 + 2 * df$x + err)
plot(y3 ~  x, data=df)

(fit3 <- lm(y3 ~ x, data=df))

abline(fit3)

ndf$y3 <- predict(fit3, newdata=ndf)
lines(y3 ~  x, data=ndf, type="l", col=2)

##
## Exemplo 4: regressões paralelas
##
df <- transform(df, y4 =  5 + 2 * x - 5*x10 + err)
plot(y4 ~  x, data=df)

(fit4 <- lm(y4 ~ x+x10, data=df))

ndf$y4 <- predict(fit4, newdata=ndf)
lines(y4 ~  x, data=ndf, type="l", col=2)

##
## Exemplo 5: regressões com o mesmo intercepto
##
df <- transform(df, y5 =  5 + 2 * x - 2.5*x*x10 + err)
plot(y5 ~  x, data=df, xlim=c(0, 20))

(fit5 <- lm(y5 ~ x + I(x*x10), data=df))

ndf$y5 <- predict(fit5, newdata=ndf)
lines(y5 ~  x, data=ndf, type="l", col=2)
abline(c(coef(fit5)[1],sum(coef(fit5)[2:3])), lty=3, col=2)

##
## Exemplo 6: regressão por partes
##
df <- transform(df, y6 = 5 + 2 * x - I(10 * x10) + (2.5 * x * x10) + err)
plot(y6 ~  x, data=df)

(fit6 <- lm(y6 ~ x + x10 + I(x*x10), data=df))

ndf$y6 <- predict(fit6, newdata=ndf)
lines(y6 ~  x, data=ndf, type="l", col=2)

##
## Exemplo 7: regressão segmentada
##
df <- transform(df, y7 = 5 + 1 * x + 2.5 * (x-10) * x10 + err)
plot(y7 ~  x, data=df)

(fit7 <- lm(y7 ~ x + I((x-10)*x10), data=df))

ndf$y7 <- predict(fit7, newdata=ndf)
lines(y7 ~  x, data=ndf, type="l", col=2)

## mais enxuto:
df <- transform(df, xK = ifelse(x < 10, 0, x-10))
ndf <- transform(ndf, xK = ifelse(x < 10, 0, x-10))
(fit7 <- lm(y7 ~ x + xK, data=df))
ndf$y7 <- predict(fit7, newdata=ndf)
lines(y7 ~  x, data=ndf, type="l", col=4)


## Obs 2: e se o ponto de corte tivesse que ser estimado?

Ks <- c(5, 7.5, 10, 12.5)
regseg.f <- function(k, lines=FALSE, fit=TRUE){
    df <- transform(df, xK = ifelse(x < k, 0, x-k))
    ndf <- transform(ndf, xK = ifelse(x < k, 0, x-k))
    reg <- lm(y7 ~ x + xK, data=df)
    if(lines){
        ndf$yK <- predict(reg, newdata=ndf)
        lines(yK ~ x, data=ndf, type="l", lty=2)
    }
    if(fit) return(reg)
    else return(invisible())
}

fitK <- lapply(Ks, regseg.f)
fitK
names(fitK) <- paste("K",Ks, sep="-")
fitK

plot(y7 ~  x, data=df)
lapply(Ks, regseg.f, lines=TRUE, fit=FALSE)

(lLs <- sapply(fitK, logLik))
lLs[which.max(lLs)]

##
##
Ks <- seq(2, 18, by=0.1)
fitK <- lapply(Ks, regseg.f)
names(fitK) <- paste("K",Ks, sep="-")

lLs <- sapply(fitK, logLik)
lLs[which.max(lLs)]

plot(Ks, lLs, ty="l")
abline(v=9.2, lty=2)


