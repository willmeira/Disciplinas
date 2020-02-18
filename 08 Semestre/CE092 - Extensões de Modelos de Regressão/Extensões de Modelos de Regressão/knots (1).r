rm(list=ls())
df <- read.table("df02.txt", header=TRUE)
ndf <- data.frame(x = seq(0, 2, len=201))


df <- transform(df, Y = Y1)

with(df, plot(Y ~ x))

regseg.f <- function(k, lines=FALSE, fit=TRUE){
    df <- transform(df, xK = ifelse(x < k, 0, x-k))
    ndf <- transform(ndf, xK = ifelse(x < k, 0, x-k))
    reg <- lm(Y ~ x + xK, data=df)
    if(lines){
        ndf$yK <- predict(reg, newdata=ndf)
        lines(yK ~ x, data=ndf, type="l", lty=2)
    }
    if(fit) return(reg)
    else return(invisible())
}

Ks <- seq(0, 2, by=0.2)
fitK <- lapply(Ks, regseg.f)
names(fitK) <- paste("K",Ks, sep="-")

lLs <- sapply(fitK, logLik)

plot(Ks, lLs, ty="l")
lLs[which.max(lLs)]


ndf <- transform(ndf, xK = ifelse(x < Ks[which.max(lLs)], 0, x-Ks[which.max(lLs)]))

with(df, plot(Y ~ x))

Bfit <- fitK[[which.max(lLs)]]
ndf$Y <- predict(Bfit, newdata=ndf)
lines(Y ~  x, data=ndf, type="l", col=4)
