


library(segmented)
library(dynlm)
library(EnvStats)
library(astsa)

View(rec)
length(rec)
nrow(rec)

plot(cmort, main="Mortalidade Cardiovascular", xlab="", ylab="")

help(cmort)
acf2(cmort)
acf2(cmort,40)

help(ar.ols)
regr = ar.ols(cmort, order=2, demean=FALSE, intercept=TRUE)
fore = predict(regr, n.ahead=100)
par(mfrow = c(1,1),mar=c(4,3,1,1),mgp=c(1.6,.6,0), pch=19)
ts.plot(cmort, fore$pred, col=1:2, xlim=c(1970,1980), lwd=2, ylab="Recrutamento", xlab="Tempo")
 U = fore$pred+fore$se; 
 L = fore$pred-fore$se
 xx = c(time(U), rev(time(U))); yy = c(L, rev(U))
 polygon(xx, yy, border = 8, col = gray(.6, alpha = .2))
 lines(fore$pred, type="p", col=2)
 
 
 
 regr = ar.ols(rec, order=2, demean=FALSE, intercept=TRUE)
  fore = predict(regr, n.ahead=24)
  par(mfrow = c(1,1),mar=c(4,3,1,1),mgp=c(1.6,.6,0), pch=19)
  ts.plot(rec, fore$pred, col=1:2, xlim=c(1980,1990), lwd=2, ylab="Recrutamento", xlab="Tempo")
  U = fore$pred+fore$se; L = fore$pred-fore$se
  xx = c(time(U), rev(time(U))); yy = c(L, rev(U))
  polygon(xx, yy, border = 8, col = gray(.6, alpha = .2))
  lines(fore$pred, type="p", col=2)
  