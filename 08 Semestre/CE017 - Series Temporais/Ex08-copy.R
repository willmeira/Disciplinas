#
#
# N?mero de linces ca?ados no Canad?, dados anuais entre 1820 e 1934, 
# coletados pelo ec?logo Charles Elton

data(lynx)
lynx

par(mar=c(3,4,1,1))
plot(lynx, pch=16, xlab="", ylab="No. de linces ca?ados", type="b", col="black", main="")

library(astsa)
lag1.plot(lynx,12)

acf2(lynx,20)

#
# MODELOS
#

lynx.lag = ts.intersect(lynx, lynxL1=lag(lynx,-1), dframe=TRUE)
summary(mod01 <- lm(lynx ~ lynxL1, data = lynx.lag, na.action=NULL))

par(mar=c(3,4,1,1), pch=19)
plot(lynx, pch=16, xlab="", ylab="No. de linces ca?ados", type="b", col="black", main="")
lines(ts(fitted(mod01), start=1822, freq=1), col="blue", type="b", lwd=2)

acf2(residuals(mod01))

mod02 = sarima(lynx,1,0,0,0,0,0,0)

par(mar=c(3,4,1,1), pch=19)
plot(lynx, pch=16, xlab="", ylab="No. de linces ca?ados", type="b", col="black", main="")

mod02.fitted = mod02$fit$coef[[2]]+mod02$fit$coef[[1]]*lynx.lag$lynxL1

lines(ts(mod02.fitted, start=1822, freq=1), col="red", type="b", lwd=2)

#
# Existe um comportamento c?clico n?o explicado pelos modelos AR(1) considerados
#

mod03 = sarima(lynx,1,0,1,1,0,0,12, details = FALSE)
mod04 = sarima(lynx,1,0,1,0,0,1,12, details = FALSE)
mod05 = sarima(lynx,1,0,1,1,0,0,16, details = FALSE)
mod06 = sarima(lynx,1,0,1,1,0,0,18, details = FALSE)
mod07 = sarima(lynx,1,0,1,1,0,0,24, details = FALSE)
mod08 = sarima(lynx,1,0,0,1,0,0,10, details = FALSE)
mod09 = sarima(lynx,1,0,1,1,0,0,10, details = FALSE)
mod10 = sarima(lynx,1,0,1,1,0,1,10, details = FALSE)
mod11 = sarima(lynx,1,0,1,2,0,0,10, details = FALSE)
mod12 = sarima(lynx,1,0,2,2,0,1,10, details = FALSE)

modelos = matrix(c(mod03[[4]],mod03[[5]],mod03[[6]],
                     mod04[[4]],mod04[[5]],mod04[[6]],
                     mod05[[4]],mod05[[5]],mod05[[6]],
                     mod06[[4]],mod06[[5]],mod06[[6]],
                     mod07[[4]],mod07[[5]],mod07[[6]],
                     mod08[[4]],mod08[[5]],mod08[[6]],
                     mod09[[4]],mod09[[5]],mod09[[6]],
                     mod10[[4]],mod10[[5]],mod10[[6]],
                     mod11[[4]],mod11[[5]],mod11[[6]],
                     mod12[[4]],mod12[[5]],mod12[[6]]), ncol=3, byrow=TRUE)
colnames(modelos) = c("AIC","AICc","BIC")
rownames(modelos) = c("mod03","mod04","mod05","mod06","mod07","mod08","mod09",
                      "mod10","mod11","mod12")
modelos = as.table(modelos)
modelos

sarima(lynx,1,0,1,1,0,1,10)
mod10$ttable

#
# Mostrando o ajuste
#
library(forecast)

par(mar=c(3,4,1,1), pch=19)
plot(lynx, pch=16, xlab="", ylab="No. de linces ca?ados", type="b", col="black", main="")

mod10_lynx = Arima(y = lynx, order = c(1, 0, 1), 
                   seasonal = list( order = c(1, 0, 1), period = 10))

lines(ts(mod10_lynx$fitted, start=1821, freq=1), col="red", type="b", lwd=2)

#
# Nascidos vivos mensais (ajustados) em milhares nos Estados Unidos, 1948-1979.

data("birth")
par(mar=c(3,4,1,1), pch=19)
plot(birth, pch=16, xlab="", ylab="No. de nascidos vivos mensais", type="b", col="black", main="")

lag1.plot(diff(birth),12)

modelo01 = sarima(diff(birth),0,1,0,1,0,0,12)
modelo02 = sarima(diff(birth),0,1,1,1,0,0,12)
modelo03 = sarima(diff(birth),0,1,2,1,0,0,12)
modelo04 = sarima(diff(birth),0,1,0,1,0,0,12)


# Consumo mensal em MWh de Energ?a El?trica no Brasil 2013-2017
# Anu?rio Estat?stico de Energia El?trica - 2018 
# EPE - Empresa de Pesquisa Energ?tica, www.epe.gov.br
#
dados = read.csv(file = "AnuarioDados.csv", header = TRUE, sep=",")

Consumo = apply(dados$Data, 1, sum)

table(dados$Data)



