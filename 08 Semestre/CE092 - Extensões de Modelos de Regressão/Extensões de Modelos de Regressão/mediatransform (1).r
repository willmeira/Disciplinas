##
## Script para ilustrar a obtenção da média de uma variável (Y) na escala original
## quando foi transformada (Yt) para análise.
## Tipicamente (mas não obrigatóriamente) Yt possui distribuição normal

rm(list=ls())

##
## Caso 1: Y ~ Normal(50, 5^2)
##         (sem transformação)

## Por simulação...
Y <- rnorm(100000, 50, 5)
mean(Y)

rm(Y)
##
## Caso 2: Yt = log(Y) ~ Normal (5, 1.5^2)
##         Y = exp(Yt) ~ LogNormal
##
## Neste caso a esperança (e variância) na escala original possuem expressões conhecidas
## E[Y] = E[exp(Yt)] = exp(E[Yt] + 0,5 V[Yt])
## V[Y] = V[exp(Yt)] = E[exp(Yt)^2] - {E[exp(Yt)]}^2 =
##      = exp(2*E[Yt] + V[Yt])*(exp(V[Yt])-1)
exp(5 + 0.5 * 1.5^2)  # teórica

## Mas pode-se obter por simulação:
Ylog <- rnorm(100000, 5, 1.5)
mean(Ylog)

## .. agora na escala original (de Y)
exp(mean(Ylog)) ## naïve ERRADA !!!! (aproximação que pode ser muito ruim)
Y <- exp(Ylog)
mean(Y)        ## correta, aproximada por simulação

exp(mean(Ylog) + 0.5 * var(Ylog)) ## aproximada por simulação

## Note que a naïve é, na verdade, a estimativa da *mediana*
mean(Ylog)
median(Ylog)
exp(median(Ylog))
exp(mean(Ylog))
median(Y)

## E, de forma mais geral, estatísticas de ordem podem ser transformadas *diretamente*
## Por exemplo, limites de intervalos podem ser transformados diretamente
## para obter intervalos na escala original

## para 0,95:

(YlogIt <- qnorm(c(0.025, 0.975), mean=5, sd=1.5))
(YlogI <- mean(Ylog) + c(-1, 1) * qnorm(0.975) * sd(Ylog))
## verificando a cobertura:
mean(Ylog > YlogI[1] & Ylog < YlogI[2])

## transformação direta dos limites
(YI1 <- exp(YlogI))
mean(Y > YI1[1] & Y < YI1[2]) ## intervalo de quantis, deve ser *exatamente* a mesma da anterior

## +/- 
(YI2 <- mean(Y) + c(-1, 1) * qnorm(0.975) * sd(Y))
## verificando a cobertura:
mean(Y > YI2[1] & Y < YI2[2])  ## cobertura indeterminada, pode ser diferente da nominal

## Mas note que a definição de intervalo não é única!
YlogI
(YlogHDI <- HDInterval:::hdi(Ylog)) ## intervalo alternativo ao de quantis, de maior densidade
plot(density(Ylog))
abline(v=YlogI, lty=2)
abline(v=YlogHDI, lty=2, col=2)

YI1
(YHDI <- HDInterval:::hdi(Y))
plot(density(Y))
plot(density(Y), xlim=c(0, 3500))
abline(v=YI1, lty=2)
abline(v=YHDI, lty=2, col=2)
## verificando a cobertura:
mean(Y > YHDI[1] & Y < YHDI[2])

## Obs: refazer Caso 2 com Ylog ~ N(3, 0.6^2) e notar que as diferenças são bem menos dramáticas

##
## Caso 3: Y = BC^{-1}(Yt) = (lambda Yt + 1)^(1/lambda) ~ ???
##         Yt = BC(Y) = (Y^lambda - 1)/lambda ~ Normal (5, 1.5^2)
##
## Neste caso a esperança (e variância) na escala original
## pode *não ter** expressões conhecidas
rm(list=ls())
## Mas pode-se obter por simulação:
Yt <- rnorm(100000, 5, 1.5)
mean(Yt)

## suponha lambda = 0.35
(0.35 * mean(Yt) + 1)^(1/0.35) ## ERRADA !!!! (aproximação que pode ser muito ruim)
exp((1/0.35)*log(0.35 * mean(Yt) + 1))

## .. agora na escala original (de Y)
Y <- (0.35 * Yt + 1)^(1/0.35)
mean(Y)        ## correta

par(mfrow=c(1,2), mar=c(3,3,.5, .5))
plot(density(Yt))
plot(density(Y))

## Intervalos
## escala transformada
(YtIt <- qnorm(c(0.025, 0.975), mean=5, sd=1.5))
(YtI <- mean(Yt) + c(-1, 1) * qnorm(0.975) * sd(Yt))

## escala original
(YI1 <- (0.35 * YtI + 1)^(1/0.35))  ## cobertura correta, intervalo de quantis
mean(Y > YI1[1] & Y < YI1[2])

(YI2 <- mean(Y) + c(-1, 1) * qnorm(0.975) * sd(Y)) ## aproximado, cobertura incorreta
mean(Y > YI2[1] & Y < YI2[2])

(YI3 <- HDInterval:::hdi(Y)) ## cobertura correta, intervalo de maior densidade
mean(Y > YI3[1] & Y < YI3[2])

par(mfrow=c(1,2), mar=c(3,3,.5, .5))
plot(density(Yt))
abline(v=YtI, lty=2)
plot(density(Y))
abline(v=YI1, lty=2)
abline(v=YI2, lty=2, col=2)
abline(v=YI3, lty=2, col=4)
