  rm(list = ls())

#Ver formas de acesso para roda o R no servidor c3sl.

x<-1:1000000

st1 <- system.time({
    out1 <- numeric(length(X)
    for (i in 1:length(x)){
        out1[i] <- x²
    }
})

      
search()    



########################################################################################
# Aula 30/08/2019 - Geração de números aleatórios uniformes
########################################################################################
## ---- cache=FALSE, include=FALSE-----------------------------------------
source("../setup_knitr.R")


## ------------------------------------------------------------------------
(df <- data.frame(v = 1:4, name = letters[1:4]))
system.time(df[3, 2])
system.time(df[3, "name"])
system.time(df$name[3])


## ------------------------------------------------------------------------
library(microbenchmark)
microbenchmark(df[3, 2], df[3, "name"], df$name[3])


## ------------------------------------------------------------------------
x <- runif(1e6)
microbenchmark(sqrt(x), x^0.5)


## ------------------------------------------------------------------------
## Vetor com uma sequência de 1 a 1.000.000
x <- 1:1000000

## Cria um objeto de armazenamento com o mesmo tamanho do resultado
st1 <- system.time({
  out1 <- numeric(length(x))
  for(i in 1:length(x)){
    out1[i] <- x[i]^2
  }
})
st1

## Cria um objeto de tamanho "zero" e vai "crescendo" esse vetor
st2 <- system.time({
  out2 <- numeric(0)
  for(i in 1:length(x)){
    out2[i] <- x[i]^2
  }
})
st2


## ------------------------------------------------------------------------
## Vetor com uma sequência de 1 a 1.000.000
x <- 1:1000000
## Cria uma função para o primeiro caso
st1 <- function(x) {
  out1 <- numeric(length(x))
  for(i in 1:length(x)){
    out1[i] <- x[i]^2
  }
  return(x)
}
## Cria uma função para o segundo caso
st2 <- function(x) {
  out2 <- numeric(0)
  for(i in 1:length(x)){
    out2[i] <- x[i]^2
  }
  return(x)
}
microbenchmark(st1, st2, times = 1000)


## ---- eval=FALSE---------------------------------------------------------
## ## Caso 1
## Rprof() ## Abre a conexão
## out1 <- numeric(length(x))
## for(i in 1:length(x)){
##     out1[i] <- x[i]^2
## }
## Rprof(NULL) ## Fecha a conexão
## summaryRprof() ## Resumo do profiling
## 
## ## Caso 2
## Rprof()
## out2 <- numeric(0)
## for(i in 1:length(x)){
##     out2[i] <- x[i]^2
## }
## Rprof(NULL)
## summaryRprof()


## ---- eval=FALSE---------------------------------------------------------
## library(profvis)
## ## Caso 1
## profvis({
##     out1 <- numeric(length(x))
##     for(i in 1:length(x)){
##         out1[i] <- x[i]^2
##     }
## })
## 
## ## Caso 2
## profvis({
##     out2 <- numeric(0)
##     for(i in 1:length(x)){
##         out2[i] <- x[i]^2
##     }
## })


## ---- out.width='50%'----------------------------------------------------
set.seed(123)
n <- 1000; b0 <- 10; b1 <- 0.5
x <- rnorm(n, mean = 150, sd = 15)
sigma2 <- 20
y <- b0 + b1*x + rnorm(n, mean = 0, sd = sqrt(sigma2))
plot(x, y)


## ------------------------------------------------------------------------
## Modelo
m <- lm(y ~ x)
## b0 e b1
coef(m)
## sigma^2
summary(m)$sigma^2


## ---- eval=FALSE---------------------------------------------------------
## ## Número de amostras
## r <- 1e4
## ## Número de elementos em cada amostra
## m <- 100
## profvis({ #<<
## ## Vetores para armazenar os resultados
## b0.boot <- numeric(r)
## b1.boot <- numeric(r)
## s2.boot <- numeric(r)
## set.seed(123)
## for(i in 1:r){
##     select <- sample(1:length(y), size = m, replace = TRUE)
##     x.boot <- x[select]
##     y.boot <- y[select]
##     mm <- lm(y.boot ~ x.boot)
##     b0.boot[i] <- coef(mm)[1]
##     b1.boot[i] <- coef(mm)[2]
##     s2.boot[i] <- summary(mm)$sigma^2
## }
## }) #<<
## ## Por curiosidade, veja:
## par(mfrow = c(1, 3))
## hist(b0.boot)
## hist(b1.boot)
## hist(s2.boot)
## par(mfrow = c(1, 1))


## ---- error=TRUE---------------------------------------------------------
stop("Isso é um erro")
warning("Isso é um warning")
message("Isso é uma mensagem")


## ------------------------------------------------------------------------
baskara <- function(a, b, c) {
  denom <- 2 * a
  delta <- b^2 - 4 * a * c
  sqrt_delta <- sqrt(delta)
  x1 <- (-b - sqrt_delta)/denom
  x2 <- (-b + sqrt_delta)/denom
  return(c(x1, x2))
}


## ------------------------------------------------------------------------
baskara(-3, 2, 1)
baskara(0, 2, 1)
baskara(3, 2, 1)


## ---- eval=FALSE---------------------------------------------------------
## traceback()
## # No traceback available


## ------------------------------------------------------------------------
baskara <- function(a, b, c) {
  if(a == 0) stop("Argumento `a` não pode ser zero.") #<<
  denom <- 2 * a
  delta <- b^2 - 4 * a * c
  sqrt_delta <- sqrt(delta)
  x1 <- (-b - sqrt_delta)/denom
  x2 <- (-b + sqrt_delta)/denom
  return(c(x1, x2))
}


## ---- error=TRUE---------------------------------------------------------
baskara(0, 2, 1)

## ---- eval=FALSE---------------------------------------------------------
## traceback()
## # 2: stop("Argumento `a` não pode ser zero.") at #2
## # 1: baskara(0, 2, 1)


## ------------------------------------------------------------------------
baskara <- function(a, b, c) {
  if(a == 0) stop("Argumento `a` não pode ser zero.")
  denom <- 2 * a
  delta <- b^2 - 4 * a * c
  if(delta < 0) stop("Delta é negativo.") #<<
  sqrt_delta <- sqrt(delta)
  x1 <- (-b - sqrt_delta)/denom
  x2 <- (-b + sqrt_delta)/denom
  return(c(x1, x2))
}


## ---- error=TRUE---------------------------------------------------------
baskara(3, 2, 1)

## ---- eval=FALSE---------------------------------------------------------
## traceback()
## # 2: stop("Delta é negativo.") at #5
## # 1: baskara(3, 2, 1)


## ---- eval=FALSE---------------------------------------------------------
## baskara <- function(a, b, c) {
##     denom <- 2 * a
##     delta <- b^2 - 4 * a * c
##     browser() #<<
##     sqrt_delta <- sqrt(delta)
##     x1 <- (-b - sqrt_delta)/denom
##     x2 <- (-b + sqrt_delta)/denom
##     return(c(x1, x2))
## }


## ---- eval=FALSE---------------------------------------------------------
## baskara(3, 2, 1)
## # Called from: baskara(3, 2, 1)
## # Browse[1]> debug at #5: sqrt_delta <- sqrt(delta)
## # Browse[2]>


## ---- eval=FALSE---------------------------------------------------------
## baskara <- function(a, b, c) {
##     denom <- 2 * a
##     delta <- b^2 - 4 * a * c
##     sqrt_delta <- sqrt(delta)
##     x1 <- (-b - sqrt_delta)/denom
##     x2 <- (-b + sqrt_delta)/denom
##     return(c(x1, x2))
## }


## ---- eval=FALSE---------------------------------------------------------
## ## Para iniciar o debug, use:
## debug(baskara)
## ## Para entrar em modo de debug, chame a função com argumentos:
## baskara(3, 2, 1)
## ## Para sair do modo debug da função, faça:
## undebug(baskara)
## ## Ou, para entrar em modo de debug apenas uma vez e sair, use:
## debugonce(baskara)


## ------------------------------------------------------------------------
baskara <- function(a, b, c) {
  if(a == 0) stop("Argumento `a` não pode ser zero.") #<<
  denom <- 2 * a
  delta <- b^2 - 4 * a * c
  if(delta < 0) stop("Delta é negativo.") #<<
  sqrt_delta <- sqrt(delta)
  x1 <- (-b - sqrt_delta)/denom
  x2 <- (-b + sqrt_delta)/denom
  return(c(x1, x2))
}


## ---- error=TRUE---------------------------------------------------------
baskara(0, 2, 1)
er1 <- baskara(0, 2, 1)
er1


## ---- error=TRUE---------------------------------------------------------
try(baskara(0, 2, 1))
er2 <- try(baskara(0, 2, 1))
er2
class(er2)


## ------------------------------------------------------------------------
if(class(er) == "try-error") c(NA, NA)


## ------------------------------------------------------------------------
tryCatch(baskara(0, 2, 1),
         error = function(cmd) c(NA, NA))




########################################################################################
# Aula 06/09/2019 - Geração de números aleatórios uniformes
########################################################################################

# Recomendações de leitura
# https://www.random.org/  https://www.fsf.org/blogs/gnu-press/neug-trng

# Geração de números pseudo aleatórios
naive <- function(n, x0, a) {
  x <- integer(n + 1)
  x[1] <- x0
  for(i in 2:length(x)) {
    x[i] <- (a * x[i - 1])
  }
  return(x[-1])
}
naive(n = 10, x0 = 2, a = 345)
# É um gerador válido, mas não necessariamente bom. Como saber se é um bom gerador?
  
# Geração de números pseudo aleatórios: métodos
# O método gerador padrão no R é o Mersenne-Twister. Por exemplo
runif(10)
# gera uma sequência de números uniformes baseada nesse algoritmo.

# Através da função set.seed(<k>), podemos especificar um número inteiro k como semente (s0)
set.seed(1)
runif(10)

# A função set.seed() também serve para especificar outro método gerador. Por exemplo
set.seed(1, kind = "Knuth-TAOCP-2002")
runif(10)

# É diferente do padrão
set.seed(1, kind = "Mersenne-Twister")
runif(10) 

# Gerador congruencial linear (GCL) - Implementação básica
rcl <- function(n, x0, m, a, c, unit = TRUE) {
  x <- integer(n + 1)
  x[1] <- x0
  for(i in 2:length(x)) {
    x[i] <- (a * x[i - 1] + c) %% m
  }
  if(unit) x <- x/m
  return(x[-1])
}

# Alguns resultados
rcl(n = 10, x0 = 1, m = 1e6, a = 1, c = 1)
rcl(n = 10, x0 = 1, m = 1e6, a = 143, c = 1)
## Congruencial multiplicativo
rcl(n = 10, x0 = 1, m = 1e6, a = 1, c = 0)
rcl(n = 10, x0 = 1, m = 1e6, a = 143, c = 0)
## Periodo = 4 < m = 10
rcl(n = 12, x0 = 7, m = 10, a = 7, c = 7, unit = FALSE)
## Outros períodos
rcl(n = 12, x0 = 8, m = 10, a = 7, c = 7, unit = FALSE)
rcl(n = 12, x0 = 1, m = 10, a = 7, c = 7, unit = FALSE)
rcl(n = 12, x0 = 0, m = 10, a = 7, c = 7, unit = FALSE)
rcl(n = 12, x0 = 2, m = 10, a = 7, c = 7, unit = FALSE)
rcl(n = 12, x0 = 2, m = 100, a = 7, c = 7, unit = FALSE)

# Por exemplo, Park e Miller (1988) definiram m=(2^31)−1, a=7^5 e c=0 como uma boa combinação.
rcl(n = 12, x0 = 7, m = 2^31 - 1, a = 7^5, c = 0, unit = FALSE)
rcl(n = 12, x0 = 7, m = 2^31 - 1, a = 7^5, c = 0, unit = TRUE)

# Gerador Mersenne Twister
# Como a mudança de método gerador interfere nas funções:
RNGkind()
set.seed(1, kind = "Mersenne-Twister")
RNGkind()
runif(5)
rnorm(5)
set.seed(1, kind = "Knuth-TAOCP-2002")
RNGkind()
runif(5)
rnorm(5)
set.seed(1, kind = "Mersenne-Twister")
RNGkind()
runif(5)
rnorm(5)

# Testes de aleatoriedade
x <- rcl(n = 1000, x0 = 1, m = 2^12, a = 125, c = 1, unit = TRUE)
# Testes visuais
plot(x)
hist(x)
# ACF para correlação serial
acf(x)

# Teste de qui-quadrado 
#  Compara frequência observada por classes, com o que seria esperado
#  Testa a hipótese nula de que as frequências observadas e esperadas são iguais

## Divide os dados em 10 classes de igual tamanho
xc <- cut(x, breaks = seq(0, 1, 0.1), include.lowest = TRUE)
## Com 1000 dados, deveriam haver 100 observações em cada classe.
## Estas são as frequências observadas
table(xc)
xc
## A função chisq.test() faz o teste usando esta tabela
chisq.test(x = table(xc))
# Não rejeita a hipótese nula de que as frequências observadas e esperadas são iguais

# Teste de Kolmogorov-Smirnov
#  Teste "de aderência" não paramétrico
#  Compara a distribuição acumulada empírica dos dados com a acumulada de alguma distribuição de referência
#  Calcula a maior distância entre estas duas distribuições
#  Testa a hipótese nula de que a acumulada da distribuição empírica (dos dados) "adere" (é igual) à distribuição teórica

ks.test(x, "punif")
# Não rejeita a hipótese nula de que a distribuição acumulada empírica segue uma uniforme

# Visualização da distribuição acumulada empírica da sequência gerada (linha preta) com uma 
# distribuição acumulada de uma uniforme gerada pela função runif() (linha vermelha - usada como referência).
plot(ecdf(x))
plot(ecdf(runif(1000)), add = TRUE, col = 2)



########################################################################################
# Aula 13/09/2019 
########################################################################################



########################################################################################
# Aula 17/09/2019 
########################################################################################



########################################################################################
# Aula 20/09/2019 
########################################################################################



########################################################################################
# Aula 27/09/2019 - Prova 01
########################################################################################



########################################################################################
# Aula 01/10/2019 - Métodos de Monte Carlo
########################################################################################

## Obtem m valores da U(0,1)
m <- 1000000
x <- runif(m)
## Calcula g(x)
theta.hat <- exp(-x)
## Calcula a média
(m.theta.hat <- sum(theta.hat)/m)
# [1] 0.6313544

## Solução analítica
(theta <- 1 - exp(-1))
# [1] 0.6321206
## Integração numérica no R
integrate(function(x) exp(-x), lower = 0, upper = 1)
# 0.6321206 with absolute error < 7e-15




## Obtem m valores da U(2,4)
m <- 10000
a <- 2; b <- 4
x <- runif(m, min = a, max = b)
## Calcula g(x)
theta.hat <- exp(-x)
## Calcula a média * (b - a)
(m.theta.hat <- (sum(theta.hat)/m) * (b - a))
# [1] 0.1171233

## Nesse caso, podemos obter a solução analítica e integração numérica no R para comparar as estimativas

## Solução analítica
(theta <- exp(-2) - exp(-4))
# [1] 0.1170196
## Integração numérica no R
integrate(function(x) exp(-x), lower = 2, upper = 4)
# 0.1170196 with absolute error < 1.3e-15


## Seguindo o exemplo anterior, vimos que a estimativa da integral é

m.theta.hat
# [1] 0.1171233

## Mas veja que existe uma distribuição de valores para compor esta estimativa média

hist(theta.hat); abline(v = theta, col = 2)

## A variância relacionada ao estimador pode então ser calculada como

## Variancia
(v.theta.hat <- sum((theta.hat - m.theta.hat)^2)/m^2)
# [1] 4.499182e-07
## Erro padrão
(se.theta.hat <- sqrt(v.theta.hat))
# [1] 0.0006707594

## Simula 1000 médias
r <- 1000
res <- matrix(runif(m * r, a, b), nrow = m, ncol = r)
m.theta.sim <- apply(res, 2, function(x) mean(exp(-x)) * (b - a))
hist(m.theta.sim); abline(v = theta, col = 2)


## Media e veriancia
c(m.theta.hat, v.theta.hat)
# [1] 1.171233e-01 4.499182e-07
## Intervalo de confiança (95%)
c(m.theta.hat - 1.96 * se.theta.hat, m.theta.hat + 1.96 * se.theta.hat)
# [1] 0.1158086 0.1184380

## Simula a convergência para o verdadeiro valor conforme aumenta o
## tamanho da amostra
nsamp <- 1e4
set.seed(19)
x <- exp(-runif(nsamp, a, b))
estint <- (cumsum(x) * (b - a))/(1:nsamp)
esterr <- sqrt(cumsum((x - estint)^2))/(1:nsamp)
plot(estint, ylim = c(.05, .15), type = "l",
     xlab = "Tamanho da amostra", ylab = "Estimativa")
abline(h = theta, col = 2)
lines(estint - 1.96 * esterr, lty = 2)
lines(estint + 1.96 * esterr, lty = 2)
rm(ls)

x <- seq(0, 1, .01)
w <- 2
f1 <- exp(-x)
f2 <- (1 / pi) / (1 + x^2)
f3 <- exp(-x) / (1 - exp(-1))
f4 <- 4 / ((1 + x^2) * pi)
g <- exp(-x) / (1 + x^2)
par(mfrow = c(1, 2))
plot(x, g, type = "l", main = "", ylab = "g(x)",
     ylim = c(0,2), lwd = 2)
lines(x, g/g, col = 2, lwd = w)
lines(x, f1, col = 3, lwd = w)
lines(x, f2, col = 4, lwd = w)
lines(x, f3, col = 5, lwd = w)
lines(x, f4, col = 6, lwd = w)
legend("topright", legend = c("g", 0:4),
       col = 1:6, lwd = w, inset = 0.02)
plot(x, g, type = "l", main = "", ylab = "g(x)/f(x)",
     ylim = c(0,3.2), lwd = w)
lines(x, g/f1, col = 3, lwd = w)
lines(x, g/f2, col = 4, lwd = w)
lines(x, g/f3, col = 5, lwd = w)
lines(x, g/f4, col = 6, lwd = w)
legend("topright", legend = c(0:4),
       col = 2:6, lwd = w, inset = 0.02)
par(mfrow = c(1, 1))
#--------------------------------------------------
m <- 10000
theta.hat <- se <- numeric(5)
g <- function(x) {
  ## exp(-x - log(1+x^2)) * (x > 0) * (x < 1)
  exp(-x)/(1 + x^2) * (x > 0) * (x < 1)
}

x <- runif(m)     # usando f0
fg <- g(x)
theta.hat[1] <- mean(fg)
se[1] <- sd(fg)

x <- rexp(m, 1)   # usando f1
fg <- g(x) / exp(-x)
theta.hat[2] <- mean(fg)
se[2] <- sd(fg)

x <- rcauchy(m)   # usando f2
i <- c(which(x > 1), which(x < 0))
x[i] <- 2  # para evitar erros em g(x)
fg <- g(x) / dcauchy(x)
theta.hat[3] <- mean(fg)
se[3] <- sd(fg)

u <- runif(m)     # f3, pelo método da inversa
x <- - log(1 - u * (1 - exp(-1)))
fg <- g(x) / (exp(-x) / (1 - exp(-1)))
theta.hat[4] <- mean(fg)
se[4] <- sd(fg)

u <- runif(m)    # f4, pelo método da inversa
x <- tan(pi * u / 4)
fg <- g(x) / (4 / ((1 + x^2) * pi))
theta.hat[5] <- mean(fg)
se[5] <- sd(fg)

rbind(theta.hat, se/sqrt(m))
#                  [,1]       [,2]        [,3]         [,4]        [,5]
# theta.hat 0.525261009 0.52980810 0.539874509 0.5250382027 0.525325250
#           0.002447843 0.00418581 0.009629678 0.0009686918 0.001408793


########################################################################################
# Aula 04/10/2019 - Métodos de Monte Carlo
########################################################################################
rm(list = ls())
## Obtem m valores da U(0,1)
m <- 10000
x <- runif(m)
## Calcula g(x)
theta.hat <- exp(-x)
## Calcula a média
(m.theta.hat <- sum(theta.hat)/m)
# [1] 0.6337583
## Solução analítica
(theta <- 1 - exp(-1))
# [1] 0.6321206

## Obtem m valores da U(2,4)
m <- 10000
a <- 2; b <- 4
x <- runif(m, min = a, max = b)
## Calcula g(x)
theta.hat <- exp(-x)
## Calcula a média * (b - a)
(m.theta.hat <- (sum(theta.hat)/m) * (b - a))
# [1] 0.1167288
## Solução analítica
(theta <- exp(-2) - exp(-4))
# [1] 0.1170196

## Obtem m valores da U(2,Inf)
m <- 10000
a <- 2; b <- 1e5
x <- runif(m, min = a, max = b)
## Calcula g(x)
theta.hat <- exp(-x)
## Calcula a média * (b - a)
(m.theta.hat <- (sum(theta.hat)/m) * (b - a))
# [1] 0.2770476
## Solução analítica
(theta <- exp(-2) - exp(-1e5))
# [1] 0.1353353
## Usando a exponencial
pexp(2, 1, lower.tail = FALSE)
# [1] 0.1353353


a <- 9.39; b <- 33.67
m <- 10000
x <- runif(m, 0.3, 0.5)
## Calcula g(x)
theta.hat <- dbeta(x, a, b)
## Calcula a média
(m.theta.hat <- (sum(theta.hat)/m) * (0.5 - 0.3))
# [1] 0.1014207
## Solução analítica
pbeta(0.5, a, b) - pbeta(0.3, a, b)
# [1] 0.1018369

m <- 10000
x <- runif(m, 0.2, 1)
## Calcula g(x)
theta.hat <- dbeta(x, a, b)
## Calcula a média
(m.theta.hat <- (sum(theta.hat)/m) * (1 - 0.2))
# [1] 0.585035
## Solução analítica
pbeta(0.2, a, b, lower.tail = FALSE)
# [1] 0.5876479

(M <- optimize(f = function(x) {dexp(x, 1)/dexp(x, .5)},
               interval = c(0, 50), maximum = TRUE)$objective)
# [1] 1.99993

curve(dexp(x, 1), from = 0, to = 10, col = 4, ylim = c(0, 1))
curve(dexp(x, 0.5), from = 0, to = 10, add = TRUE, lty = 2)
curve(M * dexp(x, .5), add = TRUE, lty = 2, lwd = 2)
  legend("topright", legend = c("f(x)", "g(x)", "M g(x)"),
       lty = c(1, 2, 2), col = c(4, 1, 1), lwd = c(1, 1, 2), bty = "n")

## Criando os elementos necessários
f <- function(x) dexp(x, 1)
g <- function(x) dexp(x, 0.5)
## Simula valores de f
N <- 1e5
x <- numeric(N)
i <- 1
j <- 1
while(i <= N) {
  y <- rexp(1, 0.5)
  u <- runif(1)
  r <- f(y)/(M * g(y))
  if(u < r) {
    x[i] <- y
    i <- i + 1
  }
  j <- j + 1
}
length(x)
# [1] 100000
hist(x, freq = FALSE, ylim = c(0, 1))
curve(dexp(x, 1), add = TRUE, from = 0, to = 10, col = 2)

sum(x > 0 & x <= max(x))/N
# [1] 1

## Probabilidade empírica
sum(x >= 2 & x <= 4)/N
# [1] 0.117
## Teórica
pexp(4, 1) - pexp(2, 1)
# [1] 0.1170196

## Probabilidade empírica
sum(x >= 2)/N
# [1] 0.13488
## Teórica
pexp(2, 1, lower.tail = FALSE)
# [1] 0.1353353

a <- 9.39; b <- 33.67
(M <- optimize(f = function(x) {dbeta(x, a, b)/dunif(x)},
               interval = c(0, 1), maximum = TRUE)$objective)
# [1] 6.426808

curve(dbeta(x, a, b), from = 0, to = 1, col = 4, ylim = c(0, 7))
curve(dunif(x), from = 0, to = 1, add = TRUE, lty = 2)
curve(M * dunif(x), add = TRUE, lty = 2, lwd = 2)
legend("right", legend = c("f(x)", "g(x)", "M g(x)"),
       lty = c(1, 2, 2), col = c(4, 1, 1), lwd = c(1, 1, 2), bty = "n")


## Criando os elementos necessários
f <- function(x, a, b) dbeta(x, a, b)
g <- function(x) dunif(x)
## Simula valores de f
N <- 1e5
x <- numeric(N)
i <- 1
j <- 1
while(i <= N) {
  y <- runif(1)
  u <- runif(1)
  r <- f(y, a, b)/(M * g(y))
  if(u < r) {
    x[i] <- y
    i <- i + 1
  }
  j <- j + 1
}
length(x)
# [1] 100000
sum(x >= 0 & x <= 1)/N
# [1] 1
hist(x, freq = FALSE, ylim = c(0, 7))
curve(dbeta(x, a, b), add = TRUE, from = 0, to = 1, col = 2)

## P[0.3 < X < 0.5]
sum(x > 0.3 & x < 0.5)/N
# [1] 0.10234
pbeta(0.5, a, b) - pbeta(0.3, a, b)
# [1] 0.1018369
## P[X > 0.2]
sum(x > 0.2)/N
# [1] 0.58714
pbeta(0.2, a, b, lower.tail = FALSE)
# [1] 0.5876479

## Define funções
g <- function(x) exp(-x) * (x >= 2)
f <- function(x) dexp(x, 0.5) # Proposta é Exp(0.5)
## 1. Amostra da proposta
m <- 1e5
x <- rexp(m, 0.5)
## 2. Calcula a média da razão
mean(g(x)/f(x))
# [1] 0.1358222
## Teórico
pexp(2, 1, lower.tail = FALSE)
# [1] 0.1353353

## Define funções
g <- function(x) exp(-x) * (x >= 0)
f <- function(x) dexp(x, 0.5)
## 1. Amostragem
m <- 1e5
x <- rexp(m, 0.5)
## 2. Calcula pesos
w <- g(x)/f(x)
w.norm <- w/sum(w)
## 3. Re-amostragem por importância
k <- 1e4
## Amostra considerando os pesos normalizados para cada X
g.sample <- sample(x, size = k, replace = TRUE, prob = w.norm)
## Verifica a distribuição da amostra
hist(g.sample, freq = FALSE, ylim = c(0, 1))
curve(dexp(x, 1), add = TRUE, from = 0, to = 10, col = 2)
plot(x, w.norm)


## P[2 < X < 4]
sum(g.sample >= 2 & g.sample <= 4)/k
# [1] 0.1204
## Teórica
pexp(4, 1) - pexp(2, 1)
# [1] 0.1170196
## P[X > 2]
sum(g.sample > 2)/k
# [1] 0.1361
pexp(2, 1, lower.tail = FALSE)
  # [1] 0.1353353

## Define funções
a <- 9.39; b <- 33.67
g <- function(x, a, b) dbeta(x, a, b)
f <- function(x) dunif(x) # usando U(0,1)
## 1. Amostragem
m <- 1e5
x <- runif(m)
## 2. Calcula pesos
w <- g(x, a, b)/f(x)
w.norm <- w/sum(w)
## 3. Re-amostragem por importância
k <- 1e4
## Amostra considerando os pesos normalizados para cada X
g.sample <- sample(x, size = k, replace = TRUE, prob = w.norm)
## Verifica a distribuição da amostra
hist(g.sample, freq = FALSE, ylim = c(0, 7))
curve(dbeta(x, a, b), add = TRUE, from = 0, to = 1, col = 2)
plot(x, w.norm)



## P[0.3 < X < 0.5]
sum(g.sample > 0.3 & g.sample < 0.5)/k
# [1] 0.102
pbeta(0.5, a, b) - pbeta(0.3, a, b)
# [1] 0.1018369
## P[X > 0.2]
sum(g.sample > 0.2)/k
# [1] 0.584
pbeta(0.2, a, b, lower.tail = FALSE)
# [1] 0.5876479

summary(g.sample)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.03969 0.17313 0.21344 0.21780 0.25754 0.50699

  quantile(g.sample, probs = c(.025, .975))
#      2.5%     97.5% 
# 0.1094833 0.3526790


########################################################################################
# Aula 08/10/2019 - Correção de prova e comentários sobre atividades.
########################################################################################

########################################################################################
# Aula 11/10/2019 - MCMC: Metropolis random walk e amostrador independente. 
########################################################################################

  ## Simulação de um random walk
  rw1 <- function(T, x1, seed) {
    x <- numeric(T)
    x[1] <- x1
    set.seed(seed)
    e <- rnorm(T)
    for(t in 1:(T - 1)) {
      x[t + 1] <- x[t] + e[t]
    }
    return(x)
  }
  par(mfrow = c(2, 1))
  plot(rw1(T = 100, x1 = 10, seed = 1), type = "l",
       xlab = "Tempo", ylab = "x")
  plot(rw1(T = 1000, x1 = 10, seed = 1), type = "l",
       xlab = "Tempo", ylab = "x")
  par(mfrow = c(1, 1))
  
# Exemplo com uniforme  
f <- function(x) dnorm(x, 0, 1)
delta <- 0.5
N <- 500
x <- numeric(N)
x[1] <- 0
set.seed(2019-10-11)
for(i in 2:N) {
  z <- runif(1, -delta, delta)
  y <- x[i - 1] + z
  alpha <- min(f(y)/f(x[i - 1]), 1)
  u <- runif(1)
  if(u <= alpha) {
    x[i] <- y
  } else {
    x[i] <- x[i - 1]
  }
}
plot(x, type = "l")  
  
  
# Veja o que acontece se aumentarmos o valor de δ
  
f <- function(x) dnorm(x, 0, 1)
delta <- 2
N <- 500
x2 <- numeric(N)
x2[1] <- 0
set.seed(2019-10-11)
for(i in 2:N) {
  z <- runif(1, -delta, delta)
  y <- x2[i - 1] + z
  alpha <- min(f(y)/f(x2[i - 1]), 1)
  u <- runif(1)
  if(u <= alpha) {
    x2[i] <- y
  } else {
    x2[i] <- x2[i - 1]
  }
}
plot(x2, type = "l")

  
# Compara a distribuição das amostras com a distribuição teórica
  
par(mfrow = c(1, 2))
plot(ecdf(x))
curve(pnorm(x), add = TRUE, col = 2)
plot(ecdf(x2))
curve(pnorm(x), add = TRUE, col = 2)
par(mfrow = c(1, 1))
  
########################################################################################
# Aula 15/10/2019 - Métodos de Monte Carlo via Cadeias de Markov
########################################################################################
  estados <- c("PR", "RS", "SC")
  P <- matrix(c(.3, .3, .4, .4, .4, .2, .5, .3, .2),
              byrow = TRUE, ncol = 3)
  dimnames(P) <- list(estados, estados); P
  #     PR  RS  SC
  # PR 0.3 0.3 0.4
  # RS 0.4 0.4 0.2
  # SC 0.5 0.3 0.2
  rowSums(P)
  # PR RS SC 
  #  1  1  1
  colSums(P)
  #  PR  RS  SC 
  # 1.2 1.0 0.8
  ## DAG
  diagram::plotmat(t(P), relsize = .75)

  
pi0 <- c(0, 0, 1)
(pi1 <- pi0 %*% P)
#       PR  RS  SC
# [1,] 0.5 0.3 0.2
  
  
(pi2 <- pi1 %*% P)
#        PR   RS  SC
# [1,] 0.37 0.33 0.3  

install.packages("expm")
library(expm) # potencia de matriz
# Loading required package: Matrix
# 
# Attaching package: 'expm'
# The following object is masked from 'package:Matrix':
# 
#     expm
pi0 %*% (P %^% 50)
#             PR        RS        SC
# [1,] 0.3888889 0.3333333 0.2777778  


pi0 %*% (P %^% 5)
#           PR      RS      SC
# [1,] 0.38905 0.33333 0.27762
pi0 %*% (P %^% 10)
#             PR        RS        SC
# [1,] 0.3888888 0.3333333 0.2777778
pi0 %*% (P %^% 1e2)
#             PR        RS        SC
# [1,] 0.3888889 0.3333333 0.2777778
pi0 %*% (P %^% 1e3)
#             PR        RS        SC
# [1,] 0.3888889 0.3333333 0.2777778
pi0 %*% (P %^% 1e4)
#             PR        RS        SC
# [1,] 0.3888889 0.3333333 0.2777778  
  
## Distribuição estacionária
(pi_e <- pi0 %*% (P %^% 1e4))
#             PR        RS        SC
# [1,] 0.3888889 0.3333333 0.2777778
## Propriedade da distribuição estacionária
pi_e %*% P
#             PR        RS        SC
# [1,] 0.3888889 0.3333333 0.2777778  
  

########################################################################################
# Aula 22/10/2019 - Métodos de Monte Carlo em inferência estatística
########################################################################################
rm(list = ls())
# 2.1 Exemplo: Estimação de Monte Carlo de um erro padrão
  N <- 1000
  g <- numeric(N)
  str(g)
  for (i in 1:N) {
    x <- rnorm(2)
    g[i] <- abs(x[1] - x[2])
  }
  (est <- mean(g))
  # [1] 1.131141
  hist(g)

# Em uma amostra de Monte Carlo, o tamanho da amostra é N, por isso, o erro padrão da estimativa será
  ## Variância da distribuição amostral
  sum((g - est)^2)/(N - 1)
  # [1] 0.7158632
  var(g)
  # [1] 0.7158632
  ## Erro padrão = desvio padrão da distribuição amostral
  sqrt(sum((g - est)^2))/(N - 1)
  # [1] 0.02676901
  sd(g)/sqrt(N - 1)
  # [1] 0.02676901

# 2.2 Exemplo: Erro Quadrático Médio  
  ## Tamanho da amostra
  n <- 10
  ## Número de repetições
  N <- 1000
  tmean1 <- numeric(N)
  for (i in 1:N) {
    x <- sort(rnorm(n))
    tmean1[i] <- sum(x[2:(n - 1)])/(n - 2)
  }
  ## Estimativa pontual
  (m.tmean1 <- mean(tmean1))
  # [1] 0.006333479
  ## Variância
  sum((tmean1 - m.tmean1)^2)/(N - 1)
  # [1] 0.05371879
  ## Erro padrão = desvio padrão da distribuição amostral
  sqrt(sum((tmean1 - m.tmean1)^2))/(N - 1)
  # [1] 0.007332978
  ## EQM
  (eqm1 <- mean(tmean1^2))
  # [1] 0.05370518
  hist(tmean1)  

  
# Repare que a mediana também é uma média aparada: ela “apara” todos os valores das caudas 
# menos um (quando n for ímpar), ou dois (quando n for par), e calcula a média. Portanto, 
# podemos reptir o mesmo procedimento para a mediana.

  n <- 20
  N <- 1000
  tmean2 <- numeric(N)
  for (i in 1:N) {
    x <- sort(rnorm(n))
    tmean2[i] <- median(x)
  }
  ## Estimativa pontual
  (m.tmean2 <- mean(tmean2))
  # [1] 0.006313864
  ## Variância
  sum((tmean2 - m.tmean2)^2)/(N - 1)
  # [1] 0.0701361
  ## Erro padrão = desvio padrão da distribuição amostral
  sqrt(sum((tmean2 - m.tmean2)^2))/(N - 1)
  # [1] 0.008378921
  ## EQM
  (eqm2 <- mean(tmean2^2))
  # [1] 0.07010583
  hist(tmean2)
  

# Agora podemos comparar qual dos dois estimadores é o melhor para a média populacional, através dos EQMs.
  ## Qual dos dois possui menor EQM
  eqm1 <= eqm2
  # [1] TRUE
  ## Eficiência relativa
  eqm1/eqm2
  # [1] 0.7660587
  

# 2.3 Exemplo: Estimativa de nível de confiança
  n <- 20
  alpha <- .05
  x <- rnorm(n, mean = 0, sd = 2)
  (UCL <- (n - 1) * var(x) / qchisq(alpha, df = n - 1))
  # [1] 4.388738
  
  
# A proporção amostral de intervalos que contém θ é então uma estimativa de Monte Carlo do 
# verdadeiro nível de confiança (1−α). (Note aqui o uso da função replicate() no lugar do for()).
  n <- 20
  m <- 1000
  alpha <- .05
  UCL <- replicate(m, expr = {
    x <- rnorm(n, mean = 0, sd = 2)
    (n - 1) * var(x) / qchisq(alpha, df = n - 1)
  })
  ## Número de intervalos que contém sigma^2 = 4
  sum(UCL > 4)
  # [1] 943
  ## Nível de confiança empírico
  sum(UCL > 4)/N
  # [1] 0.943
  mean(UCL > 4)
  # [1] 0.943
  
# Veja que o nível de confiança empírico é muito próximo do nível de confiança teórico, de 95%.
# Para 100 intervalos calculados, podemos visualizar o procedimento:
    
  UCL.sim <- replicate(100, expr = {
      x <- rnorm(n, mean = 0, sd = 2)
      (n - 1) * var(x) / qchisq(alpha, df = n - 1)
    })
  plot(NULL, NULL, xlim = c(0, 100), ylim = c(0, max(UCL.sim)), ylab = "")
  segments(1:100,
           0,
           1:100,
           UCL.sim)
  abline(h = 4, col = 2)
  

# Por exemplo, suponha que ao invés de normal, os dados foram obtidos a partir de uma população
# que segue uma distribuição χ2 com 2 graus de liberdade, que também possui variância 4, mas
# claramente não é normal.
  curve(dchisq(x, df = 2), to = 10)
  
  
# Podemos repetir o procedimento acima, substituindo as amostras de X da normal pela χ2(2) e 
# verificar qual seria então o nível de confiança empírico.
  n <- 20
  m <- 1000
  alpha <- .05
  UCL <- replicate(m, expr = {
    x <- rchisq(n, df = 2)
    (n - 1) * var(x) / qchisq(alpha, df = n - 1)
  })
  ## Número de intervalos que contém sigma^2 = 4
  sum(UCL > 4)
  # [1] 788
  ## Nível de confiança empírico
  sum(UCL > 4)/N
  # [1] 0.788
  mean(UCL > 4)
  # [1] 0.788  
  
  
# Visualmente temos:
  UCL.sim <- replicate(100, expr = {
      x <- rchisq(n, df = 2)
      (n - 1) * var(x) / qchisq(alpha, df = n - 1)
    })
  plot(NULL, NULL, xlim = c(0, 100), ylim = c(0, max(UCL.sim)), ylab = "")
  segments(1:100,
           0,
           1:100,
           UCL.sim)
  abline(h = 4, col = 2)
  
  
  
########################################################################################
# Aula 25/10/2019 - 
########################################################################################



########################################################################################
# Aula 29/10/2019 - 
########################################################################################



########################################################################################
# Aula 01/11/2019 - Métodos de reamostragem
########################################################################################
  library(lattice)
  library(latticeExtra)
  # Loading required package: RColorBrewer
  library(plyr)
  
  
  ## Exemplo adaptado de Manly (1997)
  ## Comparação do comprimento da mandíbula de chacais machos e fêmeas
  set.seed(2)
  machos <- c(120, 107, 110, 116, 114, 111, 113, 117, 114, 112)
  ## Simula diferença para as femeas
  femeas <- rnorm(10, mean(machos) - 2, sd = sd(machos))
  da <- data.frame(comp = c(machos, femeas),
                   sexo = c(rep("M", 10), rep("F", 10)))
  densityplot(~comp, groups = sexo, data = da, auto.key = TRUE)
  
  
  ## Média por sexo
  tapply(da$comp, da$sexo, mean)
  #       F       M 
  # 112.185 113.400
  ## Diferença das médias
  diff(tapply(da$comp, da$sexo, mean))
  #        M 
  # 1.214975
  
  ## Média de cada sexo
  (m1 <- mean(machos))
  # [1] 113.4
  (m2 <- mean(femeas))
  # [1] 112.185
  ## Diferença entre as médias amostrais
  (med.amostral <- m1 - m2)
  # [1] 1.214975
  ## Calcula o desvio padrão ponderado
  n1 <- length(machos)
  v1 <- var(machos)
  n2 <- length(femeas)
  v2 <- var(femeas)
  (s.pond <- sqrt(((n1 - 1) * v1 + (n2 - 1) * v2)/(n1 + n2 - 2)))
  # [1] 3.690024
  
  ## Teste de hipótese para
  ## H0: mu1 <= mu2
  ## Ha: mu1 > mu2
  mu0 <- 0
  t.test(x = machos, y = femeas, alternative = "greater",
         var.equal = TRUE, mu = mu0)
  # 
  #   Two Sample t-test
  # 
  # data:  machos and femeas
  # t = 0.73625, df = 18, p-value = 0.2355
  # alternative hypothesis: true difference in means is greater than 0
  # 95 percent confidence interval:
  #  -1.646627       Inf
  # sample estimates:
  # mean of x mean of y 
  #   113.400   112.185
  ## Estatística de teste
  (tcalc <- (m1 - m2)/(s.pond * sqrt(1/n1 + 1/n2)))
  # [1] 0.7362465
  ## Valor crítico
  (tcrit <- qt(.025, df = n1 + n2 - 2, lower.tail = FALSE))
  # [1] 2.100922
  ## p-valor
  pt(tcalc, df = n1 + n2 - 2, lower.tail = FALSE)
  # [1] 0.2355338
  
  ## Teste por simulação via Bootstrap
  N <- 10000
  ## Se a hipótese nula é verdadeira, então o comprimento das mandíbulas
  ## de machos e fêmeas são provenientes da mesma poplação, e portanto
  ## podem ser pensados como uma única amostra.
  amostra <- c(machos, femeas)
  ## Amostra COM REPOSIÇÃO os 20 valores, e atribui aleatoriamente 10 para
  ## cada grupo (macho ou fêmea). Se forem de fato da mesma população,
  ## então as diferenças entre as médias devem ser próximas de zero.
  am <- replicate(
    N, diff(tapply(sample(amostra, replace = TRUE), da$sexo, mean))
  )
  ## Visualização
  hist(am, main = "Distribuição amostral")
  abline(v = med.amostral, col = 2)
  
  
  ## p-valor empírico
  sum(am >= med.amostral)/N
  # [1] 0.2174
  
  ## Exemplo adaptado de Manly (1997)
  ## Comparação do comprimento da mandíbula de chacais machos e fêmeas
  machos <- c(120, 107, 110, 116, 114, 111, 113, 117, 114, 112)
  femeas <- c(110, 111, 107, 108, 110, 105, 107, 106, 111, 111)
  da <- data.frame(comp = c(machos, femeas),
                   sexo = c(rep("M", 10), rep("F", 10)))
  densityplot(~comp, groups = sexo, data = da, auto.key = TRUE)
  
  
  
  ## Média por sexo
  tapply(da$comp, da$sexo, mean)
  #     F     M 
  # 108.6 113.4
  ## Diferença das médias
  diff(tapply(da$comp, da$sexo, mean))
  #   M 
  # 4.8
  
  ## Média de cada sexo
  (m1 <- mean(machos))
  # [1] 113.4
  (m2 <- mean(femeas))
  # [1] 108.6
  ## Diferença entre as médias amostrais
  (med.amostral <- m1 - m2)
  # [1] 4.8
  ## Calcula o desvio padrão ponderado
  n1 <- length(machos)
  v1 <- var(machos)
  n2 <- length(femeas)
  v2 <- var(femeas)
  (s.pond <- sqrt(((n1 - 1) * v1 + (n2 - 1) * v2)/(n1 + n2 - 2)))
  # [1] 3.080404
  
  ## Teste de hipótese para
  ## H0: mu1 <= mu2
  ## Ha: mu1 > mu2
  mu0 <- 0
  t.test(x = machos, y = femeas, alternative = "greater",
         var.equal = TRUE, mu = mu0)
  # 
  #   Two Sample t-test
  # 
  # data:  machos and femeas
  # t = 3.4843, df = 18, p-value = 0.001324
  # alternative hypothesis: true difference in means is greater than 0
  # 95 percent confidence interval:
  #  2.411156      Inf
  # sample estimates:
  # mean of x mean of y 
  #     113.4     108.6
  ## Estatística de teste
  (tcalc <- (m1 - m2)/(s.pond * sqrt(1/n1 + 1/n2)))
  # [1] 3.484324
  ## Valor crítico
  (tcrit <- qt(.025, df = n1 + n2 - 2, lower.tail = FALSE))
  # [1] 2.100922
  ## p-valor
  pt(tcalc, df = n1 + n2 - 2, lower.tail = FALSE)
  # [1] 0.001323634
  
  ## Teste por simulação via Bootstrap
  N <- 10000
  ## Se a hipótese nula é verdadeira, então o comprimento das mandíbulas
  ## de machos e fêmeas são provenientes da mesma poplação, e portanto
  ## podem ser pensados como uma única amostra.
  amostra <- c(machos, femeas)
  ## Amostra COM REPOSIÇÃO os 20 valores, e atribui aleatoriamente 10 para
  ## cada grupo (macho ou fêmea). Se forem de fato da mesma população,
  ## então as diferenças entre as médias devem ser próximas de zero.
  am <- replicate(
    N, diff(tapply(sample(amostra, replace = TRUE), da$sexo, mean))
  )
  ## Visualização
  hist(am, main = "Distribuição amostral")
  abline(v = med.amostral, col = 2)
  
  
  ## p-valor empírico
  sum(am >= med.amostral)/N
  # [1] 0.0025
  
  
##   Uma nota de precaução
  
  ## Amostra de uma Poisson(2)
  x <- c(2, 2, 1, 1, 5, 4, 4, 3, 1, 2)
  ## Distribuição empírica
  prop.table(table(x))
  # x
  #   1   2   3   4   5 
  # 0.3 0.3 0.1 0.2 0.1
  ## Distribuição empírica acumulada
  cumsum(prop.table(table(x)))
  #   1   2   3   4   5 
  # 0.3 0.6 0.7 0.9 1.0
  
  ## Amostra via bootstrap
  ## Um passo
  am <- sample(x, replace = TRUE)
  prop.table(table(am))
  # am
  #   1   2   4   5 
  # 0.1 0.3 0.5 0.1
  cumsum(prop.table(table(am)))
  #   1   2   4   5 
  # 0.1 0.4 0.9 1.0
  
  ## B passos
  B <- 1000
  am <- sample(x, size = B, replace = TRUE)
  prop.table(table(am))
  # am
  #     1     2     3     4     5 
  # 0.308 0.303 0.084 0.206 0.099
  cumsum(prop.table(table(am)))
  #     1     2     3     4     5 
  # 0.308 0.611 0.695 0.901 1.000
  
  ## Qual o problema então?
  ## Distribuição empírica
  plot(0:5, c(0, prop.table(table(am))), type = "h")
  ## Distribuição teórica
  points((0:5) + .1, dpois(0:5, 2), type = "h", col = 2)
  
  ## 2 Estimativa de erro padrão via bootstrap ---------------------------------------------
 
  
   ## Estimativa de erro padrão via bootstrap
  library(bootstrap) # para carregar os dados
  ## Uma amostra dos dados originais
  str(law)
  # 'data.frame': 15 obs. of  2 variables:
  #  $ LSAT: num  576 635 558 578 666 580 555 661 651 605 ...
  #  $ GPA : num  3.39 3.3 2.81 3.03 3.44 3.07 3 3.43 3.36 3.13 ...
  plot(law$LSAT, law$GPA)
  
  cor(law$LSAT, law$GPA)
  # [1] 0.7763745
  ## Dados originais
  str(law82)
  # 'data.frame': 82 obs. of  3 variables:
  #  $ School: num  1 2 3 4 5 6 7 8 9 10 ...
  #  $ LSAT  : num  622 542 579 653 606 576 620 615 553 607 ...
  #  $ GPA   : num  3.23 2.83 3.24 3.12 3.09 3.39 3.1 3.4 2.97 2.91 ...
  plot(law82$LSAT, law82$GPA)


  cor(law82$LSAT, law82$GPA)
  # [1] 0.7599979
  
  ## Definições
  B <- 200
  n <- nrow(law)
  R <- numeric(B)
  
  ## Bootstrap para a estimativa do erro padrão do R (correlação amostral)
  for (b in 1:B) {
    i <- sample(1:n, size = n, replace = TRUE)
    LSAT <- law$LSAT[i]
    GPA <- law$GPA[i]
    R[b] <- cor(LSAT, GPA)
  }
  
  ## Resultado
  mean(R)
  # [1] 0.7722927
  (se.R <- sd(R))
  # [1] 0.132016
  hist(R)
  
  
  ## Usando a função boot::boot()
  
  ## Define a função que calcula a estatística de interesse
  r <- function(x, i) {
    cor(x[i, 1], x[i, 2])
  }
  
  ## Roda o processo
  library(boot)
  # 
  # Attaching package: 'boot'
  # The following object is masked from 'package:lattice':
  # 
  #     melanoma
  obj <- boot(data = law, statistic = r, R = 2000)
  obj
  # 
  # ORDINARY NONPARAMETRIC BOOTSTRAP
  # 
  # 
  # Call:
  # boot(data = law, statistic = r, R = 2000)
  # 
  # 
  # Bootstrap Statistics :
  #      original       bias    std. error
  # t1* 0.7763745 -0.004350115   0.1331296
  str(obj)
  # List of 11
  #  $ t0       : num 0.776
  #  $ t        : num [1:2000, 1] 0.926 0.698 0.642 0.586 0.8 ...
  #  $ R        : num 2000
  #  $ data     :'data.frame':    15 obs. of  2 variables:
  #   ..$ LSAT: num [1:15] 576 635 558 578 666 580 555 661 651 605 ...
  #   ..$ GPA : num [1:15] 3.39 3.3 2.81 3.03 3.44 3.07 3 3.43 3.36 3.13 ...
  #  $ seed     : int [1:626] 10403 432 1781087344 386800285 1300146352 -1043896737 -961725389 1610237524 1487280467 -2089005861 ...
  #  $ statistic:function (x, i)  
  #   ..- attr(*, "srcref")= 'srcref' int [1:8] 4 6 6 1 6 1 4 6
  #   .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x11af6310> 
  #  $ sim      : chr "ordinary"
  #  $ call     : language boot(data = law, statistic = r, R = 2000)
  #  $ stype    : chr "i"
  #  $ strata   : num [1:15] 1 1 1 1 1 1 1 1 1 1 ...
  #  $ weights  : num [1:15] 0.0667 0.0667 0.0667 0.0667 0.0667 ...
  #  - attr(*, "class")= chr "boot"
  #  - attr(*, "boot_type")= chr "boot"
  plot(obj)
  
  
  ## Acessa os valores calculados
  y <- as.vector(obj$t)
  mean(y)
  # [1] 0.7720244
  sd(y)
  # [1] 0.1331296
  
  
  ## Usando a função bootstrap::bootstrap()
  
  ## Define a função que calcula a estatística
  r <- function(x, xdata) {
    cor(xdata[x, 1], xdata[x, 2])
  }
  
  ## Procedimento
  n <- nrow(law)
  obj2 <- bootstrap(x = 1:n, nboot = 2000, theta = r, law)
  mean(obj2$thetastar)
  # [1] 0.7729704
  sd(obj2$thetastar)
  # [1] 0.1341259
  
  
  ## Estimativa do viés via bootstrap
  
  ## Estatística amostral
  (theta.hat <- cor(law$LSAT, law$GPA))
  # [1] 0.7763745
  
  ## Definições
  B <- 2000
  n <- nrow(law)
  theta.b <- numeric(B)
  
  for (b in 1:B) {
    i <- sample(1:n, size = n, replace = TRUE)
    LSAT <- law$LSAT[i]
    GPA <- law$GPA[i]
    theta.b[b] <- cor(LSAT, GPA)
  }
  
  ## Viés
  mean(theta.b) - theta.hat
  # [1] -0.003264248
  
  
  
  
  
  





########################################################################################
# Aula 05/11/2019 - 4 Intervalos de confiança via Bootstrap
########################################################################################

#  A função boot::boot.ci() calcula estes três tipos de intervalos
  
  ## Exemplo para correlação
  
  ## Define a função que calcula a estatística de interesse
  r <- function(x, i) {
    cor(x[i, 1], x[i, 2])
  }
  
  ## Roda o processo
  boot.obj <- boot(data = law, statistic = r, R = 2000)
  ## Resumo
  boot.obj
  # 
  # ORDINARY NONPARAMETRIC BOOTSTRAP
  # 
  # 
  # Call:
  # boot(data = law, statistic = r, R = 2000)
  # 
  # 
  # Bootstrap Statistics :
  #      original       bias    std. error
  # t1* 0.7763745 -0.002348834   0.1330581
  ## Estatśitica amostral
  boot.obj$t0
  # [1] 0.7763745
  ## Distribuição das estimativas de bootstrap
  plot(boot.obj)


  boot.ci(boot.obj, type = c("basic", "norm", "perc"))
  # BOOTSTRAP CONFIDENCE INTERVAL CALCULATIONS
  # Based on 2000 bootstrap replicates
  # 
  # CALL : 
  # boot.ci(boot.out = boot.obj, type = c("basic", "norm", "perc"))
  # 
  # Intervals : 
  # Level      Normal              Basic              Percentile     
  # 95%   ( 0.5179,  1.0395 )   ( 0.5896,  1.1062 )   ( 0.4465,  0.9631 )  
  # Calculations and Intervals on Original Scale
  
  ## Calcule intervalos manualmente
  ## Define intervalo com alpha = 0.05
  alpha <- c(.025, .975)
  
  ## Normal
  (theta.hat <- boot.obj$t0)
  # [1] 0.7763745
  (se.theta <- sd(boot.obj$t))
  # [1] 0.1330581
  theta.hat + qnorm(alpha) * se.theta
  # [1] 0.5155853 1.0371636
  ## Note que é diferente do resultado da função pois a função corrige
  ## pelo viés internamente
  boot.ci(boot.obj, type = "norm")
  # BOOTSTRAP CONFIDENCE INTERVAL CALCULATIONS
  # Based on 2000 bootstrap replicates
  # 
  # CALL : 
  # boot.ci(boot.out = boot.obj, type = "norm")
  # 
  # Intervals : 
  # Level      Normal        
  # 95%   ( 0.5179,  1.0395 )  
  # Calculations and Intervals on Original Scale
  
  ## Básico
  2 * theta.hat - quantile(boot.obj$t, probs = rev(alpha), type = 6)
  #     97.5%      2.5% 
  # 0.5896451 1.1062186
  boot.ci(boot.obj, type = "basic")
  # BOOTSTRAP CONFIDENCE INTERVAL CALCULATIONS
  # Based on 2000 bootstrap replicates
  # 
  # CALL : 
  # boot.ci(boot.out = boot.obj, type = "basic")
  # 
  # Intervals : 
  # Level      Basic         
  # 95%   ( 0.5896,  1.1062 )  
  # Calculations and Intervals on Original Scale
  
  ## Percentil
  quantile(boot.obj$t, probs = alpha, type = 6)
  quantile(boot.obj$t, probs = alpha)
  #      2.5%     97.5% 
  # 0.4465304 0.9631039
  boot.ci(boot.obj, type = "perc")
  # BOOTSTRAP CONFIDENCE INTERVAL CALCULATIONS
  # Based on 2000 bootstrap replicates
  # 
  # CALL : 
  # boot.ci(boot.out = boot.obj, type = "perc")
  # 
  # Intervals : 
  # Level     Percentile     
  # 95%   ( 0.4465,  0.9631 )  
  # Calculations and Intervals on Original Scale
  
  
  ## Define função geral para calcular o intervalo t de bootstrap
  boot.t.ci <- function(x, B = 500, R = 100, level = .95, statistic){
    ## B = número de estimativas bootstrap (geral)
    ## R = número de estimativas bootstrap para o erro padrão
    x <- as.matrix(x);  n <- nrow(x)
    stat <- numeric(B); se <- numeric(B)
    ## Função local para calcular o erro padrão de cada amostra
    ## bootstrap x^{(b)} => bootstrap dentro de bootstrap
    boot.se <- function(x, R, f) {
      x <- as.matrix(x); m <- nrow(x)
      th <- replicate(R, expr = {
        i <- sample(1:m, size = m, replace = TRUE)
        ## f() é uma função = estatística calculada de interesse
        f(x[i, ])
      })
      return(sd(th))
    }
    ## Bootstrap geral
    for (b in 1:B) {
      j <- sample(1:n, size = n, replace = TRUE)
      y <- x[j, ]
      ## Calcula a estatística de interesse
      stat[b] <- statistic(y)
      ## Calcula o erro padrão baseado na amostra x^{(b)}. Aqui é
      ## feito um bootstrap dentro do outro
      se[b] <- boot.se(y, R = R, f = statistic)
    }
    ## Estatística amostral
    stat0 <- statistic(x)
    ## Estatística "estudentizada"
    t.stats <- (stat - stat0)/se
    ## Erro padrão das estimativas de bootstrap
    se0 <- sd(stat)
    ## Define alpha com base no nível de confiança
    alpha <- 1 - level
    ## Determina os quantis da distribuição da estatística
    ## "estudentizada"
    Qt <- quantile(t.stats, c(alpha/2, 1 - alpha/2), type = 1)
    ## Calcule limites do intervalo (inverte os nomes)
    CI <- rev(stat0 - Qt * se0)
    names(CI) <- rev(names(CI))
    return(list(CI = CI, stat = stat,
                t.stats = t.stats, Qt = Qt))
  }
  
  ## Aplica a função
  ci <- boot.t.ci(law, statistic = r, B = 2000, R = 200)
  
  ## Resultados
  ci$CI
  #       2.5%      97.5% 
  # -0.2041483  0.9812997
  ci$Qt
  #      2.5%     97.5% 
  # -1.569855  7.511420
  length(ci$stat)
  # [1] 2000
  length(ci$t.stats)
  # [1] 2000
  ## Distribuições
  par(mfrow = c(1, 2))
  ## Distribuição amostral
  hist(ci$stat)
  ## Distribuição "estudentizada" de referência
  hist(ci$t.stats); abline(v = ci$Qt, col = 2)
  
  par(mfrow = c(1, 1))
  
  
  data(patch, package = "bootstrap")
  patch
  #   subject placebo oldpatch newpatch     z     y
  # 1       1    9243    17649    16449  8406 -1200
  # 2       2    9671    12013    14614  2342  2601
  # 3       3   11792    19979    17274  8187 -2705
  # 4       4   13357    21816    23798  8459  1982
  # 5       5    9055    13850    12560  4795 -1290
  # 6       6    6290     9806    10157  3516   351
  # 7       7   12412    17208    16570  4796  -638
  # 8       8   18806    29044    26325 10238 -2719
  
  
  ## Estimativas básicas
  (theta.hat <- mean(patch$y)/mean(patch$z))
  # [1] -0.0713061
  
  ## Bootstrap para erro padrão
  n <- nrow(patch)
  B <- 2000
  theta.b <- numeric(B)
  for (b in 1:B) {
    i <- sample(1:n, size = n, replace = TRUE)
    y <- patch$y[i]
    z <- patch$z[i]
    theta.b[b] <- mean(y)/mean(z)
  }
  
  ## Estimativas
  mean(theta.b)
  # [1] -0.06407866
  (bias <- mean(theta.b) - theta.hat)
  # [1] 0.007227438
  (se <- sd(theta.b))
  # [1] 0.1034864
  
  ## Intervalos de confiança para a estimativa
  ## Usando o pacote boot
  theta.boot <- function(dat, ind) {
    y <- dat[ind, 1]
    z <- dat[ind, 2]
    mean(y)/mean(z)
  }
  
  dat <- cbind(patch$y, patch$z)
  boot.obj <- boot(dat, statistic = theta.boot, R = 2000)
  boot.obj
  # 
  # ORDINARY NONPARAMETRIC BOOTSTRAP
  # 
  # 
  # Call:
  # boot(data = dat, statistic = theta.boot, R = 2000)
  # 
  # 
  # Bootstrap Statistics :
  #       original     bias    std. error
  # t1* -0.0713061 0.00984133   0.1030833
  boot.ci(boot.obj, type = c("basic", "norm", "perc"))
  # BOOTSTRAP CONFIDENCE INTERVAL CALCULATIONS
  # Based on 2000 bootstrap replicates
  # 
  # CALL : 
  # boot.ci(boot.out = boot.obj, type = c("basic", "norm", "perc"))
  # 
  # Intervals : 
  # Level      Normal              Basic              Percentile     
  # 95%   (-0.2832,  0.1209 )   (-0.3126,  0.0941 )   (-0.2367,  0.1700 )  
  # Calculations and Intervals on Original Scale
  
  ## Intervalo t de bootstrap
  ci <- boot.t.ci(dat, statistic = theta.boot, B = 2000, R = 200)
  ## Resultados
  ci$CI
  #       2.5%      97.5% 
  # -0.2633727  0.4707715
  ci$Qt
  #      2.5%     97.5% 
  # -5.185837  1.837424
  ## Distribuições
  par(mfrow = c(1, 2))
  ## Distribuição amostral
  hist(ci$stat)
  ## Distribuição "estudentizada" de referência
  hist(ci$t.stats); abline(v = ci$Qt, col = 2)
  
  par(mfrow = c(1, 1))
  
  
  
  
  
########################################################################################
# Aula 08/11/2019 - Métodos de reamostragem - Jackknife
########################################################################################

##  2.1 Exemplo: média amostral
  
  ## Simula valores de uma normal
  set.seed(123)
  x <- rnorm(30, 100, 5)
  (n <- length(x))
  # [1] 30
  (xbar <- mean(x))
  # [1] 99.76448
  (s2 <- var(x))
  # [1] 24.06053
  (ep <- sqrt(s2/n))
  # [1] 0.8955544
  
  ## Estimativas parciais
  theta.jack <- numeric(n)
  for(i in 1:n) {
    theta.jack[i] <- mean(x[-i])
  }
  theta.jack
  #  [1]  99.85299  99.79605  99.48762  99.74420  99.73407  99.46066  99.67689
  #  [8]  99.97447  99.87478  99.83320  99.54531  99.69432  99.68726  99.73728
  # [15]  99.85219  99.44827  99.67052 100.09543  99.63544  99.83788  99.94047
  # [22]  99.79394  99.93326  99.88203  99.86413 100.04717  99.61191  99.72992
  # [29]  99.95259  99.54018
  
  ## Conceito: reproduzir o valor
  x[1]
  # [1] 97.19762
  n * xbar - (n - 1) * theta.jack[1]
  # [1] 97.19762
  
  ## Pseudo valores
  (pv <- n * xbar - (n - 1) * theta.jack)
  #  [1]  97.19762  98.84911 107.79354 100.35254 100.64644 108.57532 102.30458
  #  [8]  93.67469  96.56574  97.77169 106.12041 101.79907 102.00386 100.55341
  # [15]  97.22079 108.93457 102.48925  90.16691 103.50678  97.63604  94.66088
  # [22]  98.91013  94.86998  96.35554  96.87480  91.56653 104.18894 100.76687
  # [29]  94.30932 106.26907
  mean(pv)
  # [1] 99.76448
  xbar # média da amostra
  # [1] 99.76448
  
  ## Erro padrão
  ## Usando os pseudo-valores
  sqrt(var(pv)/n)
  # [1] 0.8955544
  ep # erro padrão da amostra
  # [1] 0.8955544
  
  ## Resultados
  round(cbind(Amostra = x, "Pseudo-valores" = pv,
              "Estimativas parciais" = theta.jack), 2)
  #       Amostra Pseudo-valores Estimativas parciais
  #  [1,]   97.20          97.20                99.85
  #  [2,]   98.85          98.85                99.80
  #  [3,]  107.79         107.79                99.49
  #  [4,]  100.35         100.35                99.74
  #  [5,]  100.65         100.65                99.73
  #  [6,]  108.58         108.58                99.46
  #  [7,]  102.30         102.30                99.68
  #  [8,]   93.67          93.67                99.97
  #  [9,]   96.57          96.57                99.87
  # [10,]   97.77          97.77                99.83
  # [11,]  106.12         106.12                99.55
  # [12,]  101.80         101.80                99.69
  # [13,]  102.00         102.00                99.69
  # [14,]  100.55         100.55                99.74
  # [15,]   97.22          97.22                99.85
  # [16,]  108.93         108.93                99.45
  # [17,]  102.49         102.49                99.67
  # [18,]   90.17          90.17               100.10
  # [19,]  103.51         103.51                99.64
  # [20,]   97.64          97.64                99.84
  # [21,]   94.66          94.66                99.94
  # [22,]   98.91          98.91                99.79
  # [23,]   94.87          94.87                99.93
  # [24,]   96.36          96.36                99.88
  # [25,]   96.87          96.87                99.86
  # [26,]   91.57          91.57               100.05
  # [27,]  104.19         104.19                99.61
  # [28,]  100.77         100.77                99.73
  # [29,]   94.31          94.31                99.95
  # [30,]  106.27         106.27                99.54
  
##  2.2 Exemplo: desvio padrão amostral
  
  ## Exemplo 2.1 do Manly. Estimativa do erro padrão para o desvio padrão
  ## Valores de X ~ Exp(1). E[X] = 1 e Var[X] = 1
  x <- c(3.56, 0.69, 0.1, 1.84, 3.93, 1.25, 0.18, 1.13, 0.27, 0.5, 0.67,
         0.01, 0.61, 0.82, 1.7, 0.39, 0.11, 1.2, 1.21, 0.72)
  hist(x)
  
  
  (n <- length(x))
  # [1] 20
  (xbar <- mean(x))
  # [1] 1.0445
  (sx <- sd(x))
  # [1] 1.05968
  
  ## Obter uma estimativa do erro padrão para o desvio padrão
  
  ## Estimativas parciais
  theta.jack <- numeric(n)
  for(i in 1:n) {
    theta.jack[i] <- sd(x[-i])
  }
  theta.jack
  #  [1] 0.9029186 1.0853369 1.0644890 1.0715868 0.8357022 1.0875825 1.0684568
  #  [8] 1.0885209 1.0724860 1.0807253 1.0849440 1.0595853 1.0836350 1.0873628
  # [15] 1.0771155 1.0771511 1.0650050 1.0880677 1.0879814 1.0858855
  
  ## Pseudo valores
  (pv <- n * sx - (n - 1) * theta.jack)
  #  [1] 4.0381381 0.5721907 0.9683003 0.8334424 5.3152487 0.5295241 0.8929116
  #  [8] 0.5116940 0.8163565 0.6598100 0.5796552 1.0614703 0.6045255 0.5336979
  # [15] 0.7283971 0.7277214 0.9584962 0.5203059 0.5219449 0.5617665
  mean(pv)
  # [1] 1.09678
  sx # desvio padrão amostral
  # [1] 1.05968
  
  ## Erro padrão
  ## Usando os pseudo-valores
  sqrt(var(pv)/n)
  # [1] 0.2802791
  
  ## Resultados
  round(cbind(Amostra = x, "Pseudo-valores" = pv,
              "Estimativas parciais" = theta.jack), 2)
  #       Amostra Pseudo-valores Estimativas parciais
  #  [1,]    3.56           4.04                 0.90
  #  [2,]    0.69           0.57                 1.09
  #  [3,]    0.10           0.97                 1.06
  #  [4,]    1.84           0.83                 1.07
  #  [5,]    3.93           5.32                 0.84
  #  [6,]    1.25           0.53                 1.09
  #  [7,]    0.18           0.89                 1.07
  #  [8,]    1.13           0.51                 1.09
  #  [9,]    0.27           0.82                 1.07
  # [10,]    0.50           0.66                 1.08
  # [11,]    0.67           0.58                 1.08
  # [12,]    0.01           1.06                 1.06
  # [13,]    0.61           0.60                 1.08
  # [14,]    0.82           0.53                 1.09
  # [15,]    1.70           0.73                 1.08
  # [16,]    0.39           0.73                 1.08
  # [17,]    0.11           0.96                 1.07
  # [18,]    1.20           0.52                 1.09
  # [19,]    1.21           0.52                 1.09
  # [20,]    0.72           0.56                 1.09
  
##  2.3 Exemplo: correlação
  
  ## Estimativa de erro padrão via bootstrap
  data(law, package = "bootstrap")
  str(law)
  # 'data.frame': 15 obs. of  2 variables:
  #  $ LSAT: num  576 635 558 578 666 580 555 661 651 605 ...
  #  $ GPA : num  3.39 3.3 2.81 3.03 3.44 3.07 3 3.43 3.36 3.13 ...
  plot(law$LSAT, law$GPA) 
  
  
  (rho <- cor(law$LSAT, law$GPA))
  # [1] 0.7763745
  ## Definições
  B <- 2000
  n <- nrow(law)
  R <- numeric(B)
  ## Bootstrap para a estimativa do erro padrão do R (correlação amostral)
  for (b in 1:B) {
    i <- sample(1:n, size = n, replace = TRUE)
    LSAT <- law$LSAT[i]
    GPA <- law$GPA[i]
    R[b] <- cor(LSAT, GPA)
  }
  ## Resultado
  mean(R)
  # [1] 0.7754856
  (se.R <- sd(R))
  # [1] 0.1305849
  
  ## Usando jackknife
  theta.jack <- numeric(n)
  for(j in 1:n) {
    theta.jack[j] <- cor(law$LSAT[-j], law$GPA[-j])
  }
  ## Estimativas parciais
  theta.jack
  #  [1] 0.8929471 0.7637068 0.7549984 0.7760968 0.7313197 0.7799687 0.7845360
  #  [8] 0.7361618 0.7517391 0.7761231 0.8181007 0.7857184 0.7403509 0.7670413
  # [15] 0.7798725
  
  ## Pseudo valores
  ## Note que alguns valores estão fora do intervalo [-1,1]
  (pv <- n * rho - (n - 1) * theta.jack)
  #  [1] -0.8556427  0.9537216  1.0756402  0.7802626  1.4071420  0.7260560
  #  [7]  0.6621137  1.3393518  1.1212703  0.7798940  0.1922075  0.6455592
  # [13]  1.2807048  0.9070386  0.7274020
  mean(pv)
  # [1] 0.7828481
  rho # valor da amostra
  # [1] 0.7763745
  
  ## Erro padrão
  sqrt(var(pv)/n)
  # [1] 0.1425186
  se.R # via bootstrap
  # [1] 0.1305849
  ## sqrt(((n - 1)/n) * sum((theta.jack - mean(theta.jack))^2))
  
  ## Intervalo de confiança Jackknife (supõe independência e normalidade).
  mean(pv) + qt(c(.025, .975), df = n - 1) * sqrt(var(pv)/n)
  # [1] 0.4771761 1.0885202
  cor.test(law$LSAT, law$GPA)$conf.int # teórico
  # [1] 0.4385108 0.9219648
  # attr(,"conf.level")
  # [1] 0.95
  
##  2.4 Exemplo: razão de médias
  
  data(patch, package = "bootstrap")
  n <- nrow(patch)
  y <- patch$y
  z <- patch$z
  (theta.hat <- mean(y)/mean(z))
  # [1] -0.0713061
  
  ## Estimativas parciais: jackknife
  theta.jack <- numeric(n)
  for (i in 1:n) {
    theta.jack[i] <- mean(y[-i])/mean(z[-i])
  }
  
  ## Pseudo valores
  (pv <- n * theta.hat - (n - 1) * theta.jack)
  # [1] -0.17061885  0.32904914 -0.42025606  0.35670355 -0.21575610  0.01788742
  # [7] -0.11640789 -0.41506989
  mean(pv)
  # [1] -0.07930858
  theta.hat # amostral
  # [1] -0.0713061
  
  ## Erro padrão de jackknife
  sqrt(var(pv)/n)
  # [1] 0.1055278
  
  ## Via bootstrap
  B <- 2000
  theta.b <- numeric(B)
  for (b in 1:B) {
    i <- sample(1:n, size = n, replace = TRUE)
    y <- patch$y[i]
    z <- patch$z[i]
    theta.b[b] <- mean(y)/mean(z)
  }
  ## Estimativas
  (se <- sd(theta.b))
  # [1] 0.09987555
  
  ## Intervalos de confiança para a estimativa
  ## Via bootstrap
  theta.boot <- function(dat, ind) {
    y <- dat[ind, 1]
    z <- dat[ind, 2]
    mean(y)/mean(z)
  }
  dat <- cbind(patch$y, patch$z)
  boot.obj <- boot(dat, statistic = theta.boot, R = 2000)
  boot.ci(boot.obj, type = c("basic", "norm", "perc"))
  # BOOTSTRAP CONFIDENCE INTERVAL CALCULATIONS
  # Based on 2000 bootstrap replicates
  # 
  # CALL : 
  # boot.ci(boot.out = boot.obj, type = c("basic", "norm", "perc"))
  # 
  # Intervals : 
  # Level      Normal              Basic              Percentile     
  # 95%   (-0.2805,  0.1223 )   (-0.3094,  0.0890 )   (-0.2316,  0.1668 )  
  # Calculations and Intervals on Original Scale
  ## Via jackknife
  mean(pv) + qt(c(.025, .975), df = n - 1) * sqrt(var(pv)/n)
  # [1] -0.3288421  0.1702250
  
  ## 3 Validação cruzada
  
  ## Dados
  data("ironslag", package = "DAAG")
  str(ironslag)
  # 'data.frame': 53 obs. of  2 variables:
  #  $ chemical: num  24 16 24 18 18 10 14 16 18 20 ...
  #  $ magnetic: num  25 22 17 21 20 13 16 14 19 10 ...
  plot(magnetic ~ chemical, ironslag)
  
  
  ## Modelos propostos
  a <- seq(10, 40, .1) # sequencia para graficos
  ## Linear
  par(mfrow = c(2, 2))
  L1 <- lm(magnetic ~ chemical, ironslag)
  plot(magnetic ~ chemical, ironslag, main = "Linear", pch = 19)
  yhat1 <- L1$coef[1] + L1$coef[2] * a
  lines(a, yhat1, lwd = 2, col = 2)
  ## Quadratico
  L2 <- lm(magnetic ~ chemical + I(chemical^2), ironslag)
  plot(magnetic ~ chemical, ironslag, main = "Quadratic", pch = 19)
  yhat2 <- L2$coef[1] + L2$coef[2] * a + L2$coef[3] * a^2
  lines(a, yhat2, lwd = 2, col = 2)
  ## Exponencial
  L3 <- lm(log(magnetic) ~ chemical, ironslag)
  plot(magnetic ~ chemical, ironslag, main = "Exponential", pch = 19)
  logyhat3 <- L3$coef[1] + L3$coef[2] * a
  yhat3 <- exp(logyhat3)
  lines(a, yhat3, lwd = 2, col = 2)
  ## log-log
  L4 <- lm(log(magnetic) ~ log(chemical), ironslag)
  plot(log(magnetic) ~ log(chemical), ironslag,
       main = "Log-Log", pch = 19)
  logyhat4 <- L4$coef[1] + L4$coef[2] * log(a)
  lines(log(a), logyhat4, lwd = 2, col = 2)
  
  
    ## Validação cruzada
  n <- length(ironslag$magnetic)
  e1 <- e2 <- e3 <- e4 <- numeric(n)
  for (k in 1:n) {
    y <- ironslag$magnetic[-k]
    x <- ironslag$chemical[-k]
    ## Linear
    J1 <- lm(y ~ x)
    yhat1 <- J1$coef[1] + J1$coef[2] * ironslag$chemical[k]
    e1[k] <- ironslag$magnetic[k] - yhat1
    ## Quadrático
    J2 <- lm(y ~ x + I(x^2))
    yhat2 <- J2$coef[1] + J2$coef[2] * ironslag$chemical[k] +
      J2$coef[3] * ironslag$chemical[k]^2
    e2[k] <- ironslag$magnetic[k] - yhat2
    ## Exponencial
    J3 <- lm(log(y) ~ x)
    logyhat3 <- J3$coef[1] + J3$coef[2] * ironslag$chemical[k]
    yhat3 <- exp(logyhat3)
    e3[k] <- ironslag$magnetic[k] - yhat3
    ## Log-log
    J4 <- lm(log(y) ~ log(x))
    logyhat4 <- J4$coef[1] + J4$coef[2] * log(ironslag$chemical[k])
    yhat4 <- exp(logyhat4)
    e4[k] <- ironslag$magnetic[k] - yhat4
  }
  
  ## Estimativas do erro quadrático médio
  c(mean(e1^2), mean(e2^2), mean(e3^2), mean(e4^2))
  # [1] 19.55644 17.85248 18.44188 20.45424
  
  ## Comparação dos modelos
  par(mfrow = c(4, 2))
  plot(L1$fit, L1$res, main = "Linear"); abline(0, 0)
  qqnorm(L1$res); qqline(L1$res)
  plot(L2$fit, L2$res, main = "Quadrático"); abline(0, 0)
  qqnorm(L2$res); qqline(L2$res)
  plot(L3$fit, L3$res, main = "Exponencial"); abline(0, 0)
  qqnorm(L3$res); qqline(L3$res)
  plot(L4$fit, L4$res, main = "log-log"); abline(0, 0)
  qqnorm(L4$res); qqline(L4$res)
  
  par(mfrow = c(1, 1))
  
  
########################################################################################
# Aula 12/11/2019 - Sabatina 05 - Moodle
########################################################################################
# Aula 15/11/2019 - Feriado
########################################################################################

########################################################################################
# Aula 19/11/2019 - Testes de permutação (ou aleatorização)
########################################################################################
#  2.1.1 Diferença entre médias de dois grupos
  
  ## Dados observados
  x <- c(4.1, 8.3, 2.9, 10.8, 9.5)
  y <- c(3.7, 5.1, 1.0, 7.7, 8.9)
  da <- data.frame(vals = c(x, y),
                   id = rep(c("x", "y"), each = 5))
  da
  #    vals id
  # 1   4.1  x
  # 2   8.3  x
  # 3   2.9  x
  # 4  10.8  x
  # 5   9.5  x
  # 6   3.7  y
  # 7   5.1  y
  # 8   1.0  y
  # 9   7.7  y
  # 10  8.9  y
  
  ## Compara médias
  with(da, tapply(vals, id, mean))
  #    x    y 
  # 7.12 5.28
  (obsdiff <- with(da, abs(diff(tapply(vals, id, mean)))))
  #    y 
  # 1.84
  
  ## Teste-t tradicional
  t.test(vals ~ id, data = da, var.equal = TRUE)
  # 
  #   Two Sample t-test
  # 
  # data:  vals by id
  # t = 0.88051, df = 8, p-value = 0.4043
  # alternative hypothesis: true difference in means is not equal to 0
  # 95 percent confidence interval:
  #  -2.978831  6.658831
  # sample estimates:
  # mean in group x mean in group y 
  #            7.12            5.28
  
  ## Número possível de permutações por grupo
  factorial(length(x))
  # [1] 120
  factorial(length(y))
  # [1] 120
  
  ## A permutação dentro de cada grupo não faz sentido, pois as médias não
  ## serão alteradas
  xperm <- gtools::permutations(n = length(x), r = length(x), v = x)
  str(xperm)
  #  num [1:120, 1:5] 2.9 2.9 2.9 2.9 2.9 2.9 2.9 2.9 2.9 2.9 ...
  sort(x)
  # [1]  2.9  4.1  8.3  9.5 10.8
  head(xperm)
  #      [,1] [,2] [,3] [,4] [,5]
  # [1,]  2.9  4.1  8.3  9.5 10.8
  # [2,]  2.9  4.1  8.3 10.8  9.5
  # [3,]  2.9  4.1  9.5  8.3 10.8
  # [4,]  2.9  4.1  9.5 10.8  8.3
  # [5,]  2.9  4.1 10.8  8.3  9.5
  # [6,]  2.9  4.1 10.8  9.5  8.3
  tail(xperm)
  #        [,1] [,2] [,3] [,4] [,5]
  # [115,] 10.8  9.5  2.9  4.1  8.3
  # [116,] 10.8  9.5  2.9  8.3  4.1
  # [117,] 10.8  9.5  4.1  2.9  8.3
  # [118,] 10.8  9.5  4.1  8.3  2.9
  # [119,] 10.8  9.5  8.3  2.9  4.1
  # [120,] 10.8  9.5  8.3  4.1  2.9
  yperm <- gtools::permutations(n = length(y), r = length(y), v = y)
  str(yperm)
  #  num [1:120, 1:5] 1 1 1 1 1 1 1 1 1 1 ...
  sort(y)
  # [1] 1.0 3.7 5.1 7.7 8.9
  head(yperm)
  #      [,1] [,2] [,3] [,4] [,5]
  # [1,]    1  3.7  5.1  7.7  8.9
  # [2,]    1  3.7  5.1  8.9  7.7
  # [3,]    1  3.7  7.7  5.1  8.9
  # [4,]    1  3.7  7.7  8.9  5.1
  # [5,]    1  3.7  8.9  5.1  7.7
  # [6,]    1  3.7  8.9  7.7  5.1
  tail(yperm)
  #        [,1] [,2] [,3] [,4] [,5]
  # [115,]  8.9  7.7  1.0  3.7  5.1
  # [116,]  8.9  7.7  1.0  5.1  3.7
  # [117,]  8.9  7.7  3.7  1.0  5.1
  # [118,]  8.9  7.7  3.7  5.1  1.0
  # [119,]  8.9  7.7  5.1  1.0  3.7
  # [120,]  8.9  7.7  5.1  3.7  1.0
  ## Diferença entre médias para todas as permutações
  xydiff <- numeric(nrow(xperm))
  for(i in 1:nrow(xperm)) {
    xydiff[i] <- mean(xperm[i, ]) - mean(yperm[i, ])
  }
  str(xydiff)
  #  num [1:120] 1.84 1.84 1.84 1.84 1.84 1.84 1.84 1.84 1.84 1.84 ...
  summary(xydiff)
  #    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  #    1.84    1.84    1.84    1.84    1.84    1.84
  
  ## Portanto, a permutação deve ser feita entre os grupos, ou seja,
  ## alternando todos os valores possíveis entre os dois grupos
  xy <- c(x, y)
  ## Número de permutações
  factorial(length(xy))
  # [1] 3628800
  xyperm <- gtools::permutations(n = length(xy), r = length(xy), v = xy)
  str(xyperm)
  #  num [1:3628800, 1:10] 1 1 1 1 1 1 1 1 1 1 ...
  sort(xy)
  #  [1]  1.0  2.9  3.7  4.1  5.1  7.7  8.3  8.9  9.5 10.8
  head(xyperm)
  #      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
  # [1,]    1  2.9  3.7  4.1  5.1  7.7  8.3  8.9  9.5  10.8
  # [2,]    1  2.9  3.7  4.1  5.1  7.7  8.3  8.9 10.8   9.5
  # [3,]    1  2.9  3.7  4.1  5.1  7.7  8.3  9.5  8.9  10.8
  # [4,]    1  2.9  3.7  4.1  5.1  7.7  8.3  9.5 10.8   8.9
  # [5,]    1  2.9  3.7  4.1  5.1  7.7  8.3 10.8  8.9   9.5
  # [6,]    1  2.9  3.7  4.1  5.1  7.7  8.3 10.8  9.5   8.9
  tail(xyperm)
  #            [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
  # [3628795,] 10.8  9.5  8.9  8.3  7.7  5.1  4.1  1.0  2.9   3.7
  # [3628796,] 10.8  9.5  8.9  8.3  7.7  5.1  4.1  1.0  3.7   2.9
  # [3628797,] 10.8  9.5  8.9  8.3  7.7  5.1  4.1  2.9  1.0   3.7
  # [3628798,] 10.8  9.5  8.9  8.3  7.7  5.1  4.1  2.9  3.7   1.0
  # [3628799,] 10.8  9.5  8.9  8.3  7.7  5.1  4.1  3.7  1.0   2.9
  # [3628800,] 10.8  9.5  8.9  8.3  7.7  5.1  4.1  3.7  2.9   1.0
  
  ## Calcula a diferença média para todas as permutações possíveis
  xydiff <- numeric(nrow(xyperm))
  for(i in 1:nrow(xyperm)) {
    xydiff[i] <- mean(xyperm[i, 1:5]) - mean(xyperm[i, 6:10])
  }
  str(xydiff)
  #  num [1:3628800] -5.68 -5.68 -5.68 -5.68 -5.68 -5.68 -5.68 -5.68 -5.68 -5.68 ...
  summary(xydiff)
  #    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  #   -5.68   -1.45    0.00    0.00    1.45    5.68
  hist(xydiff)
  abline(v = obsdiff, col = 2)
  
  ## P-valor do teste.
  2 * sum(xydiff >= obsdiff)/length(xydiff)
  # [1] 0.3888889
  t.test(vals ~ id, data = da, var.equal = TRUE)$p.value
  # [1] 0.404256
  
  ## Usando pacotes
  library(coin)
  # Loading required package: survival
  # 
  # Attaching package: 'survival'
  # The following object is masked from 'package:boot':
  # 
  #     aml
  # 
  # Attaching package: 'coin'
  # The following object is masked _by_ '.GlobalEnv':
  # 
  #     alpha
  oneway_test(vals ~ id, data = da)
  # 
  #   Asymptotic Two-Sample Fisher-Pitman Permutation Test
  # 
  # data:  vals by id (x, y)
  # Z = 0.89172, p-value = 0.3725
  # alternative hypothesis: true mu is not equal to 0
  oneway_test(vals ~ id, data = da,
              distribution = approximate(nresample = 10000))
  # 
  #   Approximative Two-Sample Fisher-Pitman Permutation Test
  # 
  # data:  vals by id (x, y)
  # Z = 0.89172, p-value = 0.3842
  # alternative hypothesis: true mu is not equal to 0
  library(perm)
  permTS(vals ~ id, data = da)
  # 
  #   Exact Permutation Test (network algorithm)
  # 
  # data:  vals by id
  # p-value = 0.3889
  # alternative hypothesis: true mean id=x - mean id=y is not equal to 0
  # sample estimates:
  # mean id=x - mean id=y 
  #                  1.84
  
  ## Mesmo em um caso simples como, esse, onde n = 10, já vimos que o
  ## número total de permutações possíveis pode ser muito grande, o que
  ## faz com que esse processo fique inviável computacionalmente.
  ## A ideia então é fazer um grande número de permutações aleatórias e
  ## fazer o mesmo cálculo. Isso pode ser feito retirando-se amostra COM
  ## REPOSIÇÃO da amostra conjunta (concatenando os dois grupos)
  ## Usando amostras sem reposição
  N <- 10000
  xydiff <- numeric(N)
  for(i in 1:N) {
    xydiff[i] <- diff(tapply(sample(xy), da$id, mean))
  }
  str(xydiff)
  #  num [1:10000] 0.36 1.36 -0.16 -2.96 -1.72 ...
  summary(xydiff)
  #    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  # -5.6800 -1.4400  0.0000  0.0113  1.4800  5.6800
  hist(xydiff)
  abline(v = obsdiff, col = 2)
  
  
  ## P-valor do teste.
  2 * sum(xydiff >= obsdiff)/length(xydiff)
  # [1] 0.396
  t.test(vals ~ id, data = da, var.equal = TRUE)$p.value
  # [1] 0.404256
  coin::oneway_test(vals ~ id, data = da)
  # 
  #   Asymptotic Two-Sample Fisher-Pitman Permutation Test
  # 
  # data:  vals by id (x, y)
  # Z = 0.89172, p-value = 0.3725
  # alternative hypothesis: true mu is not equal to 0
  perm::permTS(vals ~ id, data = da)
  # 
  #   Exact Permutation Test (network algorithm)
  # 
  # data:  vals by id
  # p-value = 0.3889
  # alternative hypothesis: true mean id=x - mean id=y is not equal to 0
  # sample estimates:
  # mean id=x - mean id=y 
  #                  1.84
  
## 2.1.2 Teste para correlação
  
  ## Usando o mesmo exemplo, mas agora calculando a correlação entre os
  ## grupos
  ## Correlação observada. NOTE que é necessário usar a correlação (de
  ## postos) de Spearman
  cor(x, y, method = "pearson")
  # [1] 0.9228669
  cor(x, y, method = "kendall")
  # [1] 0.8
  (obscor <- cor(x, y, method = "spearman"))
  # [1] 0.9
  
  ## Calcula a diferença média para todas as permutações possíveis
  xycor <- numeric(nrow(xyperm))
  for(i in 1:nrow(xyperm)) {
    xycor[i] <- cor(xyperm[i, 1:5], xyperm[i, 6:10],
                    method = "spearman")
  }
  str(xycor)
  #  num [1:3628800] 1 0.9 0.9 0.7 0.7 0.6 0.9 0.8 0.7 0.4 ...
  summary(xycor)
  #    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  #    -1.0    -0.4     0.0     0.0     0.4     1.0
  hist(xycor)
  abline(v = obscor, col = 2)
  
  ## P-valor do teste.
  2 * sum(xycor >= obscor)/length(xycor) # teste exato
  # [1] 0.08333333
  cor.test(x, y,  method = "pearson")$p.value
  # [1] 0.02541591
  cor.test(x, y,  method = "kendall")$p.value
  # [1] 0.08333333
  cor.test(x, y,  method = "spearman")$p.value
  # [1] 0.08333333
  spearman_test(x ~ y,
                distribution = approximate(nresample = 10000))
  # 
  #   Approximative Spearman Correlation Test
  # 
  # data:  x by y
  # Z = 1.8, p-value = 0.0833
  # alternative hypothesis: true rho is not equal to 0
  
  ## Usa amostragem SEM REPOSIÇÃO
  N <- 100000
  n <- length(xy)
  xycor <- numeric(N)
  for(i in 1:N) {
    ip <- sample(1:n, replace = FALSE)
    xp <- xy[ip[1:5]]
    yp <- xy[ip[6:10]]
    xycor[i] <- cor(xp, yp, method = "spearman")
  }
  str(xycor)
  #  num [1:100000] -0.3 0.7 0 -0.8 0.8 -0.1 0.7 -0.5 0.2 -0.1 ...
  summary(xycor)
  #      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
  # -1.000000 -0.400000  0.000000 -0.000821  0.400000  1.000000
  hist(xycor)
  abline(v = obscor, col = 2)
  
  ## P-valor do teste.
  2 * sum(xycor >= obscor)/length(xycor) # teste aproximado
  # [1] 0.08306
  cor.test(x, y,  method = "pearson")$p.value
  # [1] 0.02541591
  cor.test(x, y,  method = "kendall")$p.value
  # [1] 0.08333333
  cor.test(x, y,  method = "spearman")$p.value
  # [1] 0.08333333
  spearman_test(x ~ y,
                distribution = approximate(nresample = 10000))
  # 
  #   Approximative Spearman Correlation Test
  # 
  # data:  x by y
  # Z = 1.8, p-value = 0.0811
  # alternative hypothesis: true rho is not equal to 0
  
## 2.2 Exemplo aplicado: correlação
  
  data(law, package = "bootstrap")
  str(law)
  # 'data.frame': 15 obs. of  2 variables:
  #  $ LSAT: num  576 635 558 578 666 580 555 661 651 605 ...
  #  $ GPA : num  3.39 3.3 2.81 3.03 3.44 3.07 3 3.43 3.36 3.13 ...
  plot(law$LSAT, law$GPA)
  
  
  x <- law$LSAT
  y <- law$GPA
  (obscor <- cor(x, y, method = "spearman"))
  # [1] 0.7964286
  
  ## Impossível fazer com todas as permutações
  factorial(nrow(law))
  # [1] 1.307674e+12
  
  ## Usa amostragem SEM REPOSIÇÃO
  N <- 1000000
  xy <- c(x, y)
  n <- length(xy)
  xycor <- numeric(N)
  for(i in 1:N) {
    ip <- sample(1:n, size = n/2, replace = FALSE)
    xp <- xy[ip]
    yp <- xy[-ip]
    xycor[i] <- cor(xp, yp, method = "spearman")
  }
  str(xycor)
  #  num [1:1000000] -0.0464 0.0321 -0.175 0.2393 -0.3 ...
  summary(xycor)
  #       Min.    1st Qu.     Median       Mean    3rd Qu.       Max. 
  # -0.9107143 -0.1857143  0.0000000 -0.0003011  0.1857143  0.9464286
  hist(xycor)
  abline(v = obscor, col = 2)
  
  ## P-valor do teste.
  2 * sum(xycor >= obscor)/length(xycor) # teste aproximado
  # [1] 0.000618
  cor.test(x, y,  method = "pearson")$p.value
  # [1] 0.000665102
  cor.test(x, y,  method = "kendall")$p.value
  # [1] 0.0005320216
  cor.test(x, y,  method = "spearman")$p.value
  # [1] 0.000607857
  spearman_test(x ~ y,
                distribution = approximate(nresample = 100000))
  # 
  #   Approximative Spearman Correlation Test
  # 
  # data:  x by y
  # Z = 2.98, p-value = 0.00056
  # alternative hypothesis: true rho is not equal to 0
  
##  2.3 Exemplo das aulas anteriores
  
  ## Exemplo adaptado de Manly (1997)
  ## Comparação do comprimento da mandíbula de chacais machos e fêmeas
  set.seed(2)
  machos <- c(120, 107, 110, 116, 114, 111, 113, 117, 114, 112)
  ## Simula diferença para as femeas
  femeas <- rnorm(10, mean(machos) - 2, sd = sd(machos))
  da <- data.frame(comp = c(machos, femeas),
                   sexo = c(rep("M", 10), rep("F", 10)))
  densityplot(~comp, groups = sexo, data = da, auto.key = TRUE)
  
  
  ## Média por sexo
  tapply(da$comp, da$sexo, mean)
  #       F       M 
  # 112.185 113.400
  ## Diferença das médias
  diff(tapply(da$comp, da$sexo, mean))
  #        M 
  # 1.214975
  
  ## Média de cada sexo
  (m1 <- mean(machos))
  # [1] 113.4
  (m2 <- mean(femeas))
  # [1] 112.185
  ## Diferença entre as médias amostrais
  (med.amostral <- m1 - m2)
  # [1] 1.214975
  ## Calcula o desvio padrão ponderado
  n1 <- length(machos)
  v1 <- var(machos)
  n2 <- length(femeas)
  v2 <- var(femeas)
  (s.pond <- sqrt(((n1 - 1) * v1 + (n2 - 1) * v2)/(n1 + n2 - 2)))
  # [1] 3.690024
  
  ## Teste de hipótese para
  ## H0: mu1 <= mu2
  ## Ha: mu1 > mu2
  mu0 <- 0
  t.test(x = machos, y = femeas, alternative = "greater",
         var.equal = TRUE, mu = mu0)
  # 
  #   Two Sample t-test
  # 
  # data:  machos and femeas
  # t = 0.73625, df = 18, p-value = 0.2355
  # alternative hypothesis: true difference in means is greater than 0
  # 95 percent confidence interval:
  #  -1.646627       Inf
  # sample estimates:
  # mean of x mean of y 
  #   113.400   112.185
  ## Estatística de teste
  (tcalc <- (m1 - m2)/(s.pond * sqrt(1/n1 + 1/n2)))
  # [1] 0.7362465
  ## Valor crítico
  (tcrit <- qt(.025, df = n1 + n2 - 2, lower.tail = FALSE))
  # [1] 2.100922
  ## p-valor
  pt(tcalc, df = n1 + n2 - 2, lower.tail = FALSE)
  # [1] 0.2355338
  
  ## Teste por simulação via _permutação_
  N <- 10000
  ## Se a hipótese nula é verdadeira, então o comprimento das mandíbulas
  ## de machos e fêmeas são provenientes da mesma poplação, e portanto
  ## podem ser pensados como uma única amostra.
  amostra <- c(machos, femeas)
  ## Amostra SEM REPOSIÇÃO os 20 valores, e atribui aleatoriamente 10 para
  ## cada grupo (macho ou fêmea). Se forem de fato da mesma população,
  ## então as diferenças entre as médias devem ser próximas de zero.
  am <- replicate(
    N, diff(tapply(sample(amostra, replace = FALSE), da$sexo, mean))
  )
  ## Visualização
  hist(am, main = "Distribuição amostral")
  abline(v = med.amostral, col = 2)
  
  ## p-valor empírico
  sum(am >= med.amostral)/N
  # [1] 0.2309
  
  ## Exemplo adaptado de Manly (1997)
  ## Comparação do comprimento da mandíbula de chacais machos e fêmeas
  machos <- c(120, 107, 110, 116, 114, 111, 113, 117, 114, 112)
  femeas <- c(110, 111, 107, 108, 110, 105, 107, 106, 111, 111)
  da <- data.frame(comp = c(machos, femeas),
                   sexo = c(rep("M", 10), rep("F", 10)))
  densityplot(~comp, groups = sexo, data = da, auto.key = TRUE)
  
  ## Média por sexo
  tapply(da$comp, da$sexo, mean)
  #     F     M 
  # 108.6 113.4
  ## Diferença das médias
  diff(tapply(da$comp, da$sexo, mean))
  #   M 
  # 4.8
  
  ## Média de cada sexo
  (m1 <- mean(machos))
  # [1] 113.4
  (m2 <- mean(femeas))
  # [1] 108.6
  ## Diferença entre as médias amostrais
  (med.amostral <- m1 - m2)
  # [1] 4.8
  ## Calcula o desvio padrão ponderado
  n1 <- length(machos)
  v1 <- var(machos)
  n2 <- length(femeas)
  v2 <- var(femeas)
  (s.pond <- sqrt(((n1 - 1) * v1 + (n2 - 1) * v2)/(n1 + n2 - 2)))
  # [1] 3.080404
  
  ## Teste de hipótese para
  ## H0: mu1 <= mu2
  ## Ha: mu1 > mu2
  mu0 <- 0
  t.test(x = machos, y = femeas, alternative = "greater",
         var.equal = TRUE, mu = mu0)
  # 
  #   Two Sample t-test
  # 
  # data:  machos and femeas
  # t = 3.4843, df = 18, p-value = 0.001324
  # alternative hypothesis: true difference in means is greater than 0
  # 95 percent confidence interval:
  #  2.411156      Inf
  # sample estimates:
  # mean of x mean of y 
  #     113.4     108.6
  ## Estatística de teste
  (tcalc <- (m1 - m2)/(s.pond * sqrt(1/n1 + 1/n2)))
  # [1] 3.484324
  ## Valor crítico
  (tcrit <- qt(.025, df = n1 + n2 - 2, lower.tail = FALSE))
  # [1] 2.100922
  ## p-valor
  pt(tcalc, df = n1 + n2 - 2, lower.tail = FALSE)
  # [1] 0.001323634
  
  ## Teste por simulação via _permutação_
  N <- 10000
  ## Se a hipótese nula é verdadeira, então o comprimento das mandíbulas
  ## de machos e fêmeas são provenientes da mesma população, e portanto
  ## podem ser pensados como uma única amostra.
  amostra <- c(machos, femeas)
  ## Amostra SEM REPOSIÇÃO os 20 valores, e atribui aleatoriamente 10 para
  ## cada grupo (macho ou fêmea). Se forem de fato da mesma população,
  ## então as diferenças entre as médias devem ser próximas de zero.
  am <- replicate(
    N, diff(tapply(sample(amostra, replace = FALSE), da$sexo, mean))
  )
  ## Visualização
  hist(am, main = "Distribuição amostral")
  abline(v = med.amostral, col = 2)
  
  ## p-valor empírico
  sum(am >= med.amostral)/N
  # [1] 0.0015
  
##  2.4 Índice de Moran (correlação espacial)
  
  ## Índice de Moran para medir dependência espacial.
  
  ## Coordenadas dos eventos em uma malha regular 8 x 8.
  x <- 1:8
  y <- 1:8
  
  ## Construção da matriz de pesos que determina a vizinhança entre
  ## observações.
  ind <- expand.grid(i = 1:length(x),
                     j = 1:length(y))
  
  ##  Função que determina o peso entre duas localizações na malha.
  f <- function(i, j) {
    u <- min(3, sum(abs(ind[i, ] - ind[j, ])))
    w <- c(0, 1, sqrt(1/2), 0)[u + 1]
    return(w)
  }
  
  ##  Cria os pesos, matriz (8^2) x (8^2) = 64 x 64.
  w <- matrix(0, nrow = nrow(ind), ncol = nrow(ind))
  for (i in 1:nrow(ind)) {
    for (j in 1:nrow(ind)) {
      w[i, j] <- f(i, j)
    }
  }
  
  ##  Normaliza.
  w <- w/sum(w)
  
  ## Gráfico. Valores claros indicam maior peso entre observações.
  image(w, asp = 1, col = gray.colors(100))
  
  ## Lógica do índica de Moran: correlação entre valores observados e
  ## média dos vizinhos. Exemplo com valores simulados.
  xx <- rnorm(64)
  cor(cbind("Valores observados" = xx,
            "Média dos vizinhos" = as.vector(xx %*% w)))
  #                    Valores observados Média dos vizinhos
  # Valores observados          1.0000000         -0.2233522
  # Média dos vizinhos         -0.2233522          1.0000000
  
  ## Índice de Moran
  moran <- function(x, w) {
    n <- length(x)
    xbar <- mean(x)
    dx <- x - xbar
    xi <- rep(dx, each = n)
    xj <- rep(dx)
    xixj <- xi * xj
    pm <- matrix(xixj, ncol = n)
    pmw <- pm * w
    spmw <- sum(pmw)
    smw <- sum(w)
    sw  <- spmw / smw
    vr <- n / sum(dx^2)
    MI <- vr * sw
    return(MI)
  }
  
  ## Moran para os dados simulados
  moran(xx, w)
  # [1] -0.05779878
  
  ## A ideia do teste de permutação, é trocar de lugar as observações e
  ## calcular o índice de Moran, mantendo a matriz de pesos fixa. Se não
  ## houver dependência espacial, então qualquer observação poderia estar
  ## em qualquer lugar. Com isso, o valor calculado do índice de Moran
  ## pode ser comparado com a distribuição dos índices de Moran calculados
  ## para observações permutadas.
  ## Se o valor observado for extremo, indica que deve haver correlação
  ## espacial. Se o observado estiver no centro (ou próximo do centro) da
  ## distribuição, então não há evidências de correlação espacial.
  replicate(10, moran(sample(xx), w))
  #  [1]  0.034023631 -0.032555384 -0.041883483 -0.019790671  0.024932478
  #  [6] -0.005887561 -0.046625628  0.006277762  0.017167652  0.019192943
  
  ## Teste de permutação com saída gráfica.
  ppt <- function(z, w, N = 10000, stat, ...) {
    ## Índice de Moran por reamostragem.
    sim <- replicate(N,
                     moran(sample(z), w))
    ## Determina o p-valor.
    p.value <- mean((all <- c(stat, sim)) >= stat)
    ## Histograma da distribuição empírica sob H_0.
    hist(sim,
         sub = paste("p =", round(p.value, 4)),
         xlim = range(all),
         ...)
    abline(v = stat, col = "#903030", lty = 3, lwd = 2)
    return(p.value)
  }
  
  ## Observações simuladas.
  set.seed(17)
  par(mfrow = c(2, 3))
  
  ## Dados com dependência espacial --------------------------------------
  ## Indução de autocorrelação por meio de uma tendência.
  z <- matrix(rexp(length(x) * length(y),
                   outer(x, y^2)),
              length(x))
  image(log(z), main = "Com dependência")
  
  cor(cbind("Valores observados" = as.vector(z),
            "Média dos vizinhos" = as.vector(as.vector(z) %*% w)))
  #                    Valores observados Média dos vizinhos
  # Valores observados          1.0000000          0.1335676
  # Média dos vizinhos          0.1335676          1.0000000
  
  ## Índice de Moran com dados originais.
  (stat <- moran(z, w))
  # [1] 0.06551254
  
  hist(z)
  ppt(z, w, stat = stat, main = "I de Moran", xlab = "I")
  # [1] 0.01559844
  
  ## Teste usando spdep
  spdep::moran.test(z, spdep::mat2listw(w))
  # 
  #   Moran I test under randomisation
  # 
  # data:  z  
  # weights: spdep::mat2listw(w)    
  # 
  # Moran I statistic standard deviate = 2.7152, p-value = 0.003312
  # alternative hypothesis: greater
  # sample estimates:
  # Moran I statistic       Expectation          Variance 
  #      0.0655125441     -0.0158730159      0.0008984441
  ## De help(moran.test):
  ## The assumptions underlying the test are sensitive to the form of the
  ## graph of neighbour relationships and other factors, and results may
  ## be checked against those of moran.mc permutations
  spdep::moran.mc(z, spdep::mat2listw(w), nsim = 10000)
  # 
  #   Monte-Carlo simulation of Moran I
  # 
  # data:  z 
  # weights: spdep::mat2listw(w)  
  # number of simulations + 1: 10001 
  # 
  # statistic = 0.065513, observed rank = 9837, p-value = 0.0164
  # alternative hypothesis: greater
  
  ## Dados sem dependência espacial --------------------------------------
  ## Geração de de um conjunto de dados sob hipótese nula.
  z <- matrix(rnorm(length(x) * length(y), 0, 1/2), length(x))
  image(z, main = "Sem dependência")
  
  cor(cbind("Valores observados" = as.vector(z),
            "Média dos vizinhos" = as.vector(as.vector(z) %*% w)))
  #                    Valores observados Média dos vizinhos
  # Valores observados         1.00000000        -0.04208797
  # Média dos vizinhos        -0.04208797         1.00000000
  
  # Índice de Moran com dados originais.
  (stat <- moran(z, w))
  # [1] -0.008995086
  
  hist(z)
  ppt(z, w, stat = stat, main = "I de Moran", xlab = "I")
  
  
  # [1] 0.4111589
  par(mfrow = c(1, 1))
  
  ## Teste usando spdep
  spdep::moran.test(z, spdep::mat2listw(w))
  # 
  #   Moran I test under randomisation
  # 
  # data:  z  
  # weights: spdep::mat2listw(w)    
  # 
  # Moran I statistic standard deviate = 0.12925, p-value = 0.4486
  # alternative hypothesis: greater
  # sample estimates:
  # Moran I statistic       Expectation          Variance 
  #      -0.008995086      -0.015873016       0.002831704
  spdep::moran.mc(z, spdep::mat2listw(w), nsim = 10000)
  # 
  #   Monte-Carlo simulation of Moran I
  # 
  # data:  z 
  # weights: spdep::mat2listw(w)  
  # number of simulations + 1: 10001 
  # 
  # statistic = -0.0089951, observed rank = 5846, p-value = 0.4155
  # alternative hypothesis: greater
  
  
  
  
########################################################################################
# Aula 12/11/2019 - 
########################################################################################

########################################################################################
# Aula 12/11/2019 - 
########################################################################################

########################################################################################
# Aula 12/11/2019 - 
########################################################################################

########################################################################################
# Aula 12/11/2019 - 
########################################################################################
