## Aceitação-rejeição --------------------------------------------------
## Define funções
f <- function(x) dcauchy(x, 0, 1)
g <- function(x) dnorm(x, 0, 1)
## Máximo M
(M <- optimize(f = function(x) {f(x)/g(x)},
               interval = c(0, 1), maximum = TRUE)$objective)
# [1] 2.669744
curve(f, from = 0, to = 1, col = 4)
curve(g, from = 0, to = 1, add = TRUE, lty = 2)
curve(M * g(x), add = TRUE, lty = 2, lwd = 2)
legend("right", legend = c("f(x)", "g(x)", "M g(x)"),
       lty = c(1, 2, 2), col = c(4, 1, 1), lwd = c(1, 1, 2), bty = "n")


## Simula com número fixo
N <- 1e5
## Amostra da proposta U(0,1)
y <- rnorm(N)
## Amostra u também de U(0,1)
u <- rnorm(N)
## Calcula a razão
r <- f(y)/(M * g(y))
## x serão os valores de y onde u < r
x.ar <- y[u < r]
## Aceitados
ua <- u[u < r]


## Metropolis-Hastings -------------------------------------------------
## Simula com número fixo
N <- 1e5
x <- numeric(N)
x[1] <- rnorm(1)
k <- 0 # para contar quantos foram aceitos
for (i in 2:N) {
  y <- rnorm(1)
  num <- f(y) * g(x[i - 1])
  den <- f(x[i - 1]) * g(y)
  alpha <- num/den
  u <- rnorm(1)
  if (u <= alpha) {
    x[i] <- y
  } else {
    x[i] <- x[i - 1]
    k <- k + 1     # contagem doa aceitos
  }
}

## Taxa de aceitação - AR
1/M # teórica
# [1] 0.3745677
length(ua)/N
# [1] 0.37523
## Taxa de aceitação - MH
k/N
# [1] 0.5426
## Compara amostras com acumulada teórica
par(mfrow = c(1, 2))
plot(ecdf(x.ar), main = "Aceitação-rejeição")
curve(pcauchy(x, 0, 1), add = TRUE, from = 0, to = 1, col = 2)
plot(ecdf(x), main = "Metropolis-Hastings")
curve(pcauchy(x, 0, 1), add = TRUE, from = 0, to = 1, col = 2)
legend("right", legend = c("Empírica", "Teórica"),
       lty = 1, col = 1:2, bty = "n")


## Compara autocorrelação
acf(x.ar, main = "Aceitação-rejeição")
acf(x, main = "Metropolis-Hastings")

## Compara as duas cadeias
par(mfrow = c(2, 1))
plot.ts(x.ar[5000:5200], main = "Aceitação-rejeição")
plot.ts(x[5000:5200], main = "Metropolis-Hastings")

par(mfrow = c(1, 1))

