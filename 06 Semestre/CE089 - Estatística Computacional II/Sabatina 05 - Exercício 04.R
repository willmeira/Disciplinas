
rm(list = ls())

## Questão 03

fun <- function(n, m, k, p) {
  tmean <- numeric(m)
  for (i in 1:m) {
    sigma <- sample(c(1, 10), size = n,
                    replace = TRUE, prob = c(p, 1-p))
    x <- sort(rnorm(n, 0, sigma))
    tmean[i] <- sum(x[(k+1):(n-k)]) / (n-2*k)
  }
  mse.est <- mean(tmean^2)
  se.mse <- sqrt(mean((tmean-mean(tmean))^2)) / sqrt(m)
  return(c(mse.est, se.mse))
}

# Essa função foi utilizada da seguinte forma:
  
n <- 20
K <- n/2 - 1
m <- 1000
# p <- 0.9
mse <- matrix(0, n/2, 6)

for (k in 0:K) {
  mse[k+1, 1:2] <- fun(n = n, m = m, k = k, p = 0.5)
  mse[k+1, 3:4] <- fun(n = n, m = m, k = k, p = .95)
  mse[k+1, 5:6] <- fun(n = n, m = m, k = k, p = .9)
}
mse*20


fun(n = n, m = m, k = k, p = 1.0)*20
fun(n = n, m = m, k = k, p = .95)*20
fun(n = n, m = m, k = k, p = .9)*20
fun(n = n, m = m, k = k, p = .5)*20


## Questão 04

x <- c(8.84, 8.91, 9.47, 9.83, 10.04, 10.42, 10.58, 11.32)
sd(x)
(sx <- sd(x))
n=8

theta.jack <- numeric(n)
for(i in 1:n) {
  theta.jack[i] <- sd(x[-i])
}
theta.jack

(pv <- n * sx - (n - 1) * theta.jack)

mean(pv)
sx

sqrt(var(pv)/n)

mean(pv) + qt(c(.025, .975), df = n - 1) * sqrt(var(pv)/n)


round(cbind(Amostra = x, "Pseudo-valores" = pv,
            "Estimativas parciais" = theta.jack), 2)




(n <- length(x))
# [1] 30
(xbar <- mean(x))
# [1] 99.76448
(s2 <- var(x))

sd <- sd(x)
# [1] 24.06053

(ep <- sqrt(s2/n))
# [1] 0.8955544

## Estimativas parciais
theta.jack <- numeric(n)
for(i in 1:n) {
  # theta.jack[i] <- mean(x[-i])
  theta.jack[i] <- sd(x[-i])
}
round( theta.jack, 4)

pv_sd <- n*sd - (n-1)*theta.jack
mean(pv_sd)
sd
sqrt(var(pv_sd)/n)

round( cbind(Amostra=x, "Pseudo Valores" = pv_sd, "est. parc" = theta.jack), 2)

mean(pv_sd) + qt(c(0.025, 0.975), df = n-1)*sqrt(var(pv_sd)/n )
