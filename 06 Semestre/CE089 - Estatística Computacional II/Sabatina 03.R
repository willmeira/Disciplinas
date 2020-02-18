randd <- function(x, px) {
  u <- 0.8204558
  x <- 1
  Fx <- px[x]
  while(u > Fx) {
    x <- x + 1
    Fx <- Fx + px[x]
  }
  return(x)
}

k <- 6
x <- 1:6
px <- c(0.15,0.12,0.21,0.12,0.2,0.2)
randd(x = x, px = px)


##----------------------------------------------------------------------
## Simulando valores da lomax a partir de Uniformes.



iFx <- function(u, lambda,k){
  ((1-u)^(-1/k)-1)/lambda
}

## Números uniformes. Poderia usar a rand0() da aula passada.
u <-c(0.29, 0.60, 0.18, 0.13, 0.59)

## Números de distribuição Exponencial.
x <- iFx(u, lambda=0.8,k=0.5)
head(x)
######################################################################################
## Triangular com a = -1, c = 0, b = -1.
f <- function(x) {
  0 + ifelse(x > 0, 1 - x, x + 1) * (x > -1) * (x < 1)
}
## Parabólica em [-1, 1]
g <- function(x) 3/4 * (1 - x^2) * (x >= -1) * (x <= 1)



######################################################################################

# Densidade da X, distribuição alvo. triangular
f_X <- function(x, r) {
  b <- r; a <- -r; c <- 0
  f <- 0 + 2 * (x - a)/((b - a) * (c - a)) * (x <= c) * (x > a) +
    2 * (b - x)/((b - a) * (b - c)) * (x > c) * (x < b)
  return(f)
}
# Densidade da Y, distribuição candidata.parabolica.
f_Y <- function(y, r) {
  f <- 3/4*(1 - y^2) * (y >= -r) * (y <= r)
  return(f)
}
# Gráficos.
curve(f_X(x, r = 1), from = -1, to = 1)
curve(f_Y(y, r = 1), xname = "y", add = TRUE, col = 2, lty = 2)
curve(4/3 * f_Y(y, r = 1), xname = "y", add = TRUE, col = 2)
M <- 4/3
x <- numeric(n)
#y <- c(−0.44,0.67,0.34,−0.33 ,0.05,−0.39,0.62,−0.80,−0.94,0.18 )
#u <- c(0.77,0.95,0.18,0.13,0.34,0.69,0.65,0.89,0.66,-0.18)




y = 0.85
u <-0.73
ratio <- f_X(y, r = 1)/(M * f_Y(y, r = 1))
if (u < ratio ) {
  x <- y
  print("u < ratio, então valor aceito.")
} else {
  print("u >= ratio, então valor descartado.")
}


#-----------------------------------------------------------------

# a = 7^5
# m = 2^31 - 1
set.seed(6)
rand_congr0 <- function(n = 1, x0, a = 2^16, m = 2^31) {
  if (missing(x0)) x0 <- as.integer(Sys.time())
  stopifnot(x0 < m)
  n <- n + 1
  x <- vector(mode = "integer", length = n)
  x[1] <- x0
  for (i in 2:n) {
    x[i] <- (a + 3) %% m
  }
  return(x[-1])
}
rand_congr0(n = 100,1000)
#------------------------------------------------------------------
plot(rnorm(10000), rnorm(10000))


## Gerando valores da normal pelo algoritmo de Box-Muller
Nsim <- 2500
## Amostra das uniformes
u1 <- runif(Nsim)
u2 <- runif(Nsim)
## Raio
R <- sqrt(-2 * log(u1))
## Angulo
T <- 2 * pi * u2
x1 <- R * cos(T)
x2 <- R * sin(T)
plot(x1, x2, xlim = c(-4, 4), ylim = c(-4, 4))
## Confere
par(mfrow = c(1, 2))
plot(ecdf(c(x1, x2)))
curve(pnorm(x), add = TRUE, col = 2)
q <- qnorm(ppoints(Nsim))
qqplot(q, c(x1, x2))
abline(0, 1, col = 2)
par(mfrow = c(1, 1))
## Uma função mais eficiente
boxmuller <- function(n) {
  ## Executa o algoritmo em somente metade dos valores requeridos
  m <- ceiling(n/2)
  u1 <- runif(m)
  u2 <- runif(m)
  R <- sqrt(-2 * log(u1))
  T <- 2 * pi * u2
  x <- c(R * cos(T), R * sin(T))
  ## Se n for par, retorne tudo, caso contrário, tire um valor
  if (n %% 2 == 0) x else x[-1]
}
boxmuller(2)
# [1] 0.9234418 1.0826671
boxmuller(3)
# [1]  0.66055651  0.07799255 -1.80340422
boxmuller(4)
# [1]  0.6975372 -0.3609859 -0.1677477 -0.4755585
boxmuller(5)
# [1] 1.0071460 1.0662797 0.1684460 0.1523806 0.3098626

















## Confere os métodos padrão para a geração de valores aleatórios. O
## primeiro algoritmo é da Uniforme, o segundo é o da Normal e o
## terceiro é o método utilizado pela função sample(). Veja os detalhes
## em help(Random)
RNGkind()
# [1] "Mersenne-Twister" "Inversion"        "Rejection"
## Define semente e altera o gerados para o de Box-Muller
set.seed(5, normal.kind = "Box-Muller")
## Box-MUller pela rnorm
xx <- rnorm(10)
xx
sum(xx)


## Box-Muller implementado aqui
yy <- boxmuller(10)
sum(yy)

RNGkind()
# [1] "Mersenne-Twister" "Box-Muller"       "Rejection"
## Volta para o padrão da rnorm
set.seed(5, normal.kind = "Inversion")
RNGkind()
# [1] "Mersenne-Twister" "Inversion"        "Rejection"
## Gera valores com o algoritmo padrão
zz <- rnorm(10)
zz
sum(zz)
##############################################################

rcl <- function(n, x0, m, a, c, unit = TRUE) {
  x <- integer(n + 1)
  x[1] <- x0
  for(i in 2:length(x)) {
    x[i] <- (a * x[i - 1] + c) %% m
  }
  if(unit) x <- x/m
  return(x[-1])
}


f(xi)=(a   xi−1 + c) mod m
f(xi)=(2^16     + 3)xi−1 mod 231

rcl(n = 12, x0 = 7, m = 2^31 - 1, a = 7^5, c = 0, unit = FALSE)
rcl(n = 5, x0 = 0, m = 2^31, a = 2^16, c = 3, unit = FALSE)
rcl(n = 5, x0 = 0, m = 2^31, a = 2^16, c = 3, unit = TRUE)
#  [1]     117649 1977326743  621132276  452154665 1566311569 1143995257
#  [7]  707192808 1615021558 1621510873 1165762081 1469983786 1365616214
rcl(n = 12, x0 = 7, m = 2^31 - 1, a = 7^5, c = 0, unit = TRUE)



# a = 7^5
# m = 2^31 - 1
set.seed(4)
rand_congr0 <- function(n = 1, x0, a = 2^16, m = 2^31) {
  if (missing(x0)) x0 <- as.integer(Sys.time())
  stopifnot(x0 < m)
  n <- n + 1
  x <- vector(mode = "integer", length = n)
  x[1] <- x0
  for (i in 2:n) {
    x[i] <- (a + 3) %% m
  }
  return(x[-1])
}
rand_congr0(n = 5,1000)
rand_congr0(n = 5,10000000)

















