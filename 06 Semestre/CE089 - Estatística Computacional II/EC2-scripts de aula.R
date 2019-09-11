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
# Aula 24/09/2019 
########################################################################################



########################################################################################
# Aula 27/09/2019 
########################################################################################

    
    
    
    