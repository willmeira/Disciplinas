a<-c(4, 90.3, TRUE, "a")
class(a)

b<-c(2, 4, 6) + 1i 
class(b)


c<-factor(c("B", "C", "A"))
class(c)
unclass(factor)
c


d<-c(10L, 20L, 300L) 
class(d)
typeof(d)

e<-c(3, 5, 6) 
class(e)
typeof(e)




f<-c(1:100)^2
f

sum(f)



g<-c(1, 2, 3) + 10
g

h<-c(1:6) + 5
h

g+h

i<-exp(4)
i
j<-seq(from = 0, to = i, length.out = 100)
sum(j)



k<-seq(from=1,to=100, by=2)^2
k
sum(k)


l<-c(1, 2, 3)
m<-c(3, 4, 5, 6) 
l*m





shift <- function(x, ...) {
  a <- min(x, ...)
  b <- max(x, ...) - a
  res <- (x - a)/b
  return(res)
}
shift(12)

shift(c(2, 3, 6))
shift(c(-3, -5, 6))
shift(-15244)
shift(c(9, 3, 6))


Crie uma matriz 4 x 3 com os números de 1 a 12, preenchendo por linha, e chame este objeto de A. Crie outra matriz 4 x 4, 
preenchendo com valores de uma amostra da distribuição uniforme entre 0 e 1 (por coluna). Use set.seed(123) imediatamente 
antes de gerar os números aleatórios e chame este objeto de B. Por fim, crie um vetor com os valores 10, 20, 30, 40 e armazene 
no objeto C. Com isso, avalie as afirmações abaixo.

Obs.: em questões que envolvem soma, arredonde o resultado final para 3 casas decimais (ex.: round(..., 3)).

Escolha uma ou mais:
  a. O produto matricial t(C) %*% A resulta em uma matriz 1 x 3.
b. A soma do produto matricial de B por A e 182.69.
c. Os produtos A * C e A %*% C geram os mesmos resultados.
d. A soma de todos os elementos em B é 9.446.
e. O produto matricial de A por B não é possível porque o número de colunas em A é diferente do número de linhas em B.
f. O produto entre uma matriz qualquer e um escalar resulta sempre em um escalar.
g. A soma do produto matricial de B por C é 234.474.
h. Os produtos A * C e C * A não são válidos pois A é uma matriz e C é um vetor.
i. O primeiro elemento do resultado do produto matricial de C por A é 700.
j. A soma de todos os elementos em A é 55.




da <- data.frame(c1 = rnorm(10),
                 c2 = runif(10))

A <- matrix(1:12, nrow = 4, ncol = 3, byrow = TRUE)
A

set.seed(123)
B <- matrix(runif(16), nrow = 4, ncol = 4)
B
GG<-(runif(16))
GG
 
sum(B)
    
C<-c(10, 20, 30, 40)

t(C) %*% A

t(C)

sum(B %*% A)

A %*% C
C %*% A
sum(A)


sum(B %*% C)
