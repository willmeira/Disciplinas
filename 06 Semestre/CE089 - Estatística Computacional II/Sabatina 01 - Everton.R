#---------------trabalho 1 de R2----------------------------
#------------------quest?o 1--------------------------------
a<- c(10L, 20L, 300L) #ser? "numeric".
typeof(a)

b<-factor(c("B", "C", "A"))
b

unclass(b)

c<-c(2, 4, 6) + 1i
typeof(c)

d<-c(4, 90.3, TRUE, "a")
typeof(d)

e<-c(3, 5, 6)
typeof(e)
class(e)
unclass(e)

# Will
A = c(2, 4, 6) + 1i
str(A)
typeof(A)
class(A)

B = c(10L, 20L, 300L)
str(B)
typeof(B)
class(B)

C =  c(4, 90.3, TRUE, "a")
str(C)
typeof(C)
class(C)

D = c(3, 5, 6)
typeof(D)
str(D)
class(D)
sum(D)


E = factor(c("B", "C", "A"))
str(E)
typeof(E)
class(E)


#-----------------qust?o 2------------------------------

b<-(seq(from = 1, to = 100, by=2))^2
sum(b)  

k<-seq(from=1,to=100, by=2)^2
k
sum(k)



A2<-(seq(from = 1, to = 100))^2  
sum(A2)  

B2<-seq(from = 0, to = exp(4), length.out = 100 )
sum(B2)

i<-exp(4)
i
j<-seq(from = 0, to = i, length.out = 100)
sum(j)

C2 =  c(1, 2, 3) + 10 com o vetor 1:6 + 5 resulta no vetor 2 4 6 5 7 9.


d<-c(1, 2, 3) + 10 
x<-1:6 + 5  
y<-d+x
y  

c(1, 2, 3) e c(3, 4, 5, 6) é 3 8 15, 
e<-c(1,2,3) ;k<-c(3,4,5,6)
e*k




#------a-------------------quest?o 3---chutei tudo-----------------------
shift <- function(x, ...) {
  a <- min(x, ...)
  b <- max(x, ...) - a
  res <- (x - a)/b
  return(res)
  
help(shift)
  
b<-shift(c(9, 3, NA, 6))
  


shift <- function(x, ...) {
  a <- min(x, ...)
  b <- max(x, ...) - a
  res <- (x - a)/b
  return(res)
}

shift(c(2, 3, 6))
typeof(c(9, 3, NA, 6))
typeof(c(9L, 3L, 2L, 6L))

shift(c(9L, 3L, 2L, 6L))

shift(12)


#-----------------------quest?o 4------------------------

A<- matrix(1:12, nrow = 4, ncol = 3,byrow=T)
D<- matrix(1:12, nrow = 3, ncol = 4,byrow=T)
set.seed(123)
B<-matrix(runif(16,0,1),nrow=4,ncol=4)
C<-c(10,20,30,40)
t(C) %*% A
sum(B %*% C)

A * C e A %*% C 

a+c
a%%c
round(sum(B),3)
A %*% A
sum(b%%c)
b%%a
A*4
c*a
c%%a
sum(a)
A * C
C %*% A
sum(A)

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




