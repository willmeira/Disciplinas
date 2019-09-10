#  a. Se m é uma matriz, então m <- 0 fará com que todos os elementos da matriz sejam iguais a zero.
m <-matrix(seq(1,12,1),3,3)
m <- 0
#FALSE, pois transforma a matriz em um numero unico

# b. Ao indexar um data frame com [ usando o argumento drop = FALSE, a classe do objeto resultante será um data frame.
da <- data.frame(A = 4:1, B = c(2, NA, 5, 8))
g<-da[, "A", drop = FALSE]
class(da)
class(g)
#TRUE

# c. Na indexação de um vetor, se o índice utilizado for um número não inteiro, a seleção dos elementos não poderá ser realizada
cont <- c(8, 4, NA, 9, 6, 1, 7, 9)
cont
cont[1.1]
#False


# d. Considere que x é uma lista com três elementos. O resultado de x[c(1, 3)] será também uma lista.
mat <- matrix(1:9, nrow = 3)
lis <- list(c(3, 8, 7, 4), mat, 5:0)
lis[3]  
class(lis)
class(lis[c(1, 3)])
#TRUE

# e. Se x <- c("y", "u"), então letters[x] retorna "y" "u".
x <- c("y", "u") 
letters[x] 
#retorna "y" "u"?
#FALSE


## QUESTÃO 02 --------------------------------------------------

# Obs. 1: quando necessário, arredonde o resultado com a função round(), com o número de casas decimais adequado.

# Obs. 2: em algumas questões será necessário o uso das funções table(), unique() e grep(). Veja a página de ajuda dessas funções.

df <-read.csv("pib_gapminder.csv",header = T,dec = ".")
head(df)

#  a. O número de informações por continente é o seguinte: África (624), Américas (300), Ásia (396), Europa (360), Oceania (24).
tapply(df$continente, index, cont)
tapply(df$continente,  FUN = count)
apply(X = notas[, provas], MARGIN = 1, FUN = mean)
# EXcel TRUE

#  b. Consideranto todos os anos disponíveis na base de dados, a média do PIB per capita do Brasil foi de 5829.317.
## Média da prova 1 por situação
tapply(df$pibPercap, df$pais,  mean)
# EXcel TRUE

#  c. A média do número de habitantes no continente europeu nos últimos 3 anos da base de dados (1997, 2002, 2007) foi de 19.258.517.
tapply(df$pop, df$continente, mean)
aggregate(pop ~ continente, data = df, FUN = mean)
# EXcel TRUE

#  d. Os dados do Marrocos não constam nessa base de dados.
#FALSE

#  e. Apenas 2 países possuem expectativa de vida acima de 82 anos.
#TRUE ???

#  f. A média do PIB per capita dos 4 países que compõem o Mercosul atualmente foi de 6281.
#FALSE ???

#  g. A expectativa de vida no Brasil em 2007 era de 72.39 anos.
#TRUE

#  h. Os dados correspondem a uma série temporal de 56 anos.
#????

#  i. Apenas 3 países da Oceania estão presentes na base de dados.
#FALSE

#  j. A população do Sri Lanka em 1952 era de 7982342 habitantes.
#TRUE


#########################

x <- 1:100
y <- numeric(length(x))
for(i in seq_along(x)) {
  if (x[i] %% 5 == 0) {
    y[i] <- x[i]/5
  } else {
    if(x[i] %% 2 == 0) {
      y[i] <- x[i]/2
    } else {
      y[i] <- x[i]
    }
  }
}




##########################

## Simula do modelo
set.seed(123)
n <- 1000; b0 <- 10; b1 <- 0.5
x <- rnorm(n, mean = 150, sd = 15)
sigma2 <- 20
y <- b0 + b1*x + rnorm(n, mean = 0, sd = sqrt(sigma2))

## Número de amostras
r <- 1e4
## Número de elementos em cada amostra
m <- 100

##----------------------------------------------------------------------
## Bootstrap
b0.boot <- numeric(r)
b1.boot <- numeric(r)
set.seed(123)
for(i in 1:r){
  select <- sample(1:length(y), size = m, replace = TRUE)
  x.boot <- x[select]
  y.boot <- y[select]
  mm <- lm(y.boot ~ x.boot)
  b0.boot[i] <- coef(mm)[1]
  b1.boot[i] <- coef(mm)[2]
}
##----------------------------------------------------------------------

