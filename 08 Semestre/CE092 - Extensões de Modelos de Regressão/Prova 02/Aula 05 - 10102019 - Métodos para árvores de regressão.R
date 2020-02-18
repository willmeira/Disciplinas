## Aula 10/10 - Extensões de Modelos de Regressão

rm(list = objects())
library(lattice)
library(latticeExtra)

#-----------------------------------------------------------------------
# Produção de teca.

# Endereço das tabelas.
pre <- "https://raw.githubusercontent.com/walmes/EACS/master/data-raw/"
files <- c(hidric = "teca_crapar.csv",
           quimic = "teca_qui.csv",
           prod = "teca_arv.csv")
urls <- paste0(pre, files)
names(urls) <- names(files)

# Lista com as tabelas.
da <- sapply(urls,
             FUN = read.table,
             header = TRUE,
             sep = ";",
             simplify = FALSE)
str(da)

# Manipular as tabelas para fazer o merge.
da$quimic <- subset(da$quimic, cam == "[0, 5)", select = -cam)
da$hidric <- subset(da$hidric, cam == "[0, 5)", select = -c(cam, cad))
da$prod <- subset(da$prod, select = c(loc, prod))
str(da)

# Aplica o merge recursivamente.
teca <- Reduce(f = merge, x = da)

# Elimina a variável identificadora (agora desnecessária).
teca$loc <- NULL

# Estrutura da tabela.
str(teca)


#-----------------------------------------------------------------------
# Preço de imóveis para 7 bairros em Curitiba.

u <- "http://www.leg.ufpr.br/~walmes/data/ap_venda7bairros_cwb_210314.txt"
ap <- read.table(file = u, header = TRUE, sep = "\t")
# str(ap)

# Usar o log do preço e da metragem.
ap <- transform(ap,
                larea = log10(area),
                lpreco = log10(preco),
                preco = NULL,
                area = NULL)

# Exclui outliers.
# plot(lpreco ~ larea, data = ap)
# dput(with(ap, identify(larea, lpreco)))

ap <- ap[-c(1966L, 2696L, 3267L), ]
ap <- subset(ap, lpreco > 4)
rownames(ap) <- NULL
str(ap)


#-----------------------------------------------------------------------
# Análise exploratória.

xyplot(lpreco ~ larea | cut(vagas, c(0:3, Inf)),
       data = ap,
       as.table = TRUE)


xyplot(lpreco ~ larea | bairro,
       data = ap,
       as.table = TRUE) +
  layer(panel.smoother(...))



#-----------------------------------------------------------------------
# Ajuste de árvore de regressão para os dados de teca.

library(rpart)
library(rpart.plot)
# help(rpart, help_type = "html")

# Ajuste do modelo.
m0 <- rpart(prod ~ ., data = teca)

# Resumo do ajuste.
summary(m0)


# Visualização da árvore de regressão.
rpart.plot(m0)


# Valores preditos.
predict(m0)


# Valores preditos (médias em cada região).
unique(sort(predict(m0)))


table(sort(predict(m0)))

#-----------------------------------------------------------------------
# Deixar a árvore crescer mais.

m1 <- rpart(prod ~ .,
            data = teca,
            control = list(minsplit = 5,
                           cp = 0.001))

rpart.plot(m1)

unique(sort(predict(m1)))

#-----------------------------------------------------------------------
# Ajuste para os dados de imóveis.

m0 <- rpart(lpreco ~ ., data = ap)

# Resumo do ajuste.
summary(m0)


# Visualização da árvore de regressão.
rpart.plot(m0)

# Visualização alternativa.
plot(m0)
text(m0)

# Valores preditos (médias em cada região).
unique(sort(predict(m0)))

# Importância das variáveis.
cbind(m0$variable.importance)

# Soma de quadrados residual do modelo nulo.
m0$frame$dev[1]                      # SQres do ~1.

(m0$frame$n[1] - 1) * var(ap$lpreco) # SQres do ~1.


# R².
1 - sum(residuals(m0)^2)/(m0$frame$dev[1])

# Criando um grid nas variáveis consideradas pela árvore.
grid <- with(ap,
             expand.grid(larea = seq(min(larea, na.rm = TRUE),
                                     max(larea, na.rm = TRUE),
                                     length.out = 50),
                         vagas = seq(min(vagas, na.rm = TRUE),
                                     max(vagas, na.rm = TRUE),
                                     length.out = 50),
                         bairro = levels(bairro)[1],
                         quartos = median(quartos, na.rm = TRUE),
                         banheiros = median(banheiros, na.rm = TRUE),
                         suites = median(suites, na.rm = TRUE),
                         KEEP.OUT.ATTRS = FALSE))
str(grid)

# Predição para os valores no grid.
grid$y <- predict(m0, newdata = grid)

# Um fator para indicar as diferentes regiões.
yp <- predict(m0, newdata = ap)
yp <- rank(yp, ties.method = "min")
yp <- as.integer(factor(yp))

# Visualização das regiões criadas pelos cortes perpendiculares aos
# eixos.
levelplot(y ~ larea + vagas, data = grid, contour = TRUE) +
  layer(panel.points(larea, vagas, col = yp), data = ap)

# Diagrama de dispersão 3D.
cloud(lpreco ~ larea + vagas, data = ap, col = yp)

# Os patamares.
wireframe(y ~ larea + vagas, data = grid, drape = TRUE)

# Deixar a árvore crescer mais.
m0 <- rpart(lpreco ~ .,
            data = ap,
            control = list(cp = 0.0025))

# Visualização da árvore de regressão.
rpart.plot(m0)

# Visualização alternativa.
plot(m0)
text(m0)

# Valores preditos (médias em cada região).
unique(sort(predict(m0)))

# Importância das variáveis.
cbind(m0$variable.importance)

# R².
1 - sum(residuals(m0)^2)/(m0$frame$dev[1])

# Sempre fazer a predição com esses inputs.
pred <- subset(ap, select = -lpreco)
names(pred)

# ID de cada registro.
n <- nrow(ap)
s <- 1:n

# Índices para amostragem com reposição (bootstrap).
i <- sample(s, size = n, replace = TRUE)

# Qual a proporção de registros únicos tomados?
u <- unique(i)
length(u)/n


# Quais as observações que ficaram de fora?
out <- which(!(s %in% u))
head(out)

tail(out)


# Amostra boostrap da tabela.
ap_bs <- ap[i, ]

# Ajuste do modelo.
m_bs <- rpart(lpreco ~ ., data = ap_bs)

# Valores preditos.
y_bs <- predict(m_bs, newdata = pred)
head(y_bs)

# Out of bag mean square error.
sum((ap[out, ]$lpreco -
       predict(m_bs, newdata = pred[out, ]))^2)/length(out)
#-----------------------------------------------------------------------
# Repetir B vezes.

set.seed(102030)
B <- 200
j <- 1
frac <- numeric(B)
fits <- replicate(B,
                  simplify = FALSE,
                  expr = {
                    # Reamostra com reposição.
                    i <- sample(s, size = n, replace = TRUE)
                    frac[j] <<- length(unique(i))/n
                    j <<- j + 1
                    ap_bs <- ap[i, ]
                    # Ajuste da árvore aos dados de treino.
                    m_bs <- rpart(lpreco ~ ., data = ap_bs)
                    return(m_bs)
                  })

# A proporção de valores usados nas amostras bootstrap.
mean(frac)


# Os preditos em cada "ensacamento" dos dados.
pred$y <- sapply(fits, FUN = predict, newdata = pred)
str(pred)

# Predição para os primeiros casos.
head(pred)


# Estatísticas para o B valores preditos para alguns casos.
mean(as.vector(pred[1, "y"]))

## [1] 5.495819

var(as.vector(pred[1, "y"]))

## [1] 0.002245619

# O predito médio ("a sabedoria das multidões").
pred$ym <- rowMeans(pred$y)

# Predito contra observado.
# x11()
xyplot(pred$ym ~ ap$lpreco, aspect = "iso") +
  layer(panel.abline(a = 0, b = 1))

# Qual o predito para o imóvel mediano?
new <- lapply(pred,
              FUN = function(x) {
                if (is.numeric(x)) {
                  median(x, na.rm = TRUE)
                } else {
                  levels(x)[1]
                }
              })
new



# Predito por cada árvore.
y <- sapply(fits, FUN = predict, newdata = new)

# Distribuição dos B valores preditos e valor médio.
plot(density(y))
rug(y)
m <- mean(y)
abline(v = m, col = 2)



library(ipred)

# help(package = "ipred", help_type = "html")

# Fazendo bagging.
bg <- bagging(lpreco ~ ., data = ap, nbagg = 200, coob = TRUE)
bg

## 
## Bagging regression trees with 200 bootstrap replications 
## 
## Call: bagging.data.frame(formula = lpreco ~ ., data = ap, nbagg = 200, 
##     coob = TRUE)
## 
## Out-of-bag estimate of root mean squared error:  0.1048

# Predito contra observado.
xyplot(predict(bg, newdata = ap) ~ ap$lpreco, aspect = "iso")


## 1.4 Árvores de regressão com random forests

#-----------------------------------------------------------------------
# Prototipando.

xvars <- names(teca)[1:20]
nv <- floor(length(xvars)/3)

# Seleciona variáveis preditoras.
v <- sample(xvars, size = nv, replace = FALSE)
v


# Seleciona registros.
n <- nrow(teca)
i <- sample(1:n, size = n, replace = TRUE)

# Ajusta o modelo.
m0 <- rpart(prod ~ .,
            data = teca[i, c(v, "prod")],
            control = list(minsplit = 3,
                           cp = 0.001))
m0


# Visualiza.
rpart.plot(m0)

# Replicar.
set.seed(302010)
B <- 1000
rf <- replicate(B,
                simplify = FALSE,
                expr = {
                  v <- sample(xvars, size = nv, replace = FALSE)
                  i <- sample(1:n, size = n, replace = TRUE)
                  m0 <- rpart(prod ~ .,
                              data = teca[i, c(v, "prod")],
                              control = list(minsplit = 3,
                                             cp = 0.01))
                  return(m0)
                })

# ATTENTION: na árvore de regressão, o sorteio das variáveis é feito
# após cada split e não uma única vez como o que está neste código.

# Obtenção dos preditos.
y_rf <- sapply(rf, FUN = predict, newdata = teca)
head(y_rf[, 1:6])


# Cálculo da média.
ym <- rowMeans(y_rf)

xyplot(ym ~ teca$prod,
       aspect = "iso",
       type = c("p", "smooth")) +
  layer(panel.abline(a = 0, b = 1))


# Correlação entre observado e predito.
cor(ym, teca$prod)

## [1] 0.8953036

#-----------------------------------------------------------------------

library(randomForest)

rf <- randomForest(prod ~ .,
                   data = teca,
                   ntree = 3,
                   mtry = 2,
                   keep.inbag = TRUE,
                   keep.forest = TRUE)
rf

## 
## Call:
##  randomForest(formula = prod ~ ., data = teca, ntree = 3, mtry = 2,      keep.inbag = TRUE, keep.forest = TRUE) 
##                Type of random forest: regression
##                      Number of trees: 3
## No. of variables tried at each split: 2
## 
##           Mean of squared residuals: 1161.826
##                     % Var explained: 11.81

1 - sum(rf$oob.times == 0)/length(rf$oob.times)

## [1] 0.7083333

str(rf)


# Número de vezes que cada variável foi usada em cada bag.
head(rf$inbag)


# Valores preditos.
sort(unique(predict(rf)))

# Inspecionando uma das árvores da floresta.
t <- 1
one_tree <- getTree(rf, k = t, labelVar = TRUE)
nrow(one_tree)

rf$forest$ndbigtree[t]


# Outra forma de acessar as variáveis usadas.
rf$forest$bestvar[, t]

names(rf$forest$xlevels)[rf$forest$bestvar[, t]]

#-----------------------------------------------------------------------
# help(package = "randomForest", help_type = "html")

rf <- randomForest(prod ~ .,
                   data = teca,
                   ntree = B,
                   mtry = nv)
rf

# Valores preditos.
yp <- predict(rf)

# Correlação entre observado e predito.
cor(yp, teca$prod)

xyplot(ym + yp ~ teca$prod,
       aspect = "iso",
       auto.key = TRUE,
       type = c("p", "smooth")) +
  layer(panel.abline(a = 0, b = 1))




# ATTENTION: a randomForest() criou árvores mais profundas que o código
# didático feito algumas linhas acima. Além do mais, as `m` variáveis
# são sorteadas a cada split e não uma vez apenas.

# o <- order(yp)
# xyplot(ym[o] + yp[o] ~ seq_along(ym), auto.key = TRUE)

# Para construir o gráfico de uma árvore, leia esse post: 
# https://stats.stackexchange.com/questions/41443/how-to-actually-plot-a-sample-tree-from-randomforestgettree.

#-----------------------------------------------------------------------
# Dados de imóveis.

nv <- floor(sqrt(ncol(ap) - 1))
nv

## [1] 2

ap2 <- na.omit(ap)

rf <- randomForest(lpreco ~ .,
                   data = ap2,
                   ntree = 100,
                   mtry = nv)
rf

## 
## Call:
##  randomForest(formula = lpreco ~ ., data = ap2, ntree = 100, mtry = nv) 
##                Type of random forest: regression
##                      Number of trees: 100
## No. of variables tried at each split: 2
## 
##           Mean of squared residuals: 0.007040021
##                     % Var explained: 90.87

# Valores preditos.
yp <- predict(rf)

cor(yp, ap2$lpreco)

## [1] 0.9533648

# Correlação entre observado e predito.
xyplot(yp ~ ap2$lpreco,
       aspect = "iso",
       auto.key = TRUE,
       type = c("p", "smooth")) +
  layer(panel.abline(a = 0, b = 1))


importance(rf, type = 2)

im <- importance(rf, type = 2)
100 * im/sum(im)

varImpPlot(rf)

library(gbm)

# help(package = "gbm", help_type = "html")

rb <- gbm(lpreco ~ .,
          data = ap,
          distribution = "gaussian",
          n.trees = 100,
          shrinkage = 0.05,
          interaction.depth = 1,
          train.fraction = 0.7,
          n.minobsinnode = 10,
          cv.folds = 3,
          keep.data = TRUE,
          verbose = FALSE,
          n.cores = 1)

rb

summary(rb)

# Valores preditos.
yp <- predict(rb)

## Using 100 trees...

# Correlação entre observado e predito.
cor(yp, ap$lpreco)

## [1] 0.930845

xyplot(yp ~ ap$lpreco,
       aspect = "iso",
       auto.key = TRUE,
       type = c("p", "smooth")) +
  latticeExtra::layer(panel.abline(a = 0, b = 1))
