# Capítulo 01

par(mar = c(4, 4, .1, .1))
plot(pressure, type = 'b', pch = 19)

knitr::kable(
  head(iris, 20), caption = 'Here is a nice table!',
  booktabs = TRUE
)

# Capítulo 02

library(labestData)

str(ZimmermannTb3.5)

# Nomes curtos dão mais agilidade.
zim <- ZimmermannTb3.5

# Atribui novos rótulos para os níveis.
levels(zim$adub) <- c("100/0/0", "50/50/0", "16/84/0", "16/42/42")

# Produção média.
aggregate(prod ~ adub, data = zim, FUN = mean)

install.packages("tables")
library(tables)

# Tabela com medidas descritivas: média, desvio-padrão e número de obs.
tabular(adub + 1 ~ prod * (mean + sd + length), data = zim)

library(lattice)

xyplot(prod ~ reorder(adub, prod),
       data = zim,
       type = c("p", "a"),
       ylab = expression("Produção de grãos de arroz" ~ kg ~ ha^{-1}),
       xlab = expression("Percentual aplicado de 80" ~
                           kg ~ ha^{-1} ~ "(Plantio/40 DAE/60 DAE)"))


# Fator de 4 níveis.
x <- gl(4, 1)

# Nome dos contrastes.
ctr <- paste0("contr.", c("treatment", "SAS", "sum", "helmert"))

# Obtendo a matriz de contrantes para cada tipo.
L <- lapply(ctr,
            FUN = function(type) {
              cbind(1, contrasts(C(x, type)))
            })
names(L) <- ctr
L

# Obtendo qual função da média cada parâmetro corresponde.
lapply(L, function(x) MASS::fractions(solve(x)))

# Ajuste do modelo.
m0 <- lm(prod ~ adub, data = zim)

# Quadro de análise de variância.
anova(m0)

summary(m0)

tr <- paste0("contr.", c("treatment", "SAS", "sum", "helmert"))

sapply(ctr,
       FUN = function(type) {
         m1 <- update(m0,
                      contrasts = list(adub = type))
         K <- cbind("(Intercept)" = 1,
                    contrasts(C(zim$adub, type)))
         K %*% coef(m1)
       })

par(mfrow = c(2, 2))
plot(m0); layout(1)