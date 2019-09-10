tb <- 
  structure(list(x = c(0.01, 0.91, 1.55, 2.22, 3.01, 3.39, 3.93,
                       3.99, 4.08, 4.97, 5.38, 6.22, 6.59, 6.94, 7.47, 8.29, 8.88, 10.41,
                       10.7, 10.87, 10.97, 12.17, 12.32, 12.46, 12.95),
                 y = c(1.72, 2.01, 3.03, 2.97, 4.44, 4.69, 6, 4.74, 5.14, 6.15, 5.28, 6.8,
                       6.48, 6.8, 6.71, 6.57, 6.47, 7.18, 7.01, 7.78, 6.59, 7.38, 7.22,
                       8.03, 7.49)), class = "data.frame", row.names = c(NA, -25L))
tb
plot(y ~x, data = tb)
C = 6
nd <- data.frame(x = seq(0, 14, by = 0.05))
#nd$x[nd$x == 6.1] <- NA
nd$x[nd$x == 6] <- NA
#nd$x[122] <- NA

#################################################################
# FIT-1
tb$x1 <- ifelse(tb$x < C, 0, tb$x)
nd$x1 <- ifelse(nd$x < C, 0, nd$x)
fit.1 <- lm(y ~ x1 + ((x1<C)*x), data=tb)
nd$pred.1 <- predict(fit.1, new=nd)
with(nd, lines(pred.1 ~ x, col="black", lty = 2))

# FIT-2
tb$x2 <- ifelse(tb$x < C, 0, 1)
nd$x2 <- ifelse(nd$x < C, 0, 1)
fit.2 <- lm(y ~ x2 + x, data=tb)
nd$pred.2 <- predict(fit.2, new=nd)
with(nd, lines(pred.2 ~ x, col="blue"))

# FIT-3
tb$x3 <- ifelse(tb$x < C, 0, tb$x)
nd$x3 <- ifelse(nd$x < C, 0, nd$x)
fit.3 <- lm(y ~ x3 + x, data=tb)
nd$pred.3 <- predict(fit.3, new=nd)
with(nd, lines(pred.3 ~ x, col="purple"))

# FIT-4
tb$x4 <- ifelse(tb$x < C, 0, tb$x-C)
nd$x4 <- ifelse(nd$x < C, 0, nd$x-C)
fit.4 <- lm(y ~ x4 + x, data=tb)
nd$pred.4 <- predict(fit.4, new=nd)
with(nd, lines(pred.4 ~ x, col="red"))

# FIT-5
tb$x5 <- ifelse(tb$x >= C, 0, tb$x-C)
nd$x5 <- ifelse(nd$x >= C, 0, nd$x-C)
fit.5 <- lm(y ~ x5, data=tb)
nd$pred.5 <- predict(fit.5, new=nd)
with(nd, lines(pred.5 ~ x, col="green"))



logLik(fit.1)
logLik(fit.2)
logLik(fit.3)

summary(fit.1)
summary(fit.2)
summary(fit.3)
summary(fit.4)
summary(fit.5)


# a. Em termos de qualidade de ajuste expressa pela soma de quadrados de resíduo, o modelo que pior se ajustou aos dados foi o modelo 2.
#
sqr1 <- sum(summary(fit.1)$residuals^2)
sqr2 <- sum(summary(fit.2)$residuals^2)
sqr3 <- sum(summary(fit.3)$residuals^2)
sqr4 <- sum(summary(fit.4)$residuals^2)
sqr5 <- sum(summary(fit.5)$residuals^2)

max(sqr1,sqr2,sqr3,sqr4,sqr5) == sqr2

#FALSE


# b. A soma de quadrados residual do modelo 1 é 4.258
# 
sum(residuals(fit.1)^2)
#FALSE

# c. O modelo 4 é um modelo encaixado no modelo 1 porque pode ser obtido por meio de uma restrição paramétrica que expresse a continuidade da função.
#
coef(fit.4)
coef(fit.1)

# d. A equação para o segundo segmento linear do modelo 1 é: 0.48 − 1.089x
#
coef(fit.1)

# e. A diferença entre valor predito sob o ponto x=c pelo primeiro e segundo segmento do modelo 2 é y^_s2(x=c)−y^_s1(x=c)=2.441
#
coef(fit.2)
nd$pred.2
nd$pred.2[100]-nd$pred.2[99]
tb$x2 <- ifelse(tb$x < C, 0, 1)
nd$x2 <- ifelse(nd$x < C, 0, 1)
fit.2 <- lm(y ~ x2 + x, data=tb)
nd$pred.2 <- predict(fit.2, new=nd)
with(nd, lines(pred.2 ~ x, col="blue"))



# f. Os modelos 2, 3 e 4 são modelos encaixados no modelo 1 e obtidos por meio de restrições paramétricas no intercepto, na inclinação e na 
#    continuidade da função no ponto c, respectivamente.
coef(fit.1)
coef(fit.2)
coef(fit.3)
coef(fit.4)

# g. O modelo 5 contém 2 parâmetros na expressão para a média.
#
coef(fit.5)
#TRUE

# h. O modelo 5 é um modelo encaixado no modelo 2 porque pode ser obtido por meio de uma restrição paramétrica que expresse a continuidade da função.
#


# i. O valor predito no ponto x=5 pelo modelo 4 é 6.829
#
data.frame(nd$x,nd$pred.4)
#TRUE

# j. O modelo 4 é um modelo encaixado no modelo 5 porque pode ser obtido por uma restrição paramétrica sobre a inclinação do segundo segmento.
#


# k. A equação para o segundo segmento linear do modelo 4 é: 8.28−0.060x
#
coef(fit.4)

# l. As inclinações para o primeiro e segundo segmento do modelo 1 são, respectivamente, 1.002 e −0.088
#
coef(fit.1)

# m. O valor predito no ponto x=7 pelo modelo 5 é 7.893
#
data.frame(nd$x,nd$pred.5)
#FALSE


# n. O teste da razão de verossimilhanças entre o modelo 5 e 4, ao nível nominal de 5%, não rejeita a hipótese nula de que a inclinação do 
#    segundo segmento linear é 0

# O. Os interceptos para o primeiro e segundo segmento do modelo 4 são, respectivamente, 1.383 e 7.918
coef(fit.4)

# p. A soma de quadrados residual do modelo 4 é 3.959
#
sqr4
#TRUE








