# tb <- structure(list(x = c(0.47, 1.88, 2.1, 2.24, 2.47, 2.59, 2.88, 
# 3.13, 3.21, 3.59, 3.63, 3.89, 6.04, 8.01, 8.13, 8.27, 8.27, 8.89, 
# 9.49, 11.06, 11.26, 11.38, 11.54, 12.12, 12.49), y = c(1.1, 2.39, 
# 2.92, 2.91, 3.14, 3.47, 3.52, 3.6, 3.9, 3.54, 4.33, 4.99, 5.16, 
# 4.41, 5.54, 4.7, 5.16, 4.94, 4.91, 4.73, 4.87, 4.19, 4.75, 5.24, 
# 4.97)), class = "data.frame", row.names = c(NA, -25L))


# tb <- structure(list(x = c(0.87, 1.53, 1.73, 1.85, 2, 2.38, 3.48, 3.94, 
# 4.1, 5.39, 5.57, 6.22, 6.69, 9.21, 9.25, 9.95, 10.04, 10.5, 11.03, 
# 11.25, 11.38, 12, 12.28, 12.57, 12.93), y = c(1.49, 2.23, 2.49, 
# 3.41, 2.35, 3.36, 5.06, 4.89, 5.52, 6.55, 7.31, 7.04, 6.3, 6.25, 
# 6.59, 6.52, 7.45, 6.31, 6.04, 5.89, 6.09, 6.65, 6.84, 6.95, 6.6
# )), class = "data.frame", row.names = c(NA, -25L))

tb <-structure(list(x = c(0.17, 0.88, 0.94, 0.98, 1.66, 1.69, 1.88, 
                           1.95, 2.28, 2.77, 3.77, 3.8, 4.45, 4.63, 4.94, 5.99, 6.89, 7.32, 
                           7.33, 7.37, 7.44, 8.56, 11.6, 12.91, 13.73), y = c(1.88, 2.11, 
                           2.31, 2.39, 3.05, 2.63, 3.26, 2.7, 3.74, 4.41, 5.39, 5.7, 6.8, 
                           6.92, 6.66, 7.69, 8.55, 8.31, 8.65, 8.26, 8.46, 8.66, 8.17, 9.17, 
                           9.28)), class = "data.frame", row.names = c(NA, -25L))

c <- 6

## Grupo
tb$g <- ifelse(tb$x < c, 0, 1)

library(ggplot2)
theme_set(theme_minimal())

# Modelo 1 ---------------------------------------
# Taxa de variação e intercepto diferentes
fit1 <- lm(y ~ x + g + I(g * x), data = tb)

# Modelo 2 ---------------------------------------
## Só muda o intercepto a taxa de variação continua a mesma
fit2 <- lm(y ~ x + g, data = tb)

# Modelo 3 ---------------------------------------
## Mesma origem
fit3 <- lm(y ~ x + I(x * g), data = tb)

# Modelo 4 ---------------------------------------
## Continuidade
fit4 <- lm(y ~ x + I((x - c) * g), data=tb)

# Modelo 5 ---------------------------------------
## Continuidade + Constante
fit5 <- lm(y ~ I((x - c) * (g - 1)), data = tb)

# Todos gráficos ---------------------------------------------------------------
tb2 <- data.frame(x = seq(0, 14, by = 0.05))
tb2$g <- ifelse(tb2$x < c, 0, 1)

tb2$pred1 <- predict(fit1, newdata = tb2)
tb2$pred2 <- predict(fit2, newdata = tb2)
tb2$pred3 <- predict(fit3, newdata = tb2)
tb2$pred4 <- predict(fit4, newdata = tb2)
tb2$pred5 <- predict(fit5, newdata = tb2)

tb2 <- tidyr::gather(tb2, pred, value, -x, -g)
write.csv(tb2, "ScoobyDoo.csv", row.names = T)


ggplot(tb2, aes(x = x, y = value, group = g)) +
    geom_point(data = tb, aes(x = x, y = y)) +
    geom_line() +
    facet_wrap(~pred)

options(digits = 4)

# 0.11
# ------------------------------------------------------------------------------

# a) FALSE
fit4

# b) FALSE
predict(fit2, newdata = data.frame(x = 6, g = 1))


# c) FALSE
fit1
predict(fit2, newdata = data.frame(x = 5, g = 1))

# d) TRUE

# e) TRUE
fit1
coef(fit1)[2] + coef(fit1)[4]

# f) FALSE

# g) TRUE
sum(residuals(fit4)^2)

# h) TRUE
predict(fit3, newdata = data.frame(x = 4, g = 0))

# i) FALSE

# j) DUVIDA

# k) FALSE

# l) FALSE
sum(residuals(fit1)^2)
sum(residuals(fit2)^2)
sum(residuals(fit3)^2)
sum(residuals(fit4)^2)
sum(residuals(fit5)^2)

# m) FALSE
sum(residuals(fit1)^2)

# n) TRUE
predict(fit2, newdata = data.frame(x = 5, g = 1)) -
    predict(fit2, newdata = data.frame(x = 5, g = 0))

# o) TRUE
fit4
predict(fit4, newdata = data.frame(x = 5, g = 1))


# p) FALSE

# 0.47
# ------------------------------------------------------------------------------

# a) FALSE
predict(fit3, data.frame(x = 1, g = 1))

# b) TRUE
fit4
predict(fit4, data.frame(x = 5, g = 1))

# c) FALSE
sum(residuals(fit1)^2)

# d) FALSE

# e) FALSE
fit1
predict(fit1, data.frame(x = 5, g = 1))

# f) TRUE
predict(fit2, newdata = data.frame(x = 5, g = 1)) -
    predict(fit2, newdata = data.frame(x = 5, g = 0))

# g) FALSE

# h) Dúvida

# i) FALSE
fit1

# j) FALSE
sum(residuals(fit1)^2)
sum(residuals(fit2)^2)
sum(residuals(fit3)^2)
sum(residuals(fit4)^2)
sum(residuals(fit5)^2)

# k) FALSE

# l) FALSE
fit4

# m) TRUE

# n) FALSE

# o) TRUE
sum(residuals(fit4)^2)

# p) TRUE
predict(fit3, newdata = data.frame(x = 4, g = 0))

# 0.87
# ------------------------------------------------------------------------------
# a) FALSE

# b) TRUE
round(sum(residuals(fit4)^2), 3)

# c) FALSE

# d) TRUE
predict(fit2, newdata = data.frame(x = 6, g = 0))

# e) FALSE (Duvida)

# f) FALSE
fit1

# g) FALSE
fit1

# h) TRUE
predict(fit2, newdata = data.frame(x = c, g = 1)) -
    predict(fit2, newdata = data.frame(x = c, g = 0))

# i) FALSE
sum(residuals(fit1)^2)

# j) FALSE

# k) FALSE
sum(residuals(fit1)^2)
sum(residuals(fit2)^2)
sum(residuals(fit3)^2)
sum(residuals(fit4)^2)
sum(residuals(fit5)^2)

# l) TRUE

# m) FALSE
fit4
coef(fit4)

# n) FALSE

# o) FALSE
predict(fit4, newdata = data.frame(x = 8, g = 1))

# p)
fit4

predict(fit4, newdata = data.frame(x = c, g = 1))
