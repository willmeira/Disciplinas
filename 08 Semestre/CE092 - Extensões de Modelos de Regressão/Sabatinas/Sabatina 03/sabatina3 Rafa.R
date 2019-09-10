rm(list=ls())
tb <- 
  structure(list(x = c(0.01, 0.91, 1.55, 2.22, 3.01, 3.39, 3.93,
                       3.99, 4.08, 4.97, 5.38, 6.22, 6.59, 6.94, 7.47, 8.29, 8.88, 10.41,
                       10.7, 10.87, 10.97, 12.17, 12.32, 12.46, 12.95),
                 y = c(1.72, 2.01, 3.03, 2.97, 4.44, 4.69, 6, 4.74, 5.14, 6.15, 5.28, 6.8,
                       6.48, 6.8, 6.71, 6.57, 6.47, 7.18, 7.01, 7.78, 6.59, 7.38, 7.22,
                       8.03, 7.49)), class = "data.frame", row.names = c(NA, -25L))
tb
plot(y ~x, data = tb)
# C = 5
nd <- data.frame(x = seq(min(tb$x), max(tb$x), by = 0.05))


nd <- data.frame(x = seq(0, 14, by = 0.05))
#nd$x[nd$x == 6.1] <- NA
#nd$x[nd$x == 6] <- NA
nd$x[122] <- NA


# FIT-1
tb$x1 <- ifelse(tb$x < 5, 0, tb$x)
nd$x1 <- ifelse(nd$x < 5, 0, nd$x)

fit.1 <- lm(y ~ x1 + I(x1>5)*x, data=tb)
nd$pred.1 <- predict(fit.1, new=nd)
with(nd, lines(pred.1 ~ x, col="black"))

# FIT-2
tb$x2 <- ifelse(tb$x < 5, 0, 1)
nd$x2 <- ifelse(nd$x < 5, 0, 1)

fit.2 <- lm(y ~ x2 + x, data=tb)
nd$pred.2 <- predict(fit.2, new=nd)
with(nd, lines(pred.2 ~ x, col="blue"))

# FIT-3
tb$x3 <- ifelse(tb$x < 5, 0, tb$x)
nd$x3 <- ifelse(nd$x < 5, 0, nd$x)

fit.3 <- lm(y ~ x3 + x, data=tb)
nd$pred.3 <- predict(fit.3, new=nd)
with(nd, lines(pred.3 ~ x, col="purple"))

# FIT-4
tb$x4 <- ifelse(tb$x < 5, 0, tb$x-5)
nd$x4 <- ifelse(nd$x < 5, 0, nd$x-5)

fit.4 <- lm(y ~ x4 + x, data=tb)
nd$pred.4 <- predict(fit.4, new=nd)
with(nd, lines(pred.4 ~ x, col="red"))

# FIT-5
tb$x5 <- ifelse(tb$x >= 5, 0, tb$x-5)
nd$x5 <- ifelse(nd$x >= 5, 0, nd$x-5)

fit.5 <- lm(y ~ x5, data=tb)
nd$pred.5 <- predict(fit.5, new=nd)
with(nd, lines(pred.5 ~ x, col="green"))

#


sqr1 <- sum(summary(fit.1)$residuals^2)
sqr2 <- sum(summary(fit.2)$residuals^2)
sqr3 <- sum(summary(fit.3)$residuals^2)
sqr4 <- sum(summary(fit.4)$residuals^2)
sqr5 <- sum(summary(fit.5)$residuals^2)

max(sqr1,sqr2,sqr3,sqr4,sqr5) == sqr2
