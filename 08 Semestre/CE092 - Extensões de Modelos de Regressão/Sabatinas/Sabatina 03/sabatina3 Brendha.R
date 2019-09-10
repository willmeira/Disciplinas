tb <-structure(list(x = c(0.47, 1.88, 2.1, 2.24, 2.47, 2.59, 2.88, 
                          3.13, 3.21, 3.59, 3.63, 3.89, 6.04, 8.01, 
                          8.13, 8.27, 8.27, 8.89, 9.49, 11.06, 11.26, 
                          11.38, 11.54, 12.12, 12.49), 
                    y = c(1.1, 2.39, 2.92, 2.91, 3.14, 3.47, 3.52, 3.6,
                          3.9, 3.54, 4.33, 4.99, 5.16, 4.41, 5.54, 4.7, 
                          5.16, 4.94, 4.91, 4.73, 4.87, 4.19, 4.75, 5.24,
                          4.97)), class = "data.frame", row.names = c(NA, -25L))
tb
plot(y ~x, data = tb)
# C = 5
nd <- data.frame(x = seq(min(tb$x), max(tb$x), by = 0.05))

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
predict(fit.3)

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


#---------------------
sum(residuals(fit.1)^2)
sum(residuals(fit.2)^2)
sum(residuals(fit.3)^2)
sum(residuals(fit.4)^2)
sum(residuals(fit.5)^2)



