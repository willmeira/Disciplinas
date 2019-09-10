 data.frame(x = seq(0, 2, by = 0.1),
                  y1 = c(5.5, 7.2, 10.4, 7.4, 7.2, 9.1, 15.9, 12.7,
                         11.3, 14.4, 14.8, 17.1, 31.4, 17.5, 27.6, 19.6,
                         27.1, 21, 33.8, 30.2, 45.1),
                  y2 = c(0.8, 1.5, 2.6, 1.5, 1.4, 2.1, 3.9, 3.3, 3, 3.9,
                         4.2, 4.8, 6.9, 5.5, 7.3, 6.7, 8.2, 8.1, 10.2,
                         10.7, 12.8))
library(lattice)
library(latticeExtra)

xyplot(y1 + y2 ~ x, data = tb1, outer = TRUE, scales = "free")
x <-  seq(0, 2, by = 0.05)

# 1.
m1 <- lm(y1 ~ x, data = tb1)
logLik(m1)
plot(y1 ~ x, data = tb1)
grid1 <- data.frame(x = seq(0, 2, by = 0.05),
                    y1 = predict(m1, newdata = as.data.frame(x)))
lines(grid1, col = 2)

# 2.
m2 <- lm(y1 ~ x + I(x^2), data = tb1)
logLik(m2)
plot(y1 ~ x, data = tb1)
grid2 <- data.frame(x = seq(0, 2, by = 0.05),
                    y1 = predict(m2, newdata = as.data.frame(x)))
lines(grid2, col = 2)
lines(grid1, col = 1)
abline(v = c(0.5, 1.5))

# 3.
m3 <- lm(log(y1) ~ x, data = tb1)
logLik(m3) - sum(log(tb1$y1))
plot(y1 ~ x, data = tb1)
grid3 <- data.frame(x = seq(0, 2, by = 0.05),
                    y1 = exp(predict(m3, newdata = as.data.frame(x))))
lines(grid3, col = 2)

# 4.
m4 <- lm(sqrt(y1) ~ x, data = tb1)
logLik(m4) - sum(log((2*sqrt(tb1$y1))))
plot(y1 ~ x, data = tb1)
grid4 <- data.frame(x = seq(0, 2, by = 0.05),
                    y1 = (predict(m4, newdata = as.data.frame(x)))^2 )
lines(grid4, col = 2)

# 5.
MASS::boxcox(m1)
m5 <- lm(log(y1) ~ x, data = tb1)
logLik(m5) - sum(log(tb1$y1))
plot(y1 ~ x, data = tb1)
grid5 <- data.frame(x = seq(0, 2, by = 0.05),
                    y1 = exp(predict(m5, newdata = as.data.frame(x))))
lines(grid5, col = 2)

# 6.
m6 <- glm(y1 ~x, data = tb1, family = Gamma)
summary(m6)
logLik(m6)
plot(y1 ~ x, data = tb1)
grid6 <- data.frame(x = seq(0, 2, by = 0.05),
                    y1 = 1/predict(m6, newdata = as.data.frame(x)))
lines(grid6, col = 2)

# 7.
m7 <- glm(y1 ~x, data = tb1, family = inverse.gaussian)
summary(m7)
logLik(m7)
plot(y1 ~ x, data = tb1)
grid7 <- data.frame(x = seq(0, 2, by = 0.05),
                    y1 = predict(m7, newdata = as.data.frame(x))^(-0.5))
lines(grid7, col = 2)

# 8.
m8 <- glm(y1 ~ x, data = tb1, family = gaussian (link = "log"))
summary(m8)
logLik(m8)
plot(y1 ~ x, data = tb1)
grid8 <- data.frame(x = seq(0, 2, by = 0.05),
                    y1 = exp(predict(m8, newdata = as.data.frame(x))))
lines(grid8, col = 2)

# =====\=====\===== #
# a (V)
logLik(m3) - sum(log(tb1$y1)) > logLik(m1)

# b (F)
grid3[1,2] < grid4[1,2]

# c (F)
logLik(m8) < logLik(m1)

# d (F)
plot(y1 ~ x, data = tb1)
lines(glines(grid1, col = 1)
lines(grid6, col = "red")
lines(grid7, col = "blue")
abline(v = c(0.75, 1.5), lty = 2, col = "green")
)
AIC(m6) < AIC(m7)
BIC(m6) < BIC(m7)
logLik(m6) > logLik(m7)

# f (V)
plot(y1 ~ x, data = tb1)
lines(grid1, col = 1)
lines(grid2, col = "red")
abline(v = c(0.5, 1.5), lty = 2, col = "green")

# g (F)
logLik(m2) < logLik(m1)

# h (V)
plot(y1 ~ x, data = tb1)
lines(grid1, col = 1)
lines(grid2, col = "red")
lines(grid6, col = "blue")
lines(grid7, col = "green")
lines(grid8, col = "purple")

# i (V)
logLik(m4) + sum(log(1/(2*sqrt(tb1$y1)))) > logLik(m1)

# j (V)
logLik(m3) - sum(log(tb1$y1))

# k (V)
logLik(m3) - sum(log(tb1$y1)) > logLik(m4) + sum(log(1/(2*sqrt(tb1$y1))))
AIC(m3) < AIC(m4)
BIC(m3) < BIC(m4)

# l (F)
max(logLik(m1), logLik(m2), logLik(m6), logLik(m7), logLik(m8)) == logLik(m8)
20:00
