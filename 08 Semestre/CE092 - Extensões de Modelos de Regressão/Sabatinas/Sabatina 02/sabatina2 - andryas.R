tb1 <- data.frame(x = seq(0, 2, by = 0.1),
                  y1 = c(5.5, 7.2, 10.4, 7.4, 7.2, 9.1, 15.9, 12.7,
                         11.3, 14.4, 14.8, 17.1, 31.4, 17.5, 27.6, 19.6,
                         27.1, 21, 33.8, 30.2, 45.1),
                  y2 = c(0.8, 1.5, 2.6, 1.5, 1.4, 2.1, 3.9, 3.3, 3, 3.9,
                         4.2, 4.8, 6.9, 5.5, 7.3, 6.7, 8.2, 8.1, 10.2,
                         10.7, 12.8))

# str(tb1)
# 
# library(lattice)
# library(latticeExtra)
# 
 xyplot(y1 + y2 ~ x, data = tb1, outer = TRUE, scales = "free")


# Modelo 1 ---------------------------------------
m1 <- lm(y1 ~ x, data = tb1)
logLik(m1)

# Modelo 2 ---------------------------------------
m2 <- lm(y1 ~ x + I(x^2), data = tb1)
logLik(m2)

# Modelo 3 ---------------------------------------
m3 <- lm(log(y1) ~ x, data = tb1)
logLik(m3) - sum(log(tb1$y1))

# Modelo 4 ---------------------------------------
m4 <- lm(sqrt(y1) ~ x, data = tb1)
# logLik(m4)
logLik(m4) - 0.5 * sum(log(tb1$y1))

# Modelo 5 ---------------------------------------
l <- MASS::boxcox(tb1$y1 ~ tb1$x)
l <- l$x[which.max(l$y)]

m5 <- lm((y1^l - 1)/l ~ x, data = tb1)
logLik(m5) - (1/l - 1) * sum(log(tb1$y1^l))

# Modelo 6 ---------------------------------------
m6 <- glm(y1 ~ x, data = tb1, family = Gamma)
logLik(m6)

# Modelo 7 ---------------------------------------
m7 <- glm(y1 ~ x, data = tb1, family = inverse.gaussian)
logLik(m7)

# Modelo 8 ---------------------------------------
m8 <- glm(y1 ~ x, data = tb1, family = gaussian(link=log))
logLik(m8)


# ------------------------------------------------------------------------------

# a.
logLik(m3) - sum(log(tb1$y1)) > logLik(m1)

# b.
predict(m3, newdata = data.frame(x = 0)) < predict(m4, newdata = data.frame(x = 0))

# c.
logLik(m8) < logLik(m1)

# d.
ym7 <- predict(m7, newdata = data.frame(x = seq(0.75, 1.5, 0.05)), type = "response")
ym1 <- predict(m1, newdata = data.frame(x = seq(0.75, 1.5, 0.05)))
ym6 <- predict(m6, newdata = data.frame(x = seq(0.75, 1.5, 0.05)), type = "response")

sum(ym7 - ym1)^2 < sum(ym7 - ym6)^2

# e.
AIC(m6) < AIC(m7)

# f.
all(predict(m1, newdata = data.frame(x = seq(0.5, 1.5, 0.05))) > predict(m2, newdata = data.frame(x = seq(0.5, 1.5, 0.05))))

# g.
logLik(m2) < logLik(m1)

# ?h.

# i.
logLik(m4) - 0.5 * sum(log(tb1$y1)) > logLik(m1)

# j.
logLik(m3) - sum(log(tb1$y1)) # TRUE

# k.
summary(m3)
summary(m4)
(sum(((log(tb1$y1) - mean(log(tb1$y1)))/sd(log(tb1$y1)) -
    (predict(m3) - mean(log(tb1$y1)))/sd(log(tb1$y1)))^2)) < 
(sum(((sqrt(tb1$y1) - mean(sqrt(tb1$y1)))/sd(sqrt(tb1$y1)) -
    (predict(m4) - mean(sqrt(tb1$y1)))/sd(sqrt(tb1$y1)))^2))

# i.
all(
    c(
        logLik(m8) > logLik(m1), 
        logLik(m8) > logLik(m2), 
        logLik(m8) > logLik(m6), 
        logLik(m8) > logLik(m7)
    )
)
