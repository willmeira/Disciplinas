#o delta da função abaixo é o parametro teta
power.t.test(delta = 15, sd = 20, sig.level = 0.05,power = 0.8, alternative="one.sided")
power.t.test(delta = 15, sd = 20, sig.level = 0.05,power = 0.8, alternative="one.sided")
power.t.test(delta = 20, sd = 30, sig.level = 0.05,power = 0.8, alternative="one.sided")

power.t.test(delta = 15, sd = 20, sig.level = 0.05,n = 23, alternative="one.sided")

power.t.test(delta = 15, sd = 20, sig.level = 0.05,n = 32, alternative="one.sided")

library(ggplot2)
library(gridExtra)
library(meta)
install.packages("meta")