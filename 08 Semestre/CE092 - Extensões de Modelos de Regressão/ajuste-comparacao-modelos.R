#-----------------------------------------------------------------------
# Extensões de modelos de regressão · CE 092
# leg.ufpr.br/ensino/EMR
#
#                                            Prof. Dr. Walmes M. Zeviani
#                                       Prof. Dr. Paulo Justiniano R. Jr
#
#                      Laboratory of Statistics and Geoinformation (LEG)
#                Department of Statistics · Federal University of Paraná
#                                       2019-Ago-15 · Curitiba/PR/Brazil
#-----------------------------------------------------------------------

#-----------------------------------------------------------------------
# Pacotes.

library(lattice)
library(hnp)
library(effects)
library(car)
library(ggplot2)
library(car)
library(statmod)
library(MASS0)
library(segmented)
#-----------------------------------------------------------------------
# Dataset 1 · Regressão.

u <- "x	y1	y2
0	5.5	0.8
0.1	7.2	1.5
0.2	10.4	2.6
0.3	7.4	1.5
0.4	7.2	1.4
0.5	9.1	2.1
0.6	15.9	3.9
0.7	12.7	3.3
0.8	11.3	3
0.9	14.4	3.9
1	14.8	4.2
1.1	17.1	4.8
1.2	31.4	6.9
1.3	17.5	5.5
1.4	27.6	7.3
1.5	19.6	6.7
1.6	27.1	8.2
1.7	21	8.1
1.8	33.8	10.2
1.9	30.2	10.7
2	45.1	12.8"

tb1 <- read.table(textConnection(u), sep = "\t", header = TRUE)
str(tb1)

xyplot(y1 + y2 ~ x,
       data = tb1,
       outer = TRUE,
       scales = "free",
       xlab = "x",
       ylab = "y",
       type = c("p","r")) # usar "smooth" no lugar do "r" pra suavizar a curva

#--------------------------------------------
# Utilizando estes dados, efetue as análises das regressões de `y1 ~ x`
# e `y2 ~ x`, cada uma delas com os modelos de regressão linear simples
# inicialmente e depois com:
#
#  * Transformação logarítmica da variável resposta.
#  * Transformação raiz quadrada da variável resposta.
#  * Transformação Box-Cox da variável resposta.
#  * Distribuição Gama para a resposta (GLM).
#  * Compare as verossimilhanças dos modelos ajustados lembrando de
#    torná-las comparáveis se necessário. ???



#----------------------------------------------------------
# ANÁLISE DESCRITIVA
#----------------------------------------------------------

#Histogramas
par(mfrow = c(2,2))
hist(tb1$y1)
hist(tb1$y2)
hist(tb1$x)

# Vamos aplicar uma transformação logaritmica nas variáveis apontadas como mais assimétricas na análise descritiva:
tb1$ly1 <- log(tb1$y1)
tb1$ly2  <- log(tb1$y2)
tb1$lx <- log(tb1$x)

par(mfrow = c(2,2))
hist(tb1$ly1)
hist(tb1$ly2)
hist(tb1$lx)

#-------------------------------------------------
# Modelos Lineares Y1 ~ X
#-------------------------------------------------

# segmentado

my1.24 <- segmented.lm(y1~x, seg.Z = ~ x)

# modelo 01
my1.01 <- lm(y1~x,data = tb1)
summary(my1.01)
par(mfrow = c(2,2))
plot(my1.01)
hnp(my1.01, xlab = 'Percentil da N(0,1)', ylab = 'Resíduos', 
    main = 'HNP - Y1~X')

# modelo 02
my1.02 <- lm(ly1~x, data = tb1)
summary(my1.02)
par(mfrow = c(2,2))
plot(my1.02)
hnp(my1.02, xlab = 'Percentil da N(0,1)', ylab = 'Resíduos', 
    main = 'HNP - LY1~X')

# modelo 03
my1.03 <- lm(sqrt(y1)~x, data = tb1)
summary(my1.03)
par(mfrow = c(2,2))
plot(my1.03)
hnp(my1.03, xlab = 'Percentil da N(0,1)', ylab = 'Resíduos', 
    main = 'HNP - Sqrt(Y1)~X')

# modelo 04 - Transformação Box-Cox da variável resposta.
boxcox(my1.01) # Transformação log
my1.04 <- lm(log(y2) ~ x, data = tb1)
summary(my1.04)
par(mfrow = c(2,2))
plot(my1.04)
hnp(my1.04, xlab = 'Percentil da N(0,1)', ylab = 'Resíduos', 
    main = 'HNP - log(Y1)~X')

# modelo 05 - Transformação Box-Cox da variável resposta.
boxcox(my1.02) # lambda = 0.5
my1.05 <- lm(((y1^0.5)-1)/0.5 ~ x, data = tb1)
summary(my1.05)
par(mfrow = c(2,2))
plot(my1.05)
hnp(my1.05, xlab = 'Percentil da N(0,1)', ylab = 'Resíduos', 
    main = 'HNP - Y1⁵~X')

shapiro.test(my1.01$residuals)
shapiro.test(my1.02$residuals)
shapiro.test(my1.03$residuals)
shapiro.test(my1.04$residuals)
shapiro.test(my1.05$residuals)

# Verificando a normalidade dos resíduos
par(mfrow = c(2,3))
hist(my1.01$residuals, probability = TRUE, main = 'Resíduos Ajuste - Y1~X',
     xlab = 'Resíduos', ylab = 'Frequência', col = "#FFFFCC") # histograma dos residuos
lines(density(my1.01$residuals), col = "#666600", lwd = 4)

hist(my1.02$residuals, probability = TRUE, main = 'Resíduos Ajuste - LY1~X',
     xlab = 'Resíduos', ylab = 'Frequência', col = "#FFFFCC") # histograma dos residuos
lines(density(my1.02$residuals), col = "#666600", lwd = 4)

hist(my1.03$residuals, probability = TRUE, main = 'Resíduos Ajuste - Sqrt(Y1)~X',
     xlab = 'Resíduos', ylab = 'Frequência', col = "#FFFFCC") # histograma dos residuos
lines(density(my1.03$residuals), col = "#666600", lwd = 4)

hist(my1.04$residuals, probability = TRUE, main = 'Resíduos Ajuste - Log(Y1)~X',
     xlab = 'Resíduos', ylab = 'Frequência', col = "#FFFFCC") # histograma dos residuos
lines(density(my1.04$residuals), col = "#666600", lwd = 4)

hist(my1.05$residuals, probability = TRUE, main = 'Resíduos Ajuste - Y1⁵~X',
     xlab = 'Resíduos', ylab = 'Frequência', col = "#FFFFCC") # histograma dos residuos
lines(density(my1.05$residuals), col = "#666600", lwd = 4)

#-------------------------------------------------
# Modelos Generalizados Y1 ~ X
#-------------------------------------------------
# gaussian family
#-------------------------------------------------

# modelo 06 - gaussian identity 
my1.06 <- glm(y1~x, data = tb1, family = gaussian(link = "identity"))
summary(my1.06)
par(mfrow = c(2,2))
plot(my1.06)
hnp(my1.06, xlab = 'Percentil da N(0,1)', ylab = 'Resíduos', 
    main = 'HNP - Gaussian Identity')
#Resíduos Quantílicos Aleatorizados
par(mfrow=c(1,2))
resy1.06 <- qresiduals(my1.06)
plot(resy1.06)
residuosy1.06 <- qresiduals(my1.06)
qqnorm(residuosy1.06)
qqline(residuosy1.06, col = 2)


# modelo 07 - gaussian log 
my1.07 <- glm(y1~x, data = tb1, family = gaussian(link = "log"))
summary(my1.07)
par(mfrow = c(2,2))
plot(my1.07)
hnp(my1.07, xlab = 'Percentil da N(0,1)', ylab = 'Resíduos', 
    main = 'HNP - Gaussian Log')
#Resíduos Quantílicos Aleatorizados
par(mfrow=c(1,2))
resy1.07 <- qresiduals(my1.07)
plot(resy1.07)
residuosy1.07 <- qresiduals(my1.07)
qqnorm(residuosy1.07)
qqline(residuosy1.07, col = 2)


# modelo 08 - gaussian inverse;
my1.08 <- glm(y1~x, data = tb1, family = gaussian(link = "inverse"))
summary(my1.08)
par(mfrow = c(2,2))
plot(my1.08)
hnp(my1.08, xlab = 'Percentil da N(0,1)', ylab = 'Resíduos', 
    main = 'HNP - Gaussian Inverse')
#Resíduos Quantílicos Aleatorizados
par(mfrow=c(1,2))
resy1.08 <- qresiduals(my1.08)
plot(resy1.08)
residuosy1.08 <- qresiduals(my1.08)
qqnorm(residuosy1.08)
qqline(residuosy1.08, col = 2)

#-------------------------------------------------
# Gamma family
#-------------------------------------------------

# modelo 09
my1.09 <- glm(y1~x, data = tb1, family = Gamma(link = "inverse"))
summary(my1.09)
par(mfrow = c(2,2))
plot(my1.09)
hnp(my1.09, xlab = 'Percentil da N(0,1)', ylab = 'Resíduos', 
    main = 'HNP - Gamma Inverse')
#Resíduos Quantílicos Aleatorizados
par(mfrow=c(1,2))
resy1.09 <- qresiduals(my1.09)
plot(resy1.09)
residuosy1.09 <- qresiduals(my1.09)
qqnorm(residuosy1.09)
qqline(residuosy1.09, col = 2)

# modelo 10
my1.10 <- glm(y1~x, data = tb1, family = Gamma(link = "identity"))
summary(my1.10)
par(mfrow = c(2,2))
plot(my1.10)
hnp(my1.10, xlab = 'Percentil da N(0,1)', ylab = 'Resíduos', 
    main = 'HNP - Gamma Identity')
#Resíduos Quantílicos Aleatorizados
par(mfrow=c(1,2))
resy1.10 <- qresiduals(my1.10)
plot(resy1.10)
residuosy1.10 <- qresiduals(my1.10)
qqnorm(residuosy1.10)
qqline(residuosy1.10, col = 2)

# modelo 11
my1.11 <- glm(y1~x, data = tb1, family = Gamma(link = "log"))
summary(my1.11)
par(mfrow = c(2,2))
plot(my1.11)
hnp(my1.11, xlab = 'Percentil da N(0,1)', ylab = 'Resíduos', 
    main = 'HNP - Gamma Log')
#Resíduos Quantílicos Aleatorizados
par(mfrow=c(1,2))
resy1.11 <- qresiduals(my1.11)
plot(resy1.11)
residuosy1.11 <- qresiduals(my1.11)
qqnorm(residuosy1.11)
qqline(residuosy1.11, col = 2)

#-------------------------------------------------
# Poisson family
#-------------------------------------------------

# modelo 12
my1.12 <- glm(y1~x, data = tb1, family = poisson(link = "log"))
summary(my1.12)
par(mfrow = c(2,2))
plot(my1.12)
hnp(my1.12, xlab = 'Percentil da N(0,1)', ylab = 'Resíduos', 
    main = 'HNP - Poisson Log')
#Resíduos Quantílicos Aleatorizados
par(mfrow=c(1,2))
resy1.12 <- qresiduals(my1.12)
plot(resy1.12)
residuosy1.12 <- qresiduals(my1.12)
qqnorm(residuosy1.12)
qqline(residuosy1.12, col = 2)

# modelo 13
my1.13 <- glm(y1~x, data = tb1, family = poisson(link = "identity"))
summary(my1.13)
par(mfrow = c(2,2))
plot(my1.13)
hnp(my1.13, xlab = 'Percentil da N(0,1)', ylab = 'Resíduos', 
    main = 'HNP - Poisson Identity')
#Resíduos Quantílicos Aleatorizados
par(mfrow=c(1,2))
resy1.13 <- qresiduals(my1.13)
plot(resy1.13)
residuosy1.13 <- qresiduals(my1.13)
qqnorm(residuosy1.13)
qqline(residuosy1.13, col = 2)

# modelo 14
my1.14 <- glm(y1~x, data = tb1, family = poisson(link = "sqrt"))
summary(my1.14)
par(mfrow = c(2,2))
plot(my1.14)
hnp(my1.14, xlab = 'Percentil da N(0,1)', ylab = 'Resíduos', 
    main = 'HNP - Poisson Sqrt')
#Resíduos Quantílicos Aleatorizados
par(mfrow=c(1,2))
resy1.14 <- qresiduals(my1.14)
plot(resy1.14)
residuosy1.14 <- qresiduals(my1.14)
qqnorm(residuosy1.14)
qqline(residuosy1.14, col = 2)


#-------------------------------------------------
# Inverse Gaussian family
#-------------------------------------------------

# modelo 15
my1.15 <- glm(y1~x, data = tb1, family = inverse.gaussian(link = "identity"))
summary(my1.15)
par(mfrow = c(2,2))
plot(my1.15)
hnp(my1.15, xlab = 'Percentil da N(0,1)', ylab = 'Resíduos', 
    main = 'HNP - Inv Gaus Ident')
#Resíduos Quantílicos Aleatorizados
par(mfrow=c(1,2))
resy1.15 <- qresiduals(my1.15)
plot(resy1.15)
residuosy1.15 <- qresiduals(my1.15)
qqnorm(residuosy1.15)
qqline(residuosy1.15, col = 2)

# modelo 16
my1.16 <- glm(y1~x, data = tb1, family = inverse.gaussian(link = "log"))
summary(my1.16)
par(mfrow = c(2,2))
plot(my1.16)
hnp(my1.16, xlab = 'Percentil da N(0,1)', ylab = 'Resíduos', 
    main = 'HNP - Inv Gaus Log')
#Resíduos Quantílicos Aleatorizados
par(mfrow=c(1,2))
resy1.16 <- qresiduals(my1.16)
plot(resy1.16)
residuosy1.16 <- qresiduals(my1.16)
qqnorm(residuosy1.16)
qqline(residuosy1.16, col = 2)


# modelo 17
my1.17 <- glm(y1~x, data = tb1, family = inverse.gaussian(link = "inverse"))
summary(my1.17)
par(mfrow = c(2,2))
plot(my1.17)
hnp(my1.17, xlab = 'Percentil da N(0,1)', ylab = 'Resíduos', 
    main = 'HNP - Inv Gaus Inv')
#Resíduos Quantílicos Aleatorizados
par(mfrow=c(1,2))
resy1.17 <- qresiduals(my1.17)
plot(resy1.17)
residuosy1.17 <- qresiduals(my1.17)
qqnorm(residuosy1.17)
qqline(residuosy1.17, col = 2)

# modelo 18
my1.18 <- glm(y1~x, data = tb1, family = inverse.gaussian(link = "1/mu^2"))
summary(my1.18)

par(mfrow = c(2,2))
plot(my1.18)
hnp(my1.18, xlab = 'Percentil da N(0,1)', ylab = 'Resíduos', 
    main = 'HNP - Inv Gaus 1/mu^2')
#Resíduos Quantílicos Aleatorizados
par(mfrow=c(1,2))
resy1.18 <- qresiduals(my1.18)
plot(resy1.18)
residuosy1.18 <- qresiduals(my1.18)
qqnorm(residuosy1.18)
qqline(residuosy1.18, col = 2)


#-------------------------------------------------
# Quasi family
#-------------------------------------------------

# modelo 19
my1.19 <- glm(y1~x, data = tb1, family = quasi(link = "identity"))
summary(my1.19)
par(mfrow = c(2,2))
plot(my1.19)
hnp(my1.19, xlab = 'Percentil da N(0,1)', ylab = 'Resíduos', 
    main = 'HNP - Quasi Ident')
#Resíduos Quantílicos Aleatorizados
par(mfrow=c(1,2))
resy1.19 <- qresiduals(my1.19)
plot(resy1.19)
residuosy1.19 <- qresiduals(my1.19)
qqnorm(residuosy1.19)
qqline(residuosy1.19, col = 2)


# modelo 20
my1.20 <- glm(y1~x, data = tb1, family = quasi(link = "inverse"))
summary(my1.20)
par(mfrow = c(2,2))
plot(my1.20)
hnp(my1.20, xlab = 'Percentil da N(0,1)', ylab = 'Resíduos', 
    main = 'HNP - Quasi Inv')
#Resíduos Quantílicos Aleatorizados
par(mfrow=c(1,2))
resy1.20 <- qresiduals(my1.20)
plot(resy1.20)
residuosy1.20 <- qresiduals(my1.20)
qqnorm(residuosy1.20)
qqline(residuosy1.20, col = 2)


# modelo 21
my1.21 <- glm(y1~x, data = tb1, family = quasi(link = "log"))
summary(my1.21)
par(mfrow = c(2,2))
plot(my1.21)
hnp(my1.21, xlab = 'Percentil da N(0,1)', ylab = 'Resíduos', 
    main = 'HNP - Quasi Log')
#Resíduos Quantílicos Aleatorizados
par(mfrow=c(1,2))
resy1.21 <- qresiduals(my1.21)
plot(resy1.21)
residuosy1.21 <- qresiduals(my1.21)
qqnorm(residuosy1.21)
qqline(residuosy1.21, col = 2)



# modelo 22
my1.22 <- glm(y1~x, data = tb1, family = quasi(link = "1/mu^2"))
summary(my1.22)
par(mfrow = c(2,2))
plot(my1.22)
hnp(my1.22, xlab = 'Percentil da N(0,1)', ylab = 'Resíduos', 
    main = 'HNP - Quasi 1/mu²')
#Resíduos Quantílicos Aleatorizados
par(mfrow=c(1,2))
resy1.22 <- qresiduals(my1.22)
plot(resy1.22)
residuosy1.22 <- qresiduals(my1.22)
qqnorm(residuosy1.22)
qqline(residuosy1.22, col = 2)



# modelo 23
my1.23 <- glm(y1~x, data = tb1, family = quasi(link = "sqrt"))
summary(my1.23)
par(mfrow = c(2,2))
plot(my1.23)
hnp(my1.23, xlab = 'Percentil da N(0,1)', ylab = 'Resíduos', 
    main = 'HNP - Quasi Sqrt')
#Resíduos Quantílicos Aleatorizados
par(mfrow=c(1,2))
resy1.23 <- qresiduals(my1.23)
plot(resy1.23)
residuosy1.23 <- qresiduals(my1.23)
qqnorm(residuosy1.23)
qqline(residuosy1.23, col = 2)



#----------------------------------------------
# Comparando os modelos
#----------------------------------------------

ajuste = c('my1.06', 'my1.07', 'my1.08', 'my1.09', 'my1.10', 'my1.11', 
           'my1.12', 'my1.13', 'my1.14', 'my1.15', 'my1.16', 'my1.17', 
           'my1.18', 'my1.19', 'my1.20', 'my1.21', 'my1.22', 'my1.23')

aic    = c(AIC(my1.06), AIC(my1.07), AIC(my1.08), AIC(my1.09), AIC(my1.10), AIC(my1.11), 
           AIC(my1.12), AIC(my1.13), AIC(my1.14), AIC(my1.15), AIC(my1.16), AIC(my1.17), 
           AIC(my1.18), AIC(my1.19), AIC(my1.20), AIC(my1.21), AIC(my1.22), AIC(my1.23))

vero   = c(logLik(my1.06), logLik(my1.08), logLik(my1.08), logLik(my1.09), logLik(my1.10), logLik(my1.11), 
           logLik(my1.12), logLik(my1.13), logLik(my1.14), logLik(my1.15), logLik(my1.16), logLik(my1.17), 
           logLik(my1.18), logLik(my1.19), logLik(my1.20), logLik(my1.21), logLik(my1.22), logLik(my1.23))

data.frame(ajuste, aic, vero)




selec <- data.frame(ajuste=c('logito', 'probito', 'cloglog', 'cauchy'),
                    aic=c(AIC(ajuste1), AIC(ajuste2), AIC(ajuste3), AIC(ajuste4)),
                    logLik=c(logLik(ajuste1),logLik(ajuste2),logLik(ajuste3),logLik(ajuste4)))






#-------------------------------------------------
#-------------------------------------------------
#-------------------------------------------------

#Medidas de influencia
inf <- influence.measures(my1.18) 
summary(inf)

compareCoefs(my1.01, my1.02, my1.03)

#-------------------------------------------------------

#razão de verossimilhança 
anova(ajuste2, ajuste2.1, test = 'Chisq') 
anova(m2, m2.1)
#----------------------------------------------

# Comparando modelos
#-------------------------------------------

#Predição
perfis <- data.frame(taxa = c(7.668333, 8.483667), licen = c(0.5703333, 0.6258036), renda = c(4241.833, 4815.457))
predict(ajuste3, newdata = perfis) 
#----------------------------------






# walmes
m0 <- lm(y1 ~ x + I(x^2), data = tb1)
plot(m0)
grid <- with(tb1,
             data.frame(x = seq(min(x),max(x),length.out = 31)))
grid$y1 <- predict(m0, newdata = grid)

x11()

plot(y1 ~ x, data = tb1)
lines(y1 ~ x, data = grid, col = "red")

#RLS OK
#RLQuad OK
#Glm's OK
#Log OK
#Raiz OK
#BCox OK
#
#
# Regressão segmentada

#-----------------------------------------------------------------------
# Dataset 2 · Análise de experimentos.

url <- "https://raw.githubusercontent.com/pet-estatistica/labestData/devel/data-raw/BanzattoQd3.2.1.txt"

tb2 <- read.table(url, header = TRUE, sep = "\t")
str(tb2)

cbind(xtabs(~trat, data = tb2))

# Média e desvio-padrão.
aggregate(pulgoes ~ trat,
          data = tb2,
          FUN = function(y) {
              c(mean = mean(y), sd = sd(y))
          })

# Diagrama de dispersão.
xyplot(pulgoes ~ reorder(trat, pulgoes),
       data = tb2,
       xlab = "Produtos para controle de pulgão",
       ylab = "Número de pulgões 36hs após pulverização",
       scales = list(x = list(rot = 90)))

#--------------------------------------------

# Utilizando estes dados, faça a análise do experimento para testar a
# hipótese de efeito de tratamentos e estudar as diferenças por
# contrastes. Conduza a análise considerando os modelos:
#
#  * Transformação Box-Cox da variável resposta.
#  * Distribuição (quasi) Poisson para a resposta.
#  * Em cada caso obtenha as médias dos tratamentos com o erro padrão e
#    intervalo de confiança.
#  * Em cada caso aplique constrastes par a par entre os tratamentos.

#-----------------------------------------------------------------------
