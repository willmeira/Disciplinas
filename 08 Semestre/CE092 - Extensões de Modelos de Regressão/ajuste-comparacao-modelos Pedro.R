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
       ylab = "y")



#--------------------------------------------
# Utilizando estes dados, efetue as análises das regressões de `y1 ~ x`
# e `y2 ~ x`, cada uma delas com os modelos de regressão linear simples
# inicialmente e depois com:
#
modelo1 <- lm(y1~x, data=tb1)
modelo2 <- lm(y2~x, data=tb1)
summary(modelo1)
summary(modelo2)

x11()
par(mfrow=c(2,2))
plot(modelo1) # Heterocedasticidade (ou, valor discrepante...)
x11()
par(mfrow=c(2,2))
plot(modelo2) # Comportamento aparentemente quadrático
par(mfrow=c(1,1))

#  * Transformação logarítmica da variável resposta.

modelo3 <- lm(log(y1)~x, data=tb1)
modelo4 <- lm(log(y2)~x, data=tb1)
summary(modelo3)
summary(modelo4)

x11()
par(mfrow=c(2,2))
plot(modelo3) # Deu uma melhorada na heterocedasticidade... porém, foge da normalidade
x11()
par(mfrow=c(2,2))
plot(modelo4) # Cagou mais ainda. Resíduos ficaram heterocedásticos
par(mfrow=c(1,1))


#  * Transformação raiz quadrada da variável resposta.

modelo5 <- lm(sqrt(y1)~x, data=tb1)
modelo6 <- lm(sqrt(y2)~x, data=tb1)
summary(modelo5)
summary(modelo6)

x11()
par(mfrow=c(2,2))
plot(modelo5) # Deu uma melhorada na heterocedasticidade... porém, foge da normalidade
x11()
par(mfrow=c(2,2))
plot(modelo6) # Cagou mais ainda. Resíduos ficaram heterocedásticos
par(mfrow=c(1,1))

#  * Transformação Box-Cox da variável resposta.

powerTransform <- function(y, lambda1, lambda2 = NULL, method = "boxcox") {
        
        boxcoxTrans <- function(x, lam1, lam2 = NULL) {
                
                # if we set lambda2 to zero, it becomes the one parameter transformation
                lam2 <- ifelse(is.null(lam2), 0, lam2)
                
                if (lam1 == 0L) {
                        log(y + lam2)
                } else {
                        (((y + lam2)^lam1) - 1) / lam1
                }
        }
        
        switch(method
               , boxcox = boxcoxTrans(y, lambda1, lambda2)
               , tukey = y^lambda1
        )
}



bc1 <- MASS::boxcox(tb1$y1~tb1$x) ; lambda1 <- bc1$x[which.max(bc1$y)]
bc2 <- MASS::boxcox(tb1$y2~tb1$x) ; lambda2 <- bc2$x[which.max(bc2$y)]

modelo7 <- lm(powerTransform(tb1$y1, lambda1)~tb1$x)
modelo8 <- lm(powerTransform(tb1$y2, lambda2)~tb1$x)
summary(modelo7)
summary(modelo8)
x11() ; par(mfrow=c(2,2))
plot(modelo7) 
x11() ; par(mfrow=c(2,2))
plot(modelo8) 
par(mfrow=c(1,1))


#  * Distribuição Gama para a resposta (GLM).
#  * Compare as verossimilhanças dos modelos ajustados lembrando de
#    torná-las comparáveis se necessário.

summary(modelo10)
AIC(modelo9,modelo10)

# Outra proposta: regressão segmentada. Tarefa de casa!

# Colocar curvas e bandas ajustadas



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
