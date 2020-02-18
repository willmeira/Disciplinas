sb <- 4
b1 <- -1.6978
sb1 <- 0.3304

exp(b1+1.96*(4.3284))/(1+exp(b1+1.96*(4.3284)))
exp(b1-1.96*(4.3284))/(1+exp(b1-1.96*(4.3284)))

qnorm(p = 0.99)



rm(list = ls())

#install.packages("foreign")
library(foreign)

dados <- read.dta("toenail.dta")
dados$trt <- as.factor(dados$trt)
summary(dados)
str(dados)

##----------------------------------------------------------------------------------------------------
## 12.1.1 - a)
##----------------------------------------------------------------------------------------------------

#install.packages("geepack")
library(geepack)
gee <- geeglm(y~month+month*trt-trt,
              family=binomial(link="logit"), data=dados, id=id,
              corstr ="exchangeable", std.err="san.se")
summary(gee)
1-exp(-0.1713)

##----------------------------------------------------------------------------------------------------
## 12.1.2 - b) Interpretaç??o do parâmetro ??2 do modelo (correspondente ao m??s) 
##----------------------------------------------------------------------------------------------------

coef(gee)[2]
1-exp(-0.1713)
1-exp(-0.1713*2)

# Estima-se que para a populaç??o de indivíduos que est??o sob o tratamento B a chance de a oconólise
# evoluir a um estado moderado ou severo diminui em 15.74% (1 ??? exp(??2)) após 1 m??s de tratamento. Já
# para a populaç??o sob mesma condiç??o, porém, após 2 meses de tratamento, a chance diminui em 29.01%
# (1 ??? exp(??2 ??? 2)).
# Já para a populaç??o que está submetida sob o tratamento A estima-se que a chance de a oconólise evoluir
# a um estado moderado ou severo diminui em 22.04% (1 ??? exp(??2 + ??3)) após 1 m??s de tratamento. Já
# para a populaç??o sob mesma condiç??o, porém, após 2 meses de tratamento, a chance diminui em 34.32%
# (1 ??? exp(??2 ??? 2 + ??3 * 2)).

1-exp(-0.1713-0.077)
1-exp((-0.1713*2) + (-0.077*2))


##----------------------------------------------------------------------------------------------------
## 12.1.3 - c) Interpretaç??o do parâmetro ??3 do modelo (correspondente ?? interaç??o tratamento.m??s)
##----------------------------------------------------------------------------------------------------

coef(gee)[3]

# Estima-se que para a populaç??o de indivíduos que est??o submetidos ao tratamento A tem-se uma chance de
# 7.48% (1 ??? exp(??3)) de ocorrer onicólise moderada ou severa menor do que o grupo que está submetido ao
# tratamento B.

1-exp(-0.077)

##----------------------------------------------------------------------------------------------------
## 12.1.4 - d) Gráfico de log(Odd) dos tratamentos em funç??o do m??s.
##----------------------------------------------------------------------------------------------------
# install.packages("lattice")
library(lattice)
tabela1 <- data.frame(grupo=rep(c("A","B"),each=19),
                      m??s=rep(c(0:18),times=2),
                      dimlogodd=c((coef(gee)[1]+coef(gee)[2]*0:18+coef(gee)[3]*0:18),
                                  (coef(gee)[1]+coef(gee)[2]*0:18)))

xyplot(dimlogodd ~ m??s,
       groups=grupo,
       type=c("p","l"),
       ylab=list("Diminuiç??o no log odds"),
       grid=T,
       main="Figura 1",
       auto.key=list(columns=2, cex.title=1,
                     title=expression('Grupos')),
       data=tabela1)

# Pela Figura 1 tem-se que há uma tend??ncia de diminuiç??o no logodds para ambos os grupos. Para o grupo
# A a diminuiç??o é mais acentuada do que para o grupo B. Assim, para o grupo A tem-se que o efeito do
# tratamento no tempo é maior do que para o grupo B.

##----------------------------------------------------------------------------------------------------
## 12.1.5 - e) Modelo misto com intercepto aleatório:
##----------------------------------------------------------------------------------------------------

# install.packages("lme4")
library(lme4)
#  logit(E(Yi ) = (??1 + b_i) + ??2 ??? month_ij + ??3 ??? treatment_i ??? month_ij

m1 <- glmer(y~(1|id)+month+month*trt-trt,
            family=binomial,
            nAGQ=20,
            data=dados)
summary(m1)


##----------------------------------------------------------------------------------------------------
## 12.1.6 - f) Estimativa de ????_b
##----------------------------------------------------------------------------------------------------

VarCorr(m1)

# Verifica-se que a variância do intercepto aleatório é igual a 16, um valor elevado. Assim, há uma grande
# variabilidade na propens??o de experimentar maior grau de infecç??o de unha do pé. Tem-se o seguinte intervalo
# de 95 % de confiança para a propens??o:
# 0-1
# Praticamente uma variaç??o de 0 a 100 % de propens??o.


##----------------------------------------------------------------------------------------------------
## 12.1.7 - g) O parâmetro ??2 é o seguinte:
##----------------------------------------------------------------------------------------------------

m1@beta[2]

# Estima-se que para um indivíduo do grupo que está sob o tratamento B a chance de a oconólise evoluir a um
# estado moderado ou severo diminui em 32.18% (1 ??? exp(?? 2 )) após 1 m??s de tratamento. Já após 2 meses de
# tratamento, a chance diminui em 54% (1 ??? exp(?? 2 ??? 2)).
# Já para um indivíduo que está submetida sob o tratamento A estima-se que a chance de a oconólise evoluir
# a um estado moderado ou severo diminui em NA% (1 ??? exp(?? 2 + ?? 3 )) após 1 m??s de tratamento. Já após
# 2 meses de tratamento, a chance diminui em 60.11% (1 ??? exp(?? 2 ??? 2 + ?? 3 )). Pode interpretar também em
# funç??o do log Odds, sendo que para um indivíduo pertencente ao grupo B estima-se que o log Odds diminui
# linearmente em -0.3883 após 1 m??s e para um indivíduo pertencente ao grupo A diminui linearmente em
# -0.5307 após 1 m??s.

##----------------------------------------------------------------------------------------------------
## 12.1.8 - h) O parâmetro ??3 é o seguinte:
##----------------------------------------------------------------------------------------------------

m1@beta[3]

# Estima-se que para um indivíduo que está submetidos ao tratamento A tem-se uma chance de 13.27
# (1 ??? exp(?? 3 )% menor de ocorrer onicólise moderada ou severa menor do que o grupo que um indivíduo que
# está submetido ao tratamento B, sendo que este indivíduo possui o mesmo risco de experimentar maior grau
# de infecç??o de unha do pé quando da aleatorizaç??o.


##----------------------------------------------------------------------------------------------------
## 12.1.9 - i)  Para os dois modelos as estimativas de ??3 s??o as seguintes:
##----------------------------------------------------------------------------------------------------

coef(gee)[3] # Modelo marginal

m1@beta[3]   # Modelo misto de efeito aleatório
  
# Conforme verificado a a chance de a oconólise evoluir a um estado moderado ou severo em relaç??o ao grupo
# de tratamento B é de 7.48% menor utilizando o modelo marginal, já utilizando um modelo misto de efeito
# aleatório é de 13.27% menor. Isto ocorreu pois há uma diferença na interpretaç??o do ?? 3 para os dois modelos.
# Para o modelo de efeitos mistos o parâmetro se refere ao efeito do tratamento na diminuiç??o da chance de
# evoluç??o da oconólise em um determinado indivíduo. Já no caso do modelo marginal refere-se ?? preval??ncia
# de indivíduos com oconólise moderada ou severa na populaç??o em relaç??o ao tratamento B contra o A.

##----------------------------------------------------------------------------------------------------
## 12.1.10 - j)  Variando os pontos de quadratura do modelo:
##----------------------------------------------------------------------------------------------------

# Número de pontos de quadratura = 2
m12 <- glmer(y~(1|id)+month+month*trt-trt,
             family=binomial,
             nAGQ=2,
             data=dados)
# Número de pontos de quadratura = 5
m15 <- glmer(y~(1|id)+month+month*trt-trt,
             family=binomial,
             nAGQ=5,
             data=dados)
# Número de pontos de quadratura = 10
  m110 <- glmer(y~(1|id)+month+month*trt-trt,
                family=binomial,
                nAGQ=10,
                data=dados)
# Número de pontos de quadratura = 20
m120 <- glmer(y~(1|id)+month+month*trt-trt,
              family=binomial,
              nAGQ=20,
              data=dados)
# Número de pontos de quadratura = 30
m130 <- glmer(y~(1|id)+month+month*trt-trt,
              family=binomial,
              nAGQ=30,
              data=dados)
# Número de pontos de quadratura = 50
m150 <- glmer(y~(1|id)+month+month*trt-trt,
              family=binomial,
              nAGQ=30,
              data=dados)
summary(m150)

# Estimativas dos parâmetros utilizando o número de pontos de quadratura descritos nas colunas.
t(data.frame("2 pontos"=summary(m12)$coefficients[,1:2],
             "5 pontos"=summary(m15)$coefficients[,1:2],
             "10 pontos"=summary(m110)$coefficients[,1:2],
             "20 pontos"=summary(m120)$coefficients[,1:2],
             "30 pontos"=summary(m130)$coefficients[,1:2],
             "50 pontos"=summary(m150)$coefficients[,1:2],
             check.names=FALSE))

# Verifica-se que para estimar bem os parâmetros necessita-se acima de 20 pontos de quadratura, tanto para a
# estimativas do valor pontual quanto para o erro padr??o dos parâmetros.

# Estimativa do erro padr??o do intercepto aleatório
data.frame("2 pontos"=3.2452,
           "5 pontos"=3.6903,
           "10 pontos"=4.0606,
           "20 pontos"=4.0017,
           "30 pontos"=4.0058,
           "50 pontos"=4.0058,
           check.names=FALSE)

# Para estimar bem o erro padr??o do intercepto aleatório é necessário uma quadratura acima de 20 pontos de
# quadratura. Conclui-se que os resultados dependem do número de pontos de quadratura.