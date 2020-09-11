################################################################################
################################################################################
################################################################################
### Distribuições discretas

library(gamlss)
library(gamlss.demo)
options(device = 'x11')

################################################################################
### Sessão 1 - Distribuições discretas implementadas no pacote gamlss

gamlss.demo()
### Experimente algumas distribuições, verifique quantos são os parâmetros, o que 
### eles controlam...

### Vamos fazer uma simulação de dados com superdispersão e excesso de zeros.
### Para isso, vamos gerar valores aleatórios de uma distribuição Poisson-Inv.
### Gaussiana com inflação de zeros (ZIPIG). A ZIPIG, assim como a binomial
### negativa (ZINBI), é gerada por uma mistura da distribuição de Poisson, em que a 
### distribuição inversa gaussiana é usada no lugar da distribuição gama, como
### no caso da ZINBI.

### Simulação de uma amostra aleatória de tamanho n = 500 de uma ZIPIG.
set.seed(123456)
dados <- rZIPIG(n = 500, mu = 3, sigma = 5, nu = 0.2)
summary(dados)

### Distribuição de frequências para os dados simulados.
y <- 0:max(dados)
dados_tab <- c(table(factor(dados, levels = y)))
plot(y, dados_tab, type = 'h', lwd = 5, xlab = 'y', las = 1, ylab = 'Frequência')

### Descritivas
mean(dados)
var(dados)
### Observe que a variância amostral é bastante superior à média, e o gráfico de 
### frequências fornece indicativos de excesso de zeros.

### Vamos ajustar os modelos Poisson, binomial negativa e PIG, além das respectivas
### versões inflacionadas de zero.

par(mfrow = c(1,2), cex = 1.15, las = 1)

### Poisson
PO_fit <- gamlss(dados~1, family = PO); PO_fit
histDist(dados, family = PO, main = 'PO', xlab = 'y', ylab = 'P(Y=y)', ylim = c(0,0.55))
wp(PO_fit, ylim.all = 6)


### Poisson inflacionada de zeros
ZIP_fit <- gamlss(dados~1, family = ZIP); ZIP_fit
histDist(dados, family = ZIP, main = 'ZIP', xlab = 'y', ylab = 'P(Y=y)', ylim = c(0,0.55))
wp(ZIP_fit, ylim.all = 6)

### Binomial negativa
NBI_fit <- gamlss(dados~1, family = NBI); NBI_fit
histDist(dados, family = NBI, main = 'NBI', xlab = 'y', ylab = 'P(Y=y)', ylim = c(0,0.6))
wp(NBI_fit, ylim.all = 2)

### Binomial negativa inflacionada de zeros
ZINBI_fit <- gamlss(dados~1, family = ZINBI); ZINBI_fit
histDist(dados, family = ZINBI, main = 'ZINBI', xlab = 'y', ylab = 'P(Y=y)', ylim = c(0,0.55))
wp(ZINBI_fit, ylim.all = 1.5)

### PIG
PIG_fit <- gamlss(dados~1, family = PIG); PIG_fit 
histDist(dados, family = PIG, main = 'PIG', xlab = 'y', ylab = 'P(Y=y)', ylim = c(0,0.55))
wp(PIG_fit)

### PIG inflacionada de zeros
ZIPIG_fit <- gamlss(dados~1, family = ZIPIG); ZIPIG_fit
histDist(dados, family = ZIPIG, main = 'ZIPIG', xlab = 'y', ylab = 'P(Y=y)', ylim = c(0,0.55))
wp(ZIPIG_fit, ylim.all = 1)

### Agora, vamos comparar os ajustes usando os AICs 
GAIC(PO_fit, ZIP_fit, NBI_fit, ZINBI_fit, PIG_fit, ZIPIG_fit, k = 2)

### Conforme esperado, o modelo ZIPIG proporciona melhor ajuste aos dados. Seu ajuste supera
### o produzido pela PIG sem parâmetro de inflação, e as demais distribuições inflacionadas.
### A Poisson e sua versão inflacionada claramente produzem ajustes insatisfatórios,
### e inferiores aos demais, indicando que tais modelos são bastante limtados para
### acomodar a superdispersão (e o excesso de zeros, no caso da Poisson) presentes nos
### dados.

################################################################################
### Sessão 2 - Modelos para contagens resultantes de misturas de distribuições

### Nesta segunda aplicação, vamos verificar através de simulação que a distribuição
### binomial negativa pode ser derivada de uma mistura contínua da Poisson em que
### o componente da mistura tem distribuição gamma. Os parâmetros da NBI serão
### fixados mu = 2 e sigma = 2.25.

set.seed(12345)

### Primeiramente, vamos simular 10000 valores aleatórios de g~gama(1,2.25^(1/2)).
g <- rGA(10000, mu = 1, sigma = 1.5)

### Agora, condicional aos valores simulados para g, vamos gerar 10000 valores 
### aleatórios de Y|g ~ Poisson(mu=2*g)
y <- rPO(10000, mu = 2*g)

### Para verificar que os valores simulados correspondem a realizações aleatórias
### vamos simular 10000 valores de uma NBI com os parâmetros correspondentes aos
### da mistura, isto é, mu = 2 e sigma = 2.25.
y_NB <- rNBI(10000, mu = 2, sigma = 2.25)

### Finalmente, vamos analisar a aderência das duas distribuições empíricas à 
### distribuição binomial negativa usando gráficos apropriados da biblioteca 
### gamlss.

### Começando pelos dados simulados diretamente da função NBI.
par(mfrow = c(1,2))
NBI_fit1 <- gamlss(y_NB~1, family = NBI)
wp(NBI_fit1, ylim.all = 2)
histDist(y_NB, family = NBI, xlab = 'y', ylab = 'P(Y=y)', ylim = c(0,0.6), main = '')

### Agora, usando os dados simulados em duas etapas, de acordo com a mistura Poisson-gama.
par(mfrow = c(1,2))
NBI_fit2 <- gamlss(y~1, family = NBI)
wp(NBI_fit2, ylim.all = 2)
histDist(y, family = NBI, xlab = 'y', ylab = 'P(Y=y)', ylim = c(0,0.6), main = '')

### Finalmente, um gráfico quantil-quantil com os dados simulados das duas formas
### para avaliar a compatibilidade das distribuições empíricas produzidas.

qqplot(y, y_NB)
abline(a = 0, b = 1)

### Pode-se observar, em ambos os casos, forte aderência à distribuição binomial
### negativa, e que os quantis das duas distribuições amostrais produzidas são
### bastante compatíveis.

### Segunda aplicação
### Agora, vamos simular valores de uma distribuição Poisson com inflação de zeros.
### Vamos fixar os parâmetros mu = 5 e sigma = 0.3.

set.seed(10)

### Primeiramente, vamos simular 10000 valores de uma binomial(1,0.3)
g <- rBI(10000, mu = 0.3)

### Agora, condicional aos valores simulados para g, vamos gerar 10000 valores 
### aleatórios de Y|g ~ Poisson(mu=2*g)
y <- rep(0, 10000)
y[g>0] <- rPO(sum(g>0), mu = 5*g[g>0])
### Simulamos valores da Poisson(5) apenas se g for diferente de zero. Caso contrário,
### y=0.

### Vamos conferir que os dados simulados de fato tem distribuição ZIP.
par(mfrow = c(1,2))
ZIP_fit1 <- gamlss(y~1, family = ZIP)
wp(ZIP_fit1, ylim.all = 2)
histDist(y, family = ZIP, xlab = 'y', ylab = 'P(Y=y)', ylim = c(0,0.7), main = '')

### Se quisermos simular direto da distribuição ZIP, basta usar rZIP:
y_ZIP <- rZIP(10000, mu = 5, sigma = 0.3)

################################################################################
### Sessão 3 - Modelos para contagens gerados por discretização de distribuições contínuas

data(cysts)
help(cysts)
require(gamlss.cens)
### y: número de cistos
### f: frequência

### Observe que os dados já estão sumarizados na forma de uma tabela de frequências.
### Podemos ajustar os modelos para os dados nesse formato de maneira equivalente
### ao ajuste para os dados brutos, declarando como resposta os valores individuais
### observados na amostra, e atribuindo como pesos (weights) as correspondentes frequências.

cysts$y[1] <- 0.000001 ### Para evitar problemas com y=0 na avaliação das densidades.

### Vamos criar uma versão discretizada da distribuição Weibull (WEI)
### O primeiro passo é a criação de uma nova família, resultante da gama sob 
### censura intervalar.

gen.cens("WEI", type = "interval")

### Vamos fazer o mesmo para algumas outras distribuições contínuas.

gen.cens("GA", type = "interval") ### Gama
gen.cens("IG", type = "interval") ### Inversa gaussiana
gen.cens("GG", type = "interval") ### Gama generalizada
gen.cens("GIG", type = "interval") ### Inversa gaussiana generalizada
gen.cens("GB2", type = "interval") ### Beta generalizada tipo 2

### Para ajustar a distribuição Weibull discretizada, o procedimento é semelhante
### ao usado em análise de sobrevivência com dados apresentando censura intervalar.

mod_WEI <- gamlss(Surv(y, y+1, type = "interval2") ~ 1, weights = f,
                  data = cysts, family = WEIic)

### Vale o mesmo para as demais distribuições.

mod_GA <- gamlss(Surv(y, y+1, type = "interval2") ~ 1, weights = f,
                  data = cysts, family = GAic)
mod_IG <- gamlss(Surv(y, y+1, type = "interval2") ~ 1, weights = f,
                 data = cysts, family = IGic)
mod_GG <- gamlss(Surv(y, y+1, type = "interval2") ~ 1, weights = f,
                 data = cysts, family = GGic)
mod_GIG <- gamlss(Surv(y, y+1, type = "interval2") ~ 1, weights = f,
                 data = cysts, family = GIGic)

GAIC(mod_WEI, mod_GA, mod_IG, mod_GG, mod_GIG, k = 2)

### A versão discretizada da distribuição gama produziu melhor ajuste (menor AIC)
### que as demais.

### Vamos comparar as frequências observadas e as ajustadas pelo modelo. Para isso,
### primeiro precisamos calcular as frequências esperadas segundo o modelo

coef(mod_GA, what = c('mu'))
coef(mod_GA, what = c('sigma'))
### Estimativas (na escala log)

n <- sum(cysts$f)
expected <- rep(0,12)
for(i in 1:12)
  expected[i] <- 100 * dGAic(Surv(i-1,i, type = 'interval2'), exp(0.5561), exp(0.4298))

cysts$expected <- round(expected,2)
cysts

### Podemos observar falta de ajuste, particularmente quanto à frequência de zeros,
### bastante superior à estimada pelo modelo. Uma alternativa zero inflacionada
### (ou zermo modificada) deve ajustar melhor os dados.

################################################################################
### Sessão 4 - Modelos Modelos inflacionados de zeros.

### Retomemos os dados de cistos em ratos.

cysts$y[1] <- 0 ### Convertendo novamente as frequências nulas para zero.

mod_ZIP <- gamlss(y ~ 1, family = ZIP, weights = f, data = cysts) ### ZI-Poisson
mod_ZINBI <- gamlss(y ~ 1, family = ZINBI, weights = f, method = mixed(20,20), data = cysts) ### ZI-binomial negativa
mod_PIG <- gamlss(y ~ 1, family = ZIPIG, weights = f, data = cysts) ### ZI-Poisson inversa gaussiana
mod_ZIBNB <- gamlss(y ~ 1, family = ZIBNB, method = mixed(20,20), weights = f, data = cysts) ### ZI-BNB
mod_ZISICHEL <- gamlss(y ~ 1, family = ZINBF, weights = f, method = mixed(20,20), data = cysts) ### ZI-BNB

GAIC(mod_ZIP, mod_ZINBI, mod_PIG, mod_ZIBNB, mod_ZISICHEL, k = 2)

### O melhor ajuste (menor AIC) foi produzido pela distribuição ZI binomial negativa.
mod_ZINBI

### Diagnóstico do ajuste.
par(mfrow = c(1,2))
wp(mod_ZINBI, ylim.all = 1.5)
histDist(y, freq = f, data = cysts, family = ZINBI, main = 'ZINBI', xlab = 'y', 
         ylab = 'P(Y=y)', ylim = c(0,0.65))
### Evidentemente o modelo se ajusta bem aos dados.

### A título de ilustração, usando o pacote gamlss.countKinf vamos implementar 
### uma versão zer-inflacionada da distribuição Poisson generalizada.

gen.Kinf(family = GEOM, kinf = 0)
mod_ZIGEOM <- gamlss(y ~ 1, family = inf0GEOM, method = mixed(20,20), weights = f, data = cysts)

AIC(mod_ZIGEOM, mod_ZINBI)
### A distribuição geométrica zero-inflacionada tem um parâmetro a menos, e produziu
### mentor AIC que a ZINBI.

### Diagnóstico do ajuste.
par(mfrow = c(1,2))
wp(mod_ZIGEOM, ylim.all = 1.5)
histDist(y, freq = f, data = cysts, family = inf0GEOM, main = 'ZIGEOM', xlab = 'y', 
         ylab = 'P(Y=y)', ylim = c(0,0.65))
### O modelo produzido ajusta os dados de maneira bastante satisfatoria.

### Finalmente, também a título de ilustração vamos gerar algumas distribuições
### K-inflacionadas baseadas na distribuição PIG.

### Distribuição PIG inflacionada em 1
gen.Kinf(family = PIG, kinf = 1)
y <- rinf1PIG(1000, mu = 1, sigma = 1, nu = 0.3)
histDist(y, family = inf1PIG, main = 'inf1PIG')

### Agora, vamos gerar uma distribuição bimodal (neste caso, inflacionada em 3)
gen.Kinf(family = PIG, kinf = 3)
y <- rinf3PIG(1000, mu = 1, sigma = 1, nu = 0.3)
histDist(y, family = inf3PIG, main = 'inf3PIG')


################################################################################
### Sessão 5 - Modelos zero-ajustados

### Dados sobre o número de visitas ao pediatra no último mês de crianças em certa
### faixa etária.

consultas <- data.frame('N_consultas' = c(0,1,2,3,4,5,6,7,8,9,10,11,12,15,16,24,48),
                      'Freq' = c(241,96,55,30,14,8,12,7,5,8,3,1,1,1,1,1,1))
consultas

### Vamos ajustar diferentes modelos zero-ajustados e zero-inflacionados. Começamos
### pelos zero inflacionados.
kids_ZIP <- gamlss(N_consultas ~ 1, family = ZIP, weights = Freq, data = consultas) ### ZI-Poisson
kids_ZINBI <- gamlss(N_consultas ~ 1, family = ZINBI, weights = Freq, data = consultas) ### ZI-binomial negativa
kids_ZIPIG <- gamlss(N_consultas ~ 1, family = ZIPIG, weights = Freq, data = consultas) ### ZI-Poisson inversa gaussiana
kids_ZIBNB <- gamlss(N_consultas ~ 1, family = ZIBNB, method = mixed(20,50), weights = Freq, data = consultas) ### ZI-BNB
kids_ZINBF <- gamlss(N_consultas ~ 1, family = ZINBF, weights = Freq, data = consultas) ### ZI-BNB
kids_ZISICHEL <- gamlss(N_consultas ~ 1, family = ZISICHEL, method = mixed(20,20), weights = Freq, data = consultas) ### ZI-BNB

### Agora, os modelos zero-ajustados.
kids_ZAP <- gamlss(N_consultas ~ 1, family = ZAP, weights = Freq, data = consultas) ### ZI-Poisson
kids_ZANBI <- gamlss(N_consultas ~ 1, family = ZANBI, weights = Freq, method = mixed(20,20), data = consultas) ### ZI-binomial negativa
kids_ZAPIG <- gamlss(N_consultas ~ 1, family = ZAPIG, weights = Freq, data = consultas) ### ZI-Poisson inversa gaussiana
kids_ZASICHEL <- gamlss(N_consultas ~ 1, family = ZASICHEL, weights = Freq, data = consultas) ### ZI-BNB

### Vamos comparar os ajustes dos modelos via AIC.
GAIC(kids_ZINBI, kids_ZIPIG, kids_ZIBNB, kids_ZINBF, kids_ZISICHEL,
     kids_ZAP, kids_ZANBI, kids_ZAPIG, kids_ZASICHEL)

### As duas versões zero-modificadas da distribuição PIG forneceram os melhores ajustes.
### Os valores de AIC são praticamente iguais para as duas.

par(mfrow = c(1,2))
wp(kids_ZAPIG, ylim.all = 1)
histDist(N_consultas, freq = Freq, data = consultas, family = ZAPIG, main = 'ZAPIG', xlab = 'y', 
         ylab = 'P(Y=y)', ylim = c(0,0.55))
### O modelo ZAPIG ajusta os dados de maneira bastante satisfatoria.

### Para finalizar, vamos ajustar "no atacado" todos os modelos para dados discretos
### implementados, incluindo os não modificados em zero.

consulta_bruto <- rep(consultas$N_consultas, times = consultas$Freq)
### Para usar a função fitDist, devemos entrar com os dados brutos (e não com as frequências)

f1 <- fitDist(consulta_bruto, type = "counts")
f1$fits

### A distribuição de Waring produz menor valor de AIC. As duas versões zero-modificadas
### da PIG ficaram na quarta posição, e a PIG original logo atrás.

################################################################################
### Sessão 6 - Demanda por assistência médica

### Dados de um estudo transversal conduzido entre  1987 e 1988. O objetivo é 
### ajustar um modelo de regressão para o número de consultas médicas em função
### de covariáveis.

require(AER)
data(NMES1988)
help(NMES1988)

nmes <- NMES1988[,c('visits', 'hospital', 'health', 'chronic', 'gender', 'school', 'insurance')]
### Consulte na documentação da base a que se refere cada uma das variáveis.

dim(nmes) ### Temos dados de 4406 pacientes.
head(nmes, 10) ### Dez primeiras linhas da base.
summary(nmes)

hist(nmes$visits, breaks = seq(-0.5,90.5,1), main = '', xlab = 'Número de consultas', 
     ylab = 'Frequência', las = 1)
### Distribuição fortemente assimétrica, com moda em zero.

prop.table(table(nmes$visits))
### 15.5% dos indivíduos não realizaram sequer uma consulta. As frequências, em geral,
### diminuem para números maiores de consultas.

### Análise bivariada.
par(mfrow = c(2,3), mar = c(4,4,2,2), las = 1, cex.axis = 1.3, cex.lab = 1.3)
plot(visits ~ hospital, data = nmes, col = 'grey60', xaxt = 'n')
axis(1, 0:8)
plot(visits ~ health, data = nmes, col = 'grey60')
plot(visits ~ chronic, data = nmes, col = 'grey60', xaxt = 'n')
axis(1, 0:8)
plot(visits ~ gender, data = nmes, col = 'grey60')
plot(visits ~ school, data = nmes, col = 'grey60')
plot(visits ~ insurance, data = nmes, col = 'grey60')
### Aparentemente a frequência de consultas médicas diminui quanto maior o número
### de internações e quanto melhor o estado de saúde do indivíduo, por exemplo.

### Antes de proceder com o ajuste de modelos de regressão, vamos analisar a distribuição
### marginal da variável resposta, e investigar qual distribuição proporciona 
### melhor ajuste. 

# f_visits <- fitDist(nmes$visits, type = 'count', k = 2) ### Demora
# f_visits
# f_visits$fits
### As versões zero-modificadas da distribuição BNB produziram, marginalmente, melhor
### ajuste. As distribuições zero-modificadas baseadas na PIG aparecem na sequência.

### Vamos começar pelo ajuste de um modelo com resposta Poisson
ajuste_PO <- gamlss(visits~., family = PO, data = nmes)
plot(ajuste_PO) ### Claramente mal ajustado.

### Agora, vamos voltar a comparar os modelos discretos implementados na biblioteca
### gamlss, mas considerando o ajuste por covariáveis do parâmetro de locação.

# c_visits <- chooseDist(ajuste_PO, type = 'count', k = 2) ### Demora
# c_visits
### Também quando ajustada por covariáveis a distribuição ZIBNB produziu menor 
### AIC. Vamos dar sequência com ela.

ajuste_ZIBNB <- gamlss(visits~., family = ZIBNB, data = nmes, method = mixed(20,100))
plot(ajuste_ZIBNB) 
wp(ajuste_ZIBNB)
### Embora o modelo ZIBNB claramente ajuste melhor os dados que a Poisson, ainda há
### bastante espaço para refinamentos, como fazer seleção de covariáveis, incluir
### covariáveis também nos preditores dos outros parâmetros, inserir termos suaves...
### Antes de propor um modelo alternativo, vamos dar uma olhada na documentação
### da distribuição ZIBNB.

help(ZIBNB)
### A distribuição ZIBNB tem quatro parâmetros: um de locação (mu, a média da BNB), um de escala (sigma),
### outro de forma (nu) e, finalmente, o parâmetro referente à inflação de zeros (tau).

### Mais adiante vamos estudar algoritos para seleção de covariáveis em GAMLSS que
### poderão ser bastante úteis nesta aplicação. Por ora, vamos ficar 

### Como refinamentos, podemos inserir suavizadores para algumas covariáveis numéricas...
ajuste_ZIBNB2 <- gamlss(visits~ chronic + hospital + insurance + gender + school + health,
                       sigma.formula=~ health + chronic + gender,
                       nu.formula =~ hospital,
                       tau.formula =~ chronic + insurance, 
                       family = ZIBNB, data = nmes, method = mixed(20,100))

AIC(ajuste_ZIBNB, ajuste_ZIBNB2, k = 2)
### O segundo modelo, com covariáveis incluídas também nos preditores de sigma,
### nu e tau, ajusta melhor os dados (menor AIC).

ajuste_ZIBNB2 
### Estimativas dos parâmetros do modelo ZIBNB ajustado aos dados de visitas ao médico.

summary(ajuste_ZIBNB2)

### Para interpretar os parâmetros, devemos levar em conta as funções de ligação. 
### Por exemplo, a frequência média de consultas associada a um ano a mais de escolaridade
### fica multiplicada por exp(0.024401) = 1.0247, ou seja, aumenta em 2.47%; a
### frequência média de consultas para pacientes que classificam a própria saúde
### como ruim (poor) é exp(0.249275) = 1.283 vezes (28.3% maior) que para aqueles
### que classificam como média, e assim por diante.

### Vamos estimar a probabilidade de 8 ou mais consultas para os seguintes perfis:

ind1 <- data.frame(hospital = 0, health = 'excellent', chronic = 1, gender = 'male', school = 8, insurance = 'no')
ind2 <- data.frame('hospital' = 2, 'health' = 'poor', 'chronic' = 2, 'gender' = 'female', 'school' = 8, 'insurance' = 'yes')
data.pred <- rbind(ind1, ind2)

pred_mu <- predict(ajuste_ZIBNB2, newdata = data.pred, what = 'mu', type = 'response')
pred_mu ### Estimativas de mu

pred_sigma <- predict(ajuste_ZIBNB2, newdata = data.pred, what = 'sigma', type = 'response')
pred_sigma ### Estimativas de sigma

pred_nu <- predict(ajuste_ZIBNB2, newdata = data.pred, what = 'nu', type = 'response')
pred_nu ### Estimativas de nu

pred_tau <- predict(ajuste_ZIBNB2, newdata = data.pred, what = 'tau', type = 'response')
pred_tau ### Estimativas de tau

### Agora, as estimativas de P(visits>=8)
pZIBNB(7, mu = pred_mu, sigma = pred_sigma, nu = pred_nu, tau = pred_tau, lower.tail = FALSE)
### Ou seja, a probabilidade estimada de 8 ou mais consultas é 0.066 para ind1, e 0.609
### para o segundo. 

### Vamos ver como ficam as distribuições ajustadas para ind1 e ind2:
par(mfrow = c(1,2), mar = c(5,4,2,2), las = 1)
plot(0:40, dZIBNB(0:40, mu = pred_mu[1], sigma = pred_sigma[1], nu = pred_nu[1], tau = pred_tau[1]), 
     xlab = 'Visitas', ylab = 'Probabilidade estimada', main = 'Indivíduo 1', type = 'h', lwd = 2)
plot(0:40, dZIBNB(0:40, mu = pred_mu[2], sigma = pred_sigma[2], nu = pred_nu[2], tau = pred_tau[2]), 
     xlab = 'Visitas', ylab = 'Probabilidade estimada', main = 'Indivíduo 2', type = 'h', lwd = 2)