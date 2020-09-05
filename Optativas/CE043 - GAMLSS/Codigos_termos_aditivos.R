########################################################################
### Exemplo 
require(gamlss)
require(MASS)
require(gamlss.add)
require(ISLR)
require(car)

data("mcycle")
help("mcycle")
head("mcycle")

x11(width = 10, height = 10)
par(cex = 1.2, las = 1)
plot(accel ~ times, data = mcycle, col = 'blue', ylab = 'Aceleração', 
     xlab = 'Tempo após o impacto (ms)', main = 'Polinômios')

########################################################################
### Modelos paramétricos. Começando pelos polinomiais.

### Primeira ordem.
ajuste1p <- gamlss(accel ~ times, data = mcycle)
lines(sort(mcycle$times), fitted(ajuste1p)[order(mcycle$times)], col = 'black', lwd = 2)

### Segunda ordem.
ajuste2p <- gamlss(accel ~ poly(times,2), data = mcycle)
lines(sort(mcycle$times), fitted(ajuste2p)[order(mcycle$times)], col = 'red', lwd = 2)

### Terceira ordem.
ajuste3p <- gamlss(accel ~ poly(times,5), data = mcycle)
lines(sort(mcycle$times), fitted(ajuste3p)[order(mcycle$times)], col = 'blue', lwd = 2)

### Ordem 10.
ajuste4p <- gamlss(accel ~ poly(times,10), data = mcycle)
lines(sort(mcycle$times), fitted(ajuste4p)[order(mcycle$times)], col = 'green', lwd = 2)

legend(x = "bottomright", lty = 1, lwd = 2, col = c('black', 'red', 'blue', 'green'),
       legend = c('Ordem 1', 'Ordem 2', 'Ordem 5', 'Ordem 10'))

GAIC(ajuste1p, ajuste2p, ajuste3p, ajuste4p, k = 2) ### Comparação via AIC.
GAIC(ajuste1p, ajuste2p, ajuste3p, ajuste4p, k = log(133)) ### Comparação via BIC.

### Em ambos os casos o polinômio de maior ordem produz menores valores. No entanto,
### fica evidente que o polinômio de grau 10 é demasiadamente complexo para ajustar
### os dados, apresentando várias oscilações que não refletem a relação entre as
### variáveis.



########################################################################
### Polinômios fracionários.
plot(accel ~ times, data = mcycle, col = 'blue', ylab = 'Aceleração', 
     xlab = 'Tempo após o impacto (ms)', main = 'Polinômios fracionários')

### Ordem 1.
ajuste1fp <- gamlss(accel ~ fp(times, npoly = 1), data = mcycle)
lines(sort(mcycle$times), fitted(ajuste1fp)[order(mcycle$times)], col = 'black', lwd = 2)
getSmo(ajuste1fp)$power
### O único termo inserido no modelo é x^2.

### Ordem 2.
ajuste2fp <- gamlss(accel ~ fp(times, npoly = 2), data = mcycle)
lines(sort(mcycle$times), fitted(ajuste2fp)[order(mcycle$times)], col = 'red', lwd = 2)
getSmo(ajuste2fp)$power
### Os termos inseridos no modelo são x^0.5 e x^0.5*log(x).

### Ordem 3.
ajuste3fp <- gamlss(accel ~ fp(times, npoly = 3), data = mcycle)
lines(sort(mcycle$times), fitted(ajuste3fp)[order(mcycle$times)], col = 'blue', lwd = 2)
getSmo(ajuste3fp)$power
### Os termos inseridos no modelo são x^2 e x^2*log(x) e x^2*(log(x))^2.

legend(x = "bottomright", lty = 1, lwd = 2, col = c('black', 'red', 'blue'),
       legend = c('Ordem 1', 'Ordem 2', 'Ordem 3'))

GAIC(ajuste1fp, ajuste2fp, ajuste3fp, k = 2)
### Novamente o modelo com maior número de parâmetros produziu menor AIC, mas o
### ajuste é claramente insatisfatório.


########################################################################
### Regressão por splines
plot(accel ~ times, data = mcycle, col = 'blue', ylab = 'Aceleração', 
     xlab = 'Tempo após o impacto (ms)', main = 'Regressão por splines')

ajuste1rs <- gamlss(accel ~ bs(times, df = 3), data = mcycle)
attr(bs(mcycle$times, df = 3),"knots") ### Zero nós.
### Os três graus de liberdade, neste caso, estão associados aos termos linear,
### quadrático e cúbico globais. Não há graus de liberdade para parâmetros adicionais,
### referentes a ajustes locais (entre nós).

model.matrix(ajuste1rs)
### Matriz do modelo (X)

ajuste2rs <- gamlss(accel ~ bs(times, df = 4), data = mcycle)
attr(bs(mcycle$times, df = 4),"knots") 
### Com quatro graus de liberdade temos um parâmetro adicional no modelo que permite
### a divisão do intervalo de x em dois. Observe que, por default, a mediana de x
### é usada como coordenada do único nó.

### Na sequência, para cada grau de liberdade adicional temos um nó e um subintervalo
### a mais no modelo.
ajuste3rs <- gamlss(accel ~ bs(times, df = 5), data = mcycle)
attr(bs(mcycle$times, df = 5),"knots") ### Dois nós.
X <- model.matrix(ajuste3rs);
ajuste3_rs_alt <- gamlss(accel ~ X - 1, data = mcycle)

ajuste4rs <- gamlss(accel ~ bs(times, df = 6), data = mcycle)
attr(bs(mcycle$times, df = 6),"knots") ### Três nós.

ajuste5rs <- gamlss(accel ~ bs(times, df = 7), data = mcycle)
attr(bs(mcycle$times, df = 7),"knots") ### Quatro nós.

ajuste6rs <- gamlss(accel ~ bs(times, df = 8), data = mcycle)
attr(bs(mcycle$times, df = 8),"knots") ### Cinco nós.

ajuste7rs <- gamlss(accel ~ bs(times, df = 9), data = mcycle)
attr(bs(mcycle$times, df = 9),"knots") ### Seis nós.

ajuste8rs <- gamlss(accel ~ bs(times, df = 10), data = mcycle)
attr(bs(mcycle$times, df = 10),"knots") ### Sete nós.

ajuste9rs <- gamlss(accel ~ bs(times, df = 12), data = mcycle)
attr(bs(mcycle$times, df = 12),"knots") ### Nove nós.

ajuste10rs <- gamlss(accel ~ bs(times, df = 15), data = mcycle)
attr(bs(mcycle$times, df = 15),"knots") ### Doze nós.

### Vamos comparar os modelos usando GAIC com diferentes constantes de
### penalização.

GAIC(ajuste1rs, ajuste2rs, ajuste3rs, ajuste4rs,
     ajuste5rs, ajuste6rs, ajuste7rs, ajuste8rs,
     ajuste9rs, ajuste10rs, k = 2)
### O ajuste com doze nós internos é selecionado. 

GAIC(ajuste1rs, ajuste2rs, ajuste3rs, ajuste4rs,
     ajuste5rs, ajuste6rs, ajuste7rs, ajuste8rs,
     ajuste9rs, ajuste10rs, k = 3.84)
### O ajuste com quatro nós internos é selecionado. 

GAIC(ajuste1rs, ajuste2rs, ajuste3rs, ajuste4rs,
     ajuste5rs, ajuste6rs, ajuste7rs, ajuste8rs,
     ajuste9rs, ajuste10rs, k = log(nrow(mcycle)))
### O ajuste com quatro nós internos é selecionado. 


lines(sort(mcycle$times), fitted(ajuste1rs)[order(mcycle$times)], col = 'black', lwd = 2)
lines(sort(mcycle$times), fitted(ajuste4rs)[order(mcycle$times)], col = 'red', lwd = 2)
lines(sort(mcycle$times), fitted(ajuste6rs)[order(mcycle$times)], col = 'blue', lwd = 2)
lines(sort(mcycle$times), fitted(ajuste10rs)[order(mcycle$times)], col = 'green', lwd = 2)

legend(x = "bottomright", lty = 1, lwd = 2, col = c('black', 'red', 'blue', 'green'),
       legend = c('df = 3; K = 0', 'df = 5; K = 2', 'df = 8; K = 5', 'df = 15; K = 12'))

### Observe que o modelo selecionado via AIC é diferente do escolhido usando as 
### demais constantes de penalização. A penalização a ser utilizada é uma questão 
### em aberto, de forma que AIC, em algumas aplicações, pode ser muito
### permissivo, conduzindo a modelos com elevado número de parâmetros, enquanto BIC pode ser 
### muito conservador, conduzindo à seleção de modelos com falta de ajuste. Valores
### intermediários para a constante de penalização (como k=3 ou k=4) podem ser 
### considerados.


########################################################################
### Regressão usando natural splines

### Como alternativa ao modelo de regressão por splines selecionado via AIC na
### passagem anterior, vamos ajustar o correspondente modelo usando splines naturais.

### Regressão por splines
plot(accel ~ times, data = mcycle, col = 'blue', ylab = 'Aceleração', 
     xlab = 'Tempo após o impacto (ms)', main = 'Natural splines vs cubic splines')

ajuste0rs <- gamlss(accel ~ bs(times, df = 10), data = mcycle)
attr(bs(mcycle$times, df = 10),"knots")

ajuste0nrs <- gamlss(accel ~ ns(times, df = 8), data = mcycle)
attr(ns(mcycle$times, df = 8),"knots")

lines(sort(mcycle$times), fitted(ajuste0rs)[order(mcycle$times)], col = 'black', lwd = 2)
lines(sort(mcycle$times), fitted(ajuste0nrs)[order(mcycle$times)], col = 'red', lwd = 2)

legend(x = "bottomright", lty = 1, lwd = 2, col = c('black', 'red'),
       legend = c('Cubic spline', 'Natural cubic spline'))

########################################################################
### Modelo de nós livres.

### Nesta aplicação de modelos com nós livres vamos considerar apenas splines
### lineares.

plot(accel ~ times, data = mcycle, col = 'blue', ylab = 'Aceleração', 
     xlab = 'Tempo após o impacto (ms)', main = 'Modelo de nós livres')

### Ajuste com um nó.
ajuste1fk <- gamlss(accel ~ fk(times, degree = 1, start = 20), data = mcycle)
getSmo(ajuste1fk) ### Posição estimada para o único nó.

### Ajuste com dois nós.
ajuste2fk <- gamlss(accel ~ fk(times, degree = 1, start = c(10,30)), data = mcycle)
getSmo(ajuste2fk) ### Posição estimada para os dois nós.

### Ajuste com três nós.
ajuste3fk <- gamlss(accel ~ fk(times, degree = 1, start = c(10,20,30)), data = mcycle)
getSmo(ajuste3fk) ### Posição estimada para os três nós.

### Ajuste com cinco nós.
ajuste4fk <- gamlss(accel ~ fk(times, degree = 1, start = c(10,20,30, 40, 50)), data = mcycle)
getSmo(ajuste4fk) ### Posição estimada para os cinco nós.

lines(sort(mcycle$times), fitted(ajuste1fk)[order(mcycle$times)], col = 'black', lwd = 2)
lines(sort(mcycle$times), fitted(ajuste2fk)[order(mcycle$times)], col = 'red', lwd = 2)
lines(sort(mcycle$times), fitted(ajuste3fk)[order(mcycle$times)], col = 'blue', lwd = 2)
lines(sort(mcycle$times), fitted(ajuste4fk)[order(mcycle$times)], col = 'green', lwd = 2, lty = 2)
legend(x = "bottomright", lty = 1, lwd = 2, col = c('black', 'red', 'blue', 'green'),
       legend = c('K = 1', 'K = 2', 'K = 3', 'K = 5'))

########################################################################
########################################################################
########################################################################
### Usando suavizadores não paramétricos.

### Splines cúbicos. Vamos testar para diferentes números de graus de liberdade.
plot(accel ~ times, data = mcycle, col = 'blue', ylab = 'Aceleração', 
     xlab = 'Tempo após o impacto (ms)', main = 'Smoothing splines')

### df = 0.
ajuste1cs <- gamlss(accel ~ cs(times, df = 0), data = mcycle)
lines(sort(mcycle$times), fitted(ajuste1cs)[order(mcycle$times)], col = 'black', lwd = 2)
getSmo(ajuste1cs)$lambda1

### df = 1.
ajuste2cs <- gamlss(accel ~ cs(times, df = 1), data = mcycle)
lines(sort(mcycle$times), fitted(ajuste2cs)[order(mcycle$times)], col = 'brown', lwd = 2)
getSmo(ajuste2cs)$lambda1

### df = 5.
ajuste3cs <- gamlss(accel ~ cs(times, df = 5), data = mcycle)
lines(sort(mcycle$times), fitted(ajuste3cs)[order(mcycle$times)], col = 'blue', lwd = 2)
getSmo(ajuste3cs)$lambda1

### df = 10.
ajuste4cs <- gamlss(accel ~ cs(times, df = 10), data = mcycle)
lines(sort(mcycle$times), fitted(ajuste4cs)[order(mcycle$times)], col = 'red', lwd = 2)
getSmo(ajuste4cs)$lambda1

### df = 30.
ajuste5cs <- gamlss(accel ~ cs(times, df = 20), data = mcycle)
lines(sort(mcycle$times), fitted(ajuste5cs)[order(mcycle$times)], col = 'orange', lwd = 2)
getSmo(ajuste5cs)$lambda1

legend(x = "bottomright", lty = 1, lwd = 2, col = c('black', 'brown', 'blue', 'red', 'orange'),
       legend = c('df = 0', 'df = 1', 'df = 5', 'df = 10', 'df = 20'))

### Comparação dos ajustes usando critérios de informação.
GAIC(ajuste1cs, ajuste2cs, ajuste3cs, ajuste4cs, ajuste5cs, k = 2) ### AIC
GAIC(ajuste1cs, ajuste2cs, ajuste3cs, ajuste4cs, ajuste5cs, k = 3.84) ### k=3.84
GAIC(ajuste1cs, ajuste2cs, ajuste3cs, ajuste4cs, ajuste5cs, k = log(nrow(mcycle))) ### BIC


########################################################################
### P-splines.
plot(accel ~ times, data = mcycle, col = 'blue', ylab = 'Aceleração', 
     xlab = 'Tempo após o impacto (ms)')

### Usando pb() não precisaremos declarar o número de graus de liberdade, mas ele
### será determinado com base na otimização de diferentes critérios.

### Vamos comparar diferentes métodos para estimação do parâmetro de penalização
### usando p-splines.

ajuste1pb <- gamlss(accel ~ pb(times, method = 'ML'), 
                    sigma.fo=~ pb(times, method = 'ML'), data = mcycle)
ajuste1pb$df.fit
lines(sort(mcycle$times), fitted(ajuste1pb)[order(mcycle$times)], col = 'black', lwd = 2)

ajuste2pb <- gamlss(accel ~ pb(times, method = 'GAIC'), data = mcycle)
ajuste2pb$df.fit
lines(sort(mcycle$times), fitted(ajuste2pb)[order(mcycle$times)], col = 'red', lwd = 2)

ajuste3pb <- gamlss(accel ~ pb(times, method = 'GCV'), data = mcycle)
ajuste3pb$df.fit
lines(sort(mcycle$times), fitted(ajuste3pb)[order(mcycle$times)], col = 'blue', lwd = 2)

legend(x = "bottomright", lty = 1, lwd = 2, col = c('black', 'red', 'blue'),
       legend = c('ML - df = 13.33', 'GAIC - df = 12.46', 'GCV - df = 11.20'))


### Para finalizar, vamos plotar os modelos selecionados usando diferentes tipos 
### de termos aditivos.

plot(accel ~ times, data = mcycle, col = 'blue', ylab = 'Aceleração', 
     xlab = 'Tempo após o impacto (ms)', main = 'Modelos selecionados')

### Polinômio
lines(sort(mcycle$times), fitted(ajuste4p)[order(mcycle$times)], col = 'aquamarine3', lwd = 2)

### Polinômio fracionário
lines(sort(mcycle$times), fitted(ajuste3fp)[order(mcycle$times)], col = 'brown', lwd = 2)

### Natural splines
lines(sort(mcycle$times), fitted(ajuste0nrs)[order(mcycle$times)], col = 'deeppink2', lwd = 2)

### Modelo de nós livres
lines(sort(mcycle$times), fitted(ajuste4fk)[order(mcycle$times)], col = 'blue', lwd = 2)

### Smoothing splines
lines(sort(mcycle$times), fitted(ajuste4cs)[order(mcycle$times)], col = 'black', lwd = 2)

### P-splines
lines(sort(mcycle$times), fitted(ajuste2pb)[order(mcycle$times)], col = 'green', lwd = 2)

legend(x = "bottomright", lty = 1, lwd = 2, col = c('aquamarine3', 'brown', 'deeppink2', 'blue', 'black', 'green'),
       legend = c('Polinômio', 'Polinômio frac.', 'Natural splines', 'Nós livres', 'Smoothing splines', 'P-splines'))

### Comparação via critério de informação.
GAIC(ajuste4p, ajuste3fp, ajuste0nrs, ajuste4fk, ajuste4cs, ajuste2pb, k = 2) ### AIC
GAIC(ajuste4p, ajuste3fp, ajuste0nrs, ajuste4fk, ajuste4cs, ajuste2pb, k = 3.84) ### k=3.84
GAIC(ajuste4p, ajuste3fp, ajuste0nrs, ajuste4fk, ajuste4cs, ajuste2pb, k = log(133)) ### BIC


################################################################################
### Análise dos preços de imóveis em bairros (distritos) de Boston.

data(Boston)
help(Boston)
require(GGally)

### Para esta aplicação vamos considerar as seguintes variáveis:
### medv: Valor mediano dos imóveis ocupados, em U$1.000 (resposta);
### crim: Taxa de crimes per-capita;
### rm: Número médio de cômodos por imóvel;
### lstat: Porcentagem da população na classe mais baixa.

Boston2 <- Boston[,c("medv", "crim", "rm", "lstat")]
summary(Boston2)
ggpairs(Boston2)
### A distribuição das taxas de criminalidade é bastante assimétrica. Vamos considerar
### a log taxa em seu lugar. 

Boston2$crim <- log(Boston2$crim)
ggpairs(Boston2)

### As variáveis estão claramente correlacionadas, mas as relações, em
### vários casos, são acentuadamente não lineares, com variâncias não constantes.

### Comecemos por um modelo linear em que as três covariáveis são incluídas
### aos preditores de mu e sigma através de termos paramétricos lineares.
ajuste1 <- gamlss(medv ~ lstat + crim + rm,
                  sigma.fo=~ lstat + crim + rm, family = NO, data = Boston2)
plot(ajuste1)

### Os termplots permitem avaliar os efeitos parciais das covariáveis nos parâmetros
### do modelo segundo o modelo ajustado.
term.plot(ajuste1, what = 'mu', pages = 1)
term.plot(ajuste1, what = 'sigma', pages = 1)

### Vamos ajustar novo modelo inserindo cada uma das variáveis (em ambos os
### preditores) através de smoothing splines.

ajuste2 <- gamlss(medv ~ pb(lstat) + pb(crim) + pb(rm),
                  sigma.fo=~ pb(lstat) + pb(crim) + pb(rm), family = NO, 
                  data = Boston2)
plot(ajuste2)
term.plot(ajuste2, what = 'mu', pages = 1)
term.plot(ajuste2, what = 'sigma', pages = 1)

### Ao usar splines, o modelo acaba ajustando parte da não linearidade presente
### nos dados. No entanto, fica bem evidente, pelos gráficos de resíduos, que
### ainda não temos um bom ajuste. Vamos tentar outra distribuição. Para fins de 
### ilustração, escolhemos a distribuição Box-Cox t (BCT).

help(BCT)

ajuste3 <- gamlss(medv ~ pb(lstat) + pb(crim) + pb(rm),
                  sigma.fo=~ pb(lstat) + pb(crim) + pb(rm), family = BCT, 
                  data = Boston2, method = mixed(30,20))
plot(ajuste3)
term.plot(ajuste3, what = 'mu', pages = 1)
term.plot(ajuste3, what = 'sigma', pages = 1)

GAIC(ajuste1, ajuste2, ajuste3, k = 3.84)
### O ajuste com distribuição BCT é claramente superior aos produzidos usando
### distribuição Normal. Os resíduos apresentam comportamento adequado
### (distribuição normal, variância homogênea, poucos resíduos com valores
### mais extremos).

### Podemos testar a inclusão de covariáveis também nos dois outros parâmetros do modelo.
ajuste4 <- update(ajuste3, nu.formula=~ lstat + crim + rm, tau.formula=~ lstat + crim + rm)

### Agora vamos testar via TRV (teste da razão de verossimilhanças) a inclusão
### dos parâmetros.

LR.test(ajuste3, ajuste4)
### Os termos incluídos são estatisticamente significativos (p<0.001)

summary(ajuste4)

### Na sequência poderíamos avaliar a remoção de covariáveis em alguns dos preditores,
### a inclusão de suavizadores em nu e tau, a remoção de suavizadores em mu e sigma.

### Vamos extrair os valores ajustados de mu, sigma, nu e tau para cada subúrbio.
fitted(ajuste4, what = 'mu')
fitted(ajuste4, what = 'sigma')
fitted(ajuste4, what = 'nu')
fitted(ajuste4, what = 'tau')

### Como alternativa podemos usar o predict para isso.
predict(ajuste4, what = 'mu', type = 'response')
predict(ajuste4, what = 'sigma', type = 'response')
predict(ajuste4, what = 'nu', type = 'response')
predict(ajuste4, what = 'tau', type = 'response')

### A vantagem do predict é a possibilidade de fazer predições para novas observações.
### Vamos fazer predições para um subúrbio hipotético com log(crim)=0, rm=5, lstat=5.

nov_sub <- data.frame(crim = 0, rm = 5, lstat = 5)

mu_chap <- predict(ajuste4, newdata = nov_sub, what = 'mu', type = 'response'); mu_chap
sig_chap <- predict(ajuste4, newdata = nov_sub, what = 'sigma', type = 'response'); sig_chap
nu_chap <- predict(ajuste4, newdata = nov_sub, what = 'nu', type = 'response'); nu_chap
tau_chap <- predict(ajuste4, newdata = nov_sub, what = 'tau', type = 'response'); tau_chap

### Usando as estimativas obtidas para os quatro parâmetros, vamos plotar o modelo
### (função densidade de probabilidade) ajustado.
curve(dBCT(x, mu = mu_chap, sigma = sig_chap, nu = nu_chap, tau = tau_chap), from = 10, to = 50,
      xlab = 'medv', ylab = 'Densidade')

### Algumas estimativas para esse subúrbio:

# P(medv<25)
pBCT(25, mu = mu_chap, sigma = sig_chap, nu = nu_chap, tau = tau_chap)

# P(medv>30)
pBCT(30, mu = mu_chap, sigma = sig_chap, nu = nu_chap, tau = tau_chap, lower.tail = FALSE)

# P(20<medv<30)
pBCT(30, mu = mu_chap, sigma = sig_chap, nu = nu_chap, tau = tau_chap)-
    pBCT(20, mu = mu_chap, sigma = sig_chap, nu = nu_chap, tau = tau_chap)

# Mediana para medv estimada para subúrbios som essas características
qBCT(0.5, mu = mu_chap, sigma = sig_chap, nu = nu_chap, tau = tau_chap)

# Quintis estimados para medv estimada para subúrbios som essas características
qBCT(c(0.2, 0.4, 0.6,0.8), mu = mu_chap, sigma = sig_chap, nu = nu_chap, tau = tau_chap)

### Repetir as análises para um subúrbio hipotético com log(crim)=1, rm=8, lstat=15

### A título de ilustração, vamos aplicar diferentes termos aditivos num mesmo
### modelo.

### Preditor de mu:
### lstat: termo paramétrico linear;
### crim: natural spline com dois graus de liberdade;
### rm: smoothing splines, com graus de liberdade a serem estimados.

### Preditor de sigma:
### lstat: removido do modelo, efeito parcial aproximadamente constante;
### crim: polinômio cúbico;
### rm: regression splines linear, com um único nó em 5.

ajuste5 <- gamlss(medv ~ lstat + ns(crim, df = 2) + pb(rm), 
                  sigma.fo=~ poly(crim,3) + bs(rm, knots = 5, degree = 1), 
                  family = BCT, data = Boston, method = RS(30))
term.plot(ajuste5, what = 'mu', pages = 1)
term.plot(ajuste5, what = 'sigma', pages = 1)


########################################################################
########################################################################
########################################################################
### Conteúdo adicional - Suavizadores para os níveis de fatores - a função pcat()

### Neste exemplo, vamos ilustrar a aplicação do suavizador implementado
### em pcat(), com o objetivo de agrupar níveis de um fator com efeitos 
### semelhantes.

### Vamos usar a base de dados disponível na página da disciplina. Trata-se
### de um estudo sobre a eficácia de diferentes abordagens de venda de um
### produto. As variáveis são as seguintes:

### abordagem: um fator com oito níveis (A-H), identificando o tipo de abordagem para venda;
### renda: em milhares de reais;
### venda: se a venda foi efetuada (Sim ou Não).

### O objetivo é modelar a probabilidade de venda em função da abordagem e
### ajustado também pela renda do indivíduo. Para isso, vamos usar regressão
### logística (família binomial).

dados <- read.csv2('https://docs.ufpr.br/~taconeli/CE06218/Dados4.csv')[,-1]

### Análise descritiva

tab1 <- table(dados$abordagem, dados$venda)
tab2 <- prop.table(tab1, 1)
dtab2 <- data.frame(tab2)
names(dtab2) <- c('Abordagem', 'Venda', 'Frequência')

x11(width = 14, height = 14)
ggplot(data=dtab2, aes(x=Abordagem, y=Frequência, fill=Venda)) +
    geom_bar(stat="identity", position=position_dodge()) +
    theme_bw(base_size = 14)

ggplot(data = dados, aes(x=venda, y=renda, fill=venda)) + geom_boxplot()+
    theme_bw(base_size = 14)

### Modelos ajustados.

ajuste_null <- gamlss(venda ~ 1, family = BI, data = dados)
### Modelo nulo.

ajuste1 <- gamlss(venda ~ renda, family = BI, data = dados)
### Modelo com efeito de renda (sem suavizador).

ajuste2 <- gamlss(venda ~ renda + abordagem, family = BI, data = dados)
### Modelo com efeito de renda e abordagem (sem suavizador).

ajuste3 <- gamlss(venda ~ pb(renda) + abordagem, family = BI, data = dados)
### Modelo com efeito de renda e abordagem (com suavizador para renda).

ajuste4 <- gamlss(venda ~ renda + pcat(abordagem), family = BI, data = dados)
### Modelo com efeito de renda e abordagem (com suavizador para abordagem).

ajuste5 <- gamlss(venda ~ pb(renda) + pcat(abordagem), family = BI, data = dados)
### Modelo com efeito de renda e abordagem (com suavizador para ambas).

GAIC(ajuste_null, ajuste1, ajuste2, ajuste3, ajuste4, ajuste5, k = 3)
### O ajuste5, com suavizador para renda e abordagem, produziu menor valor
### de GAIC. É o modelo selecionado.

plot(ajuste5)
### Diagnóstico de resíduos.

coef(getSmo(ajuste5, which = 2))
### Estimativas dos parâmetros para o fator

abord2 <- getSmo(ajuste5, which = 2)$factor
abord2
nlevels(abord2)
### A suavização resultou em uma reconfiguração para os níveis de abordagem.
### Agora, temos apenas quatro níveis. Agora, vamos visualizar o processo
### de suavização para essa variável categórica.

df <- getSmo(ajuste5, which = 2)$edf
df
### Graus de liberdade para o suavizador.

lambda <- getSmo(ajuste5, which = 2)$lambda
lambda
### parâmetro de penalização.

plotLambda(formula=venda ~ pb(renda), factor = abordagem, data = dados, 
           along = seq(-10, 2, 0.5), family = BI)
### Suavização em função de (log) lambda.

abline(v = log(lambda), col = 'red')
### Representação do valor de lambda aplicado. Assim, podemos ver que os
### níveis originais de abordagem foram agrupados da seguinte forma: 
### Nível 1: A, B, E; Nível 2: H; Nível 3: G, C; Nível 4: D, F.

### Vamos experimentar um método alternativo de estimação para lambda (GAIC):
ajuste5_v2 <- gamlss(venda ~ pb(renda) + pcat(abordagem, method = "GAIC", k = 3), 
                     family = BI, data = dados)
AIC(ajuste5, ajuste5_v2, k = 3)

lambda2 <- getSmo(ajuste5_v2, which = 2)$lambda
lambda2
abline(v = log(lambda2), col = 'blue')
### A penalização é ligeiramente maior. Mas os níveis do fator original 
### são agrupados de maneira similar à anterior. 

abord3 <- getSmo(ajuste5, which = 2)$factor
abord3
nlevels(abord3)

term.plot(ajuste5, what = 'mu', terms = 1)
### Gráfico para o efeito de renda no logito da probabilidade de venda.

### E se ajustarmos um novo modelo, análogo ao ajuste3 (com suavizador para
### renda, mas não para abordagem), mas substituindo o fator abordagem pelo
### novo fator, com quatro níveis?

ajuste3_v2 <- gamlss(venda ~ pb(renda) + abord2, family = BI, data = dados)
GAIC(ajuste5, ajuste3_v2)

### Os ajustes não são idênticos, uma vez que, no caso do ajuste com suavizador
### para abordagem, temos o efeito adicional da penalização.
