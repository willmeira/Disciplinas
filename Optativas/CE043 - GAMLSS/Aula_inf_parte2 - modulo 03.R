require(gamlss)
require(gamlss.demo)
require(gamlss.util)
require(faraway)
require(boot)

options(device = 'x11')

########################################################################
### Parte 2

### Vamos usar a base de dados worldcup do pacote faraway. O objetivo
### aqui é modelar o número de chutes a gol em função da posição do jogador
### e do tempo que ele ficou em campo. Os dados são referentes aos jogos
### da copa do mundo de 2010.

data("worldcup")
help("worldcup")

dados <- worldcup[worldcup$Position!='Goalkeeper',]
### Excluindo os goleiros da base.

par(cex = 1.25, las = 1, mar = c(4,4,2,2))
barplot(table(factor(dados$Shots)), xlab = 'Número de chutes',
        ylab = 'Frequência')

dados$Position <- factor(dados$Position)
### Apenas para excluir o nível 'Goalkeeper' do fator 'Position'.

### Alguns gráficos, para iniciar a análise.
par(cex = 1.25, las = 1, mar = c(4,4,2,2))
plot(Shots ~ Position, data = dados)
### Os gráficos indicam que atacantes chutam mais que meio-campistas, que
### por sua vez chutam mais que os jogadores de defesa.

plot(jitter(Shots) ~ Time, data = dados, ylab = 'Shots', col = 'blue', 
     cex = 0.8, pch = 20)
### A frequência de chutes aumenta conforme o tempo em campo. Percebe-se
### aumento tanto na média (locação) quanto na variância (escala). 

### Vamos começar pelo ajuste de um modelo log-linear, com resposta Poisson.
ajustePO <- gamlss(Shots ~ Position + Time, data = dados, family = PO)
plot(ajustePO)
### Evidentemente o modelo não ajusta bem os dados.

### Vamos agora substituir a Poisson pela distribuição binomial negativa.
### Testaremos diferentes especificações para o modelo.

ajusteNB1 <- gamlss(Shots ~ Position + Time,  data = dados, family = NBI)
### Modelo sem covariáveis para o parâmetro de escala.

ajusteNB2 <- gamlss(Shots ~ Position + Time, sigma.fo =~ Position, 
                    data = dados, family = NBI)
### Modelo com a posição na modelagem do parâmetro de escala.

ajusteNB3 <- gamlss(Shots ~ Position + Time, sigma.fo =~ Position + Time, 
                    data = dados, family = NBI)
### Modelo com a posição e o tempo na modelagem do parâmetro de escala.

GAIC(ajustePO, ajusteNB1, ajusteNB2, ajusteNB3, k = 2)
### O ajuste do modelo2 proporciona menor valor de AIC.

LR.test(ajusteNB2, ajusteNB3)
### O teste da razão de verossimilhanças não indica diferença significativa
### dos ajustes dos dois modelos. Logo, devemos optar pelo modelo mais simples,
### sem a covariável "Time" no parâmetro de escala.

plot(ajusteNB2)
wp(ajusteNB2)
### O modelo produz ajuste satisfatório.  

### Como o ajuste de um GAMLSS pode envolver até quatro parâmetros, (mu,
### sigma, nu e tau) as predições podem ser aplicadas a qualquer um deles.

summary(ajusteNB2)

help("predict.gamlss")

### Vamos criar uma nova base para predição. Considere jogadores com as
### seguintes covariáveis.

ndados <- data.frame(Position = rep(c('Defender', 'Forward', 'Midfielder'), each = 3),
                                    Time = rep(c(100, 200, 400), times = 3))

predict(ajusteNB2, newdata = ndados, what = 'mu')
### Valores ajustados de mu na escala do preditor.

pmu <- predict(ajusteNB2, newdata = ndados, what = 'mu', type = 'response')
pmu <- round(pmu, 3)
data.frame(ndados, pmu)
### Valores ajustados de mu na escala da resposta.

predict(ajusteNB2, newdata = ndados, what = 'sigma')
### Valores ajustados de sigma na escala do preditor.

ps <- predict(ajusteNB2, newdata = ndados, what = 'sigma', type = 'response')
ps <- round(ps, 3)
ps
### Valores ajustados de sigma na escala da resposta.

### Vamos estimar a probabilidade de se ter ao menos um chute a gol para
### cada um dos nove casos (combinações de posição e tempo).

p1m <- pNBI(q = 0, mu = pmu, sigma = ps, lower.tail = FALSE)
p1m <- round(p1m, 3)
p1m

### Agora, vamos calcular a probabilidade de cinco chutes ou mais, em cada
### caso.

p5m <- pNBI(q = 4, mu = pmu, sigma = ps, lower.tail = FALSE)
p5m <- round(p5m, 3)
p5m

### Concatenando os resultados.

data.frame(ndados, pmu, ps, p1m, p5m)

### Vamos plotar a função de probabilidade estimada para os nove casos.

par(las = 1, mar = c(5,4,3,2), mfrow = c(3,3))
for(i in 1:9)
    plot(0:25, dNBI(0:25, mu = pmu[i], sigma = ps[i]), type = 'h', xlab = 'Shots',
         ylab = 'Probabilidade', lwd = 1.5, cex.lab = 1.4, cex.axis = 1.4,
         main = paste('Caso',i))

### Agora vamos extrair também os erros padrões correspondentes às predições.
### O pacote gamlss ainda não permite obter erros padrões para observações
### que não estão na base. Vamos resolver isso aumentando a base com os dados
### de predição. No ajuste do modelo daremos peso zero a tais observações.

dados_ajuste <- dados[,c('Shots', 'Position', 'Time')]
### Selecionando apenas as variáveis usadas na análise.

dados_predic <- data.frame(1, ndados)
### Novos dados acrescentados de um vetor de uns para a resposta (poderia
### ser qualquer outro valor, não será usado para ajuste ou predição).

names(dados_predic)[1] <- 'Shots'
dados_aument <- data.frame(rbind(dados_ajuste,dados_predic))
tail(dados_aument, 15)
### Concatenando os dados de ajuste e de predição por linhas. Nas linhas
### 1 a 559 estão os dados de ajuste; nas linhas 560 a 568 os de predição.

pesos <- c(rep(1,559), rep(0,9))
pesos
dados_aument$pesos <- pesos
### Vetor de pesos.

ajusteNB2_2 <- gamlss(Shots ~ Position + Time, sigma.fo =~ Position, 
                    data = dados_aument, family = NBI, weights = pesos)
ajusteNB2_2
### Observe que o modelo ajustado é idêntico ao obtido anteriormente.

pred_se <- predict(ajusteNB2_2, what = 'mu', type = 'link', se.fit = TRUE)

pred1 <- pred_se$fit[560:568] 
pred1 ### Estimativas na escala da ligação.

erros1 <- pred_se$se.fit[560:568] 
erros1 ### Erros padrões na escala da ligação.

pred_se2 <- predict(ajusteNB2_2, what = 'mu', type = 'response', se.fit = TRUE)

pred2 <- pred_se2$fit[560:568] 
pred2 ### Estimativas na escala da resposta.
erros2 <- pred_se2$se.fit[560:568] 
erros2 ### Erros padrões na escala da resposta.

### Erros padrões na escala da resposta são aproximados via método delta,
### caso a função de ligação seja outra que não a identidade. Nesse caso,
### esses valores devem ser usados com cautela.

predict(ajusteNB2, type = 'terms') 
### Com type = 'terms' temos a contribuição de cada variável na resposta. 

predictAll(ajusteNB2, newdata = ndados, type = 'link')
### A função predictALL retorna as predições para cada parâmetro do modelo.

predictAll(ajusteNB2, newdata = ndados, type = 'response')
### Agora na escala da resposta.


########################################################################
### Agora, vamos usar simulação bootstrap para avaliar os erros padrões 
### e construir intervalos de confiança. Usaremos duas abordagens: paramétrica
### (simulando valores para a resposta com base no modelo ajustado, substituindo
### os parâmetros pelas respectivas estimativas) e outra não paramétrica
### (reamostrando com reposição as observações da base).

### NOTA: SIMULAÇÂO LEVA TEMPO, PARA ILUSTRAÇÃO TROCAR R = 999 por R = 99.

### Bootstrap paramétrico
fit_nb <- data.frame(Time = dados$Time, Position = dados$Position, 
                     fit_mu  = fitted(ajusteNB2), 
                     fit_sigma = fitted(ajusteNB2, parameter = 'sigma'))
### Base auxiliar, com as observações originais e as estimativas de mu e
### sigma produzidas pelo modelo.

head(fit_nb)

### A função boot_nb1 será usada para a simulação. A cada rodada, simulamos
### novos valores para a resposta para cada observação da base e ajustamos
### o modelo para os dados simulados. Ao final de cada iteração extraímos
### as estimativas.

boot_nb1 <- function(datab, i){
    datab$Shots <- rNBI(n = nrow(datab), mu = datab$fit_mu, sigma = datab$fit_sigma)
    ajusteboot <- gamlss(Shots ~ Position + Time, sigma.fo =~ Position, data = datab, 
                         family = NBI, method = RS(100), c.crit = 0.001, start.from = ajusteNB2)
    estimativas <- c(coef(ajusteboot), coef(ajusteboot, parameter = 'sigma'))
    estimativas
}
set.seed(1)
b1 <- boot(data = fit_nb, statistic = boot_nb1, R = 999)

plot(b1, index = 3)
boot.ci(b1, type = 'perc', index = 3) ### IC do tipo percentil
confint(ajusteNB2)[3,] ### IC Wald

### Bootstrap não paramétrico
fit_nb2 <- data.frame(Time = dados$Time, Position = dados$Position, 
                      Shots = dados$Shots)
### Base de dados auxiliar.

### A função boot_nb2 será usada para a simulação. A cada rodada, simulamos
### novas linhas para a base reamostrando, com reposição, as linhas da base original, 
### e ajustamos o modelo para os dados simulados. Ao final de cada iteração extraímos
### as estimativas.

boot_nb2 <- function(datab, i){
    datab <- datab[i,]
    ajusteboot <- gamlss(Shots ~ Position + Time, sigma.fo =~ Position, data = datab, 
                         family = NBI, method = RS(100), c.crit = 0.001, start.from = ajusteNB2)
    estimativas <- c(coef(ajusteboot), coef(ajusteboot, parameter = 'sigma'))
    estimativas
}

b2 <- boot(data = fit_nb2, statistic = boot_nb2, R = 999)

plot(b2, index = 3) 
boot.ci(b2, type = 'perc', index = 3) ### IC do tipo percentil
confint(ajusteNB2)[3,] ### IC Wald

### Vamos comparar os erros padrões extraídos da matriz de covariâncias,
### os erros padrões robustos e aqueles obtidos via bootstrap paramétrico
### e não paramétrico.


options(scipen = 5)
round(data.frame(VCOV = vcov(ajusteNB2, type = 'se'),
                 VCOV_rb = vcov(ajusteNB2, type = 'se', robust = TRUE),
                 BOOTP = apply(b1$t, 2, sd), 
                 BOOTNP = apply(b2$t, 2, sd)),5)


