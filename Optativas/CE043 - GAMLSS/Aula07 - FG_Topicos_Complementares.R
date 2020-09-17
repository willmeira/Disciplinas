################################################################################
### Sessão 1- Dados contínuos com suporte no intervalo (0,1)

require("gamlss")
require("betareg")
require("gamlss.tr")
require("gamlss.inf")
require("gamlss.demo")
source('shiny_func_TFtr.R')

options(device = 'x11')

### Começamos com algumas demos.
demo.BE() ### Distribuição Beta (BE).
demo.GB1() ### Distribuição Beta generalizada (GB1)
### Faça o mesmo para as outras distribuições implementadas com suporte em (0,1)

### Parte 1 - Dados de estresse de mulheres saudáveis em uma cidade australiana.
data("StressAnxiety")
help("StressAnxiety")

par(mar = c(5,4,2,2), las = 1, mfrow = c(1,3), cex = 1.2)
hist(StressAnxiety$stress, breaks = 10, xlab = 'Ansiedade', main = '')
hist(StressAnxiety$anxiety, breaks = 10, xlab = 'Estresse', main = '')
plot(jitter(stress, amount = 0.01) ~ jitter(anxiety, amount = 0.01), data = StressAnxiety, 
     pch = 20, cex = 0.8, xlab = 'Ansiedade', ylab = 'Estresse')

### Vamos avaliar diferentes modelos para a distribuição marginal do estresse (sem
### ajustar o efeito da ansiedade). Num primeiro momento, vamos avaliar apenas as
### distribuições explicitamente implementadas na biblioteca gamlss.

fit1 <- fitDist(stress, type = "real0to1", k = 2, data = StressAnxiety)
fit1$fits
### A distribuição beta generalizada (GB1) produziu melhor ajuste (menor AIC)

par(mfrow = c(2,2))
histDist(y = stress, family = BE, data = StressAnxiety, ylim = c(0,3), main = 'BE')
histDist(y = stress, family = SIMPLEX, data = StressAnxiety, ylim = c(0,3), main = 'SIMPLEX')
histDist(y = stress, family = LOGITNO, data = StressAnxiety, ylim = c(0,3), main = 'LOGITNO')
histDist(y = stress, family = GB1, data = StressAnxiety, ylim = c(0,3), main = 'GB1')

### Agora, vamos criar duas novas distribuições com suporte em (0,1) a partir
### (I) da inversa da função logit e (II) truncamento de distribuições contínuas.
### Vamos considerar as distribuições T-Family (TF) e skew normal type 1 (ST1).

### Versões inversa-logito
gen.Family("TF", type = 'logit')
gen.Family("ST1", type = 'logit')

### Versões truncadas
gen.trun(c(0,1), "TF", type = "both")
gen.trun(c(0,1), "ST1", type = "both")

fit2 <- fitDist(stress, type = "real0to1", extra = c("logitTF", "logitST1", "TFtr", "ST1tr"),
                k = 2, data = StressAnxiety)
fit2$fits
### A versão truncada em (0,1) da distribuição TF produziu ajuste ligeiramente
### melhor aos dados que a distribuição GB1. Vamos comparar os dois modelos 
### com base nos resíduos.

TFtr()
### A distribuição TF truncada tem três parâmetros, com funções de ligação dos
### tipos identidade, log e log, respectivamente.

fit_GB1 <- gamlss(stress ~ 1, family = GB1, method = mixed(50,20), data = StressAnxiety)
fit_TFtr <- gamlss(stress ~ 1, family = TFtr, data = StressAnxiety)

### Vamos explorar a distribuição TF truncada. 
shinyApp(ui_TFtr, server_TFtr)

### Agora, o histograma com o modelo ajustado sobreposto.
hist(StressAnxiety$stress, probability = TRUE, main = '', ylim = c(0,3), xlab = 'Estresse', las = 1)
curve(dTFtr(x, mu = fitted(fit_TFtr, what = 'mu')[1], 
            sigma = fitted(fit_TFtr, what = 'sigma')[1], 
            nu = fitted(fit_TFtr, what = 'nu')[1]), 
      col = 'red', lwd = 2, add = TRUE)


par(mfrow = c(1,2))
wp(fit_GB1, main = 'GB1')
wp(fit_TFtr, main = 'TFtr')

### O próximo passo é encontrar um modelo de regressão apropriado para explicar
### o estresse em função da ansiedade. A família considerada será a versão 
### truncada gerada da distribuição TF.

ajuste1 <- gamlss(stress ~ anxiety, family = TFtr, data = StressAnxiety)
### Modelo com efeito linear de ansiedade em mu.

ajuste2 <- gamlss(stress ~ pb(anxiety), family = TFtr, method = mixed(20,10), data = StressAnxiety)
### Modelo com efeito de ansiedade em mu incorporado através de suavizador.

GAIC(ajuste1, ajuste2, k = 2)
### O ajuste 2 produziu menor AIC, sendo preferível.

ajuste3 <- gamlss(stress ~ pb(anxiety), sigma.formula =~ pb(anxiety),  family = TFtr, 
                  method = mixed(20,10), data = StressAnxiety)
### Modelo com efeito suave de ansiedade em sigma.

GAIC(ajuste2, ajuste3, k = 2)
### O ajuste 3 produziu menor AIC, e evidencia que também o parâmetro sigma depende da ansiedade.

ajuste4 <- gamlss(stress ~ pb(anxiety), sigma.formula =~ pb(anxiety), nu.formula =~ pb(anxiety),
                  family = TFtr, method = mixed(20,20), data = StressAnxiety)
### Modelo com efeito suave de ansiedade em nu.

GAIC(ajuste3, ajuste4, k = 2)
### O ajuste 3 ainda produziu menor AIC, e evidencia que o parâmetro nu não depende da ansiedade.

### Vamos avaliar os efeitos da ansiedade em mu e sigma.
term.plot(ajuste3, what = 'mu')
term.plot(ajuste3, what = 'sigma')

### Agora, vamos fazer predições para o nível de estresse considerando dois níveis
### distintos de ansiedade: 0.05 e 0.50.

pmu <- predict(ajuste3, newdata = data.frame(anxiety = c(0.05,0.50)), type = 'response')
pmu ### Estimativas de mu

psigma <- predict(ajuste3, newdata = data.frame(anxiety = c(0.05,0.50)), what = 'sigma', type = 'response')
psigma ### Estimativas de sigma

pnu <- fitted(ajuste3, what = 'nu')[1]
pnu ### Estimativa de nu (não depende da ansiedade)

### Distribuição ajustada para o nível de estresse para ansiedade igual a 0.05.
curve(dTFtr(x, mu = pmu[1], sigma = psigma[1], nu = pnu), from = 0.001, to = 0.999,
      xlab = 'Estresse', ylab = 'Densidade', ylim = c(0,12))

### Distribuição ajustada para o nível de estresse para ansiedade igual a 0.50.
curve(dTFtr(x, mu = pmu[2], sigma = psigma[2], nu = pnu), from = 0.001, to = 0.999,
      col = 'red', add = TRUE)

legend('topright', lty = 1, col = c('black', 'red'), legend = c('0.05','0.50'),
       title = 'Anxiety', bty = 'n')

################################################################################
### Sessão 2- Distribuições zero ajustadas e inflacionadas.

require(gamlss)
require(gamlss.demo)
require(insuranceData)

### Distribuição Gamma zero-ajustada (ZAGA).

### Distribuição ZAGA para diferentes valores de nu (P(Y=0)).
x11(width = 10, height = 10)
par(mfrow = c(2,2), las = 1, mar = c(4,4,2,2), cex = 1.3, lwd = 2)
plotZAGA(mu = 1, sigma = 0.75, nu = .1)
plotZAGA(mu = 1, sigma = 0.75, nu = .5)
plotZAGA(mu = 1, sigma = 0.75, nu = .75)
plotZAGA(mu = 1, sigma = 0.75, nu = .9)

### Simulação
x11(width = 12, height = 12)
par(mfrow = c(2,2), las = 1, mar = c(4,4,2,2), cex = 1.3)
hist(rZAGA(1000, mu = 1, sigma = 0.75, nu = .1), xlab = 'y', 
     main = expression(paste(nu, '=0.1')), breaks = 20)
hist(rZAGA(1000, mu = 1, sigma = 0.75, nu = .25), xlab = 'y', 
     main = expression(paste(nu, '=0.25')), breaks = 20)
hist(rZAGA(1000, mu = 1, sigma = 0.75, nu = .5), xlab = 'y', 
     main = expression(paste(nu, '=0.5')), breaks = 20)
hist(rZAGA(1000, mu = 1, sigma = 0.75, nu = .9), xlab = 'y', 
     main = expression(paste(nu, '=0.9')), breaks = 20)

### Demo
demo.ZAGA()

### Distribuição Normal Inversa zero-ajustada (ZAIG).

### Distribuição ZAIG para diferentes valores de nu (P(Y=0)).
x11(width = 10, height = 10)
par(mfrow = c(2,2), las = 1, mar = c(4,4,2,2), cex = 1.3, lwd = 2)
plotZAIG(mu = 1, sigma = 0.75, nu = .1)
plotZAIG(mu = 1, sigma = 0.75, nu = .5)
plotZAIG(mu = 1, sigma = 0.75, nu = .75)
plotZAIG(mu = 1, sigma = 0.75, nu = .9)

### Simulação
x11(width = 12, height = 12)
par(mfrow = c(2,2), las = 1, mar = c(4,4,2,2), cex = 1.3)
hist(rZAIG(1000, mu = 1, sigma = 0.75, nu = .1), xlab = 'y', 
     main = expression(paste(nu, '=0.1')), breaks = 20)
hist(rZAIG(1000, mu = 1, sigma = 0.75, nu = .25), xlab = 'y', 
     main = expression(paste(nu, '=0.25')), breaks = 20)
hist(rZAIG(1000, mu = 1, sigma = 0.75, nu = .5), xlab = 'y', 
     main = expression(paste(nu, '=0.5')), breaks = 20)
hist(rZAIG(1000, mu = 1, sigma = 0.75, nu = .9), xlab = 'y', 
     main = expression(paste(nu, '=0.9')), breaks = 20)

### Demo
demo.ZAIG()

### Distribuição Beta zero-inflacionada (BEZI).

### Distribuição BEZI para diferentes valores de nu (P(Y=0)).
x11(width = 10, height = 10)
par(mfrow = c(2,2), las = 1, mar = c(4,4,2,2), cex = 1.3, lwd = 2)
plotBEZI(mu = 0.25, sigma = 5, nu = .1)
plotBEZI(mu = 0.25, sigma = 5, nu = .5)
plotBEZI(mu = 0.25, sigma = 5, nu = .75)
plotBEZI(mu = 0.25, sigma = 5, nu = .9)

### Simulação
x11(width = 12, height = 12)
par(mfrow = c(2,2), las = 1, mar = c(4,4,2,2), cex = 1.3)
hist(rBEZI(1000, mu = 0.25, sigma = 5, nu = .1), xlab = 'y', 
     main = expression(paste(nu, '=0.1')), breaks = 20)
hist(rBEZI(1000, mu = 0.25, sigma = 5, nu = .25), xlab = 'y', 
     main = expression(paste(nu, '=0.25')), breaks = 20)
hist(rBEZI(1000, mu = 0.25, sigma = 5, nu = .5), xlab = 'y', 
     main = expression(paste(nu, '=0.5')), breaks = 20)
hist(rBEZI(1000, mu = 0.25, sigma = 5, nu = .9), xlab = 'y', 
     main = expression(paste(nu, '=0.9')), breaks = 20)

################################################################################
### Sessão 3- Aplicação de distribuições zero-ajustadas para dados contínuos.

### Nesta aplicação vamos modelar a quantia gerada por sinistros (ocorrências) 
### por 67.586 clientes de uma seguradora. Ocorre que apenas uma pequena
### parcela dos segurados produziu ao menos um sinistro, de tal maneira
### que, para os indivíduos que não geraram sinistros, a quantia gerada
### é exatamente igual a zero. Assim, precisamos recorrer a um modelo 
### com suporte nos reais positivos, mas que atribua probabilidade não
### nula para y=0.

### Vamos recorrer aos modelos ZAGA (Zero Adjusted Gamma) e ZAIG (Zero
### Adjusted Inverse Gaussian). Nos dois casos estamos considerando uma
### distribuição contínua com suporte nos reais positivos (Gamma ou IG),
### ajustada por uma massa de probabilidade não nula em y=0. Ambos os modelos 
### têm três parâmetros: mu, que é a média para a parte não nula do modelo;
### sigma, que é o parâmetro de dispersão e nu, referente à probabilidade
### de y=0. Como habitual para os modelos da família GAMLSS, podemos modelar
### cada um dos três parâmetros em função de covariáveis, que podem ser
### incorporadas ao preditor por termos paramétricos, splines, efeitos aleatórios...

### Vamos iniciar a análise carregando a base e visualizando os dados.
data("dataCar")
help("dataCar")
head(dataCar, 10)
summary(dataCar)

### Para a sequência da análise vamos considerar apenas um subconjunto das
### variáveis originais.
dados_car <- dataCar[,c('agecat', 'area', 'veh_body', 'gender', 'veh_value', 'claimcst0')]
dados_car$agecat <- as.factor(dados_car$agecat) ### Convertendo para fator.
head(dados_car, 20)

### Vamos explorar a distribuição marginal dos dados. Primeiramente, vamos
### verificar as frequências de segurados que apresentaram e que não 
### apresentaram sinistros.

tab1 <- table(ifelse(dados_car$claimcst0 == 0, 'No claims', 'Claims')); tab1
100*prop.table(tab1)
### Dos 67.856 segurados, 4.624 apresentaram ao menos um sinistro no período
### (6,8%), de maneira que a maioria (63232 segurados, perfazendo 93,2%
### do total) não registraram qualquer sinistro, não produzindo, portanto,
### qualquer quantia para efeito de restituição.

### Agora, vamos analisar a distribuição dos valores de sinistros produzidos
### pelos 4.624 segurados que apresentaram ao menos uma ocorrência.

x11(width = 12, height = 12)
par(cex = 1.25)
hist(dados_car$claimcst0[dados_car$claimcst0!=0], breaks = seq(0,60000,1000),
     probability = TRUE, main = '', xlab = 'Quantia', ylab = 'Densidade',
     ylim = c(0,0.0007))
lines(density(dados_car$claimcst0[dados_car$claimcst0!=0]), lwd = 2, col = 'red')
### Histograma com função densidade de probabilidade estimada sobreposta. 

### Apenas com caráter exploratório, e para fins didáticos, vamos ajustar 
### algumas distribuições a esse conjunto de 4.624 observações e comparar
### os ajustes via AIC.

ajEXP <- gamlssML(dados_car$claimcst0[dados_car$claimcst0!=0], family = EXP)
ajEXP
### Exponencial

ajGA <- gamlssML(dados_car$claimcst0[dados_car$claimcst0!=0], family = GA)
ajGA
### Gamma

ajIG <- gamlssML(dados_car$claimcst0[dados_car$claimcst0!=0], family = IG)
ajIG
### Normal inversa

ajWEI <- gamlssML(dados_car$claimcst0[dados_car$claimcst0!=0], family = WEI)
ajWEI
### Weibull

GAIC(ajEXP, ajGA, ajIG, ajWEI)
### Dentre as distribuições avaliadas, a normal inversa produziu melhor 
### ajuste (menor valor de AIC). 

### Vamos sobrepor o modelo ajustado ao histograma.
histDist(dados_car$claimcst0[dados_car$claimcst0!=0], family = 'IG', main = '')

### Agora, vamos seguir com o ajuste do modelo de regressão. As covariáveis inseridas
### nos três preditores foram previamente selecionadas. Mais adiante
### trataremos da seleção de covariáveis em GAMLSS. Por hora, vamos considerar
### o modelo especificado na sequência:

ajuste <- gamlss(claimcst0 ~ gender + agecat + area, 
                 sigma.formula =~ area, 
                 nu.formula =~ agecat + area + veh_value + veh_body, 
                 data = dados_car, family = ZAGA)
plot(ajuste)

ajuste2 <- gamlss(claimcst0 ~ gender + agecat + area, 
                  sigma.formula =~ area,
                  nu.formula =~ agecat + area + veh_value + veh_body, 
                  data = dados_car, family = ZAIG)
plot(ajuste2)
GAIC(ajuste, ajuste2, k=2)

### O modelo com resposta gaussiana inversa produziu menor AIC, indicando
### melhor ajuste. A superioridade do modelo ZAIG também pode ser verificada
### com base nos gráficos de resíduos.

### Vamos extrair as estimativas dos coeficientes de regressão para os
### três parâmetros.
coef(ajuste2, what = 'mu')
### A título de ilustração: Para o efeito de sexo, o valor médio de sinistro 
### é (exp(0.1561)-1)*100 = 16.89% maior para homens do que para mulheres
### (fixados os valores das demais covariáveis). De maneira semelhante, o
### valor médio de sinistro é (exp(0.3544)-1)*100 = 42.53% maior para 
### indivíduos da área F em relação a indivíduos da área A (referência).
### Importante ressaltar que mu corresponde ao valor médio para indivíduos
### que apresentaram ao menos um sinistro. 

coef(ajuste2, what = 'sigma')

coef(ajuste2, what = 'nu')
### A título de ilustração: A log-chance do indivíduo produzir sinistros
### é 0.4563 maior para a categoria de idade 6 em relação à categoria 1,
### de maneira que a chance de produzir zero sinistros para indivíduos da
### categoria 6 é a chance da categoria 1 multiplicada por exp(0.4563)=1.5782,
### fixados os valores das demais covariáveis. Ainda, a log-chance de produzir
### zero sinistros é 1.1468 maior para caminhões do que para ônibus (referência),
### de maneira que a chance de produzir zero sinistros para caminhões
### é a chance de zero sinistros para ônibus multiplicada por exp(1.1468)=3.1481.

### Agora, vamos extrair os valores ajustados para dez observações selecionadas
### ao acaso da base.
set.seed(123456)
data_fit <- sample(1:nrow(dataCar), 10)
fmu <- fitted(ajuste, 'mu')[data_fit]; fmu
fsigma <- fitted(ajuste, 'sigma')[data_fit]; fsigma
fnu <- fitted(ajuste, 'nu')[data_fit]; fnu
data.frame(dados_car[data_fit,], fmu, fsigma, fnu)

### Dando sequência, vamos fazer predições para os dois perfis declarados
### na sequência.
data_pred <- data.frame(gender = c('F', 'M'), agecat = c('2','6'), area = c('B', 'F'),
                        veh_value = c(20,5), veh_body = c('BUS', 'SEDAN'))
data_pred

predict(ajuste, newdata = data_pred, what = 'mu', type = 'response')
### Valor médio de sinistro para indivíduos com ao menos um sinistro.

predict(ajuste, newdata = data_pred, what = 'nu', type = 'response')
### Probabilidade estimada de não apresentar qualquer sinistro.

### Na sequência, vamos explorar os efeitos 
term.plot(ajuste, what = 'mu', pages = 1)
### O valor médio de sinistros é maior para o sexo masculino, diminui conforme
### a idade do segurado e é maior nas áreas F e E em relação às demais.

term.plot(ajuste, what = 'sigma', pages = 1)
### Quanto ao parâmetro de dispersão, tem-se maior valor estimado para a 
### área F e menor para a área D.

term.plot(ajuste, what = 'nu', pages = 1)
### A probabilidade do indivíduo não apresentar sinistros aumenta conforme
### a idade, diminui conforme o valor do veículo, é maior para veículos
### convencionais e menor para ônibus.

################################################################################
### Sessão 4- Dados inflacionados em (0,1] 

### Nesta aplicação vamos analisar dados referentes à função pulmonar espirométrica,
### que é um índice estabelecido para diagnóstico de obstrução das vias aéreas.

### O objetivo é modelar a função pulmonar espirométrica (FPE) em função da altura.

data("lungFunction")
help("lungFunction")

table(ifelse(lungFunction$slf == 1, '1', '(0,1)'))
### Temos 323 indivíduos na amostra com FPE=1. Para os demais, o valor da FPE 
### está no intervalo (0,1)

hist(lungFunction$slf[lungFunction$slf!=1], ylab = 'Frequência', xlab = 'FPE', main = '')
lines(c(1,1), c(0, sum(lungFunction$slf == 1)), col = 'red', lwd = 5)
### A distribuição marginal da FPE é assimétrica à esquerda, com inflação de uns. 

plot(slf~height, data = lungFunction, pch = 20, col = 'darkgrey', ylab = 'FPE')
### A FPE aparentemente é menor para indivíduos de maior estatura, e a inflação
### de uns também diminui quanto maior a altura.

lungFunction <- transform(lungFunction, logHeight = log(height))
### Usar a log(altura) ao invés da altura na escala original resulta em melhor
### ajuste. Você pode ajustar os modelos com a altura como covariável, para 
### verificar.

plot(slf~logHeight, data = lungFunction, pch = 20, col = 'darkgrey', ylab = 'FPE')

### Modelo 1 - Distribuição beta inflacionada de uns.
mbeinf1 <- gamlss(slf ~ pb(logHeight, method = 'GAIC', k = 6),
                  sigma.formula =~ pb(logHeight, method = 'GAIC', k = 6),
                  nu.formula =~ pb(logHeight, method = 'GAIC', k = 6),
                  data = lungFunction, family = BEINF1(nu.link = "logit"))
### O objetivo de usar k=6, ao invés de k=2 (AIC), é produzir curvas centílicas
### com maior suavidade.

### Modelo 2 - Distribuição logito SST inflacionada de uns (vamos criar)

gen.Family('SST', 'logit') ### Gerando a distribuição logito SST

mlogitSST <- gamlssInf0to1(y = slf,
                           mu.formula =~pb(logHeight, method = 'GAIC', k = 6),
                           sigma.formula =~ pb(logHeight, method = 'GAIC', k = 6),
                           nu.formula =~ pb(logHeight, method = 'GAIC', k = 6),
                           tau.formula =~ pb(logHeight, method = 'GAIC', k = 6),
                           xi1.formula =~ pb(logHeight, method = 'GAIC', k = 6),
                           data = lungFunction, family = logitSST, trace = TRUE)

### Modelo 2 - Distribuição SST truncada inflacionada de uns (vamos criar)
gen.trun(c(0,1), family = "SST", type = "both")  ### Gerando a distribuição SST truncada

mSSTtr <- gamlssInf0to1(y = slf,
                           mu.formula =~pb(logHeight, method = 'GAIC', k = 6),
                           sigma.formula =~ pb(logHeight, method = 'GAIC', k = 6),
                           nu.formula =~ pb(logHeight, method = 'GAIC', k = 6),
                           tau.formula =~ pb(logHeight, method = 'GAIC', k = 6),
                           xi1.formula =~ pb(logHeight, method = 'GAIC', k = 6),
                           data = lungFunction, family = SSTtr, trace = TRUE)

### Comparação dos ajustes
par(mfrow = c(1,3), cex = 1.25, las = 1)
wp(mbeinf1, ylim.all = 1)
wp(mlogitSST, ylim.all = 1)
wp(mSSTtr, ylim.all = 1)
### O modelo baseado na distribuição beta inflacionada não ajusta bem os dados.
### Os demais, a princípio, produzem ajustes satisfatórios. Vamos comparar os ajustes
### com base nos critérios de informação.

GAIC(mbeinf1, mlogitSST, mSSTtr) ### AIC
GAIC(mbeinf1, mlogitSST, mSSTtr, k = log(nrow(lungFunction))) ### BIC
### O modelo logito SST produz menores valores de AIC e BIC, sendo preferível.

centiles.Inf0to1(mlogitSST, xvar = lungFunction$height, lwd = 2, col.centiles = c(3,4,5,6,7,6,5,4,3),
                 main = 'logit SST', legend = TRUE, xleg = 90, yleg = 0.67)
### Curvas centílicas produzidas pelo modelo logito SST.


predict(mlogitSST, 'xi1', type = 'response')
### Probabilidades preditas de FPE=1
