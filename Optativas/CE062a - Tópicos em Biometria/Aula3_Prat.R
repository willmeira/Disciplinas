########################################################################
########################################################################
########################################################################
### Teste sobre o efeito da terbutilazina, um herbicida, usada no cultivo de 
### Lemna minor, uma pequena espécie de planta aquática, realizada em uma 
### cultura asséptica de acordo com as diretrizes da OCDE. A resposta (rgr)
### representa a taxa de crescimento relativo da planta.

library("ggplot2")
library("drc")

data("terbuthylazin")
help("terbuthylazin")

########################################################################
### Visualização dos resultados
x11()
ggplot(terbuthylazin, aes(x = dose, y = rgr)) + geom_point()
### Matéria seca vs dose por tratamento.

ggplot(terbuthylazin, aes(x = I(log(dose)), y = rgr)) + geom_point()
### Considerando a dose na escala logarítmica, para melhor visualização.

########################################################################
### Ajuste dos modelos

options(digits = 8)

### Comecemos pelo modelo log-logístico de quatro parâmetros.
terb_m1 <- drm(rgr ~ dose, data = terbuthylazin, fct = LL.4())

coef(terb_m1) ### Estimativas dos parâmetros.
round(coef(terb_m1),5)

summary(terb_m1) 
### Resumo do ajuste. O parâmetro 'c', associado ao limite inferior, não
### difere significativamente de zero (p=0.9745). Vamos ajustar um novo 
### modelo fixando c=0.

terb_m2 <- drm(rgr ~ dose, data = terbuthylazin, fct = LL.4(fixed = c(NA, 0, NA, NA)))
summary(terb_m2) 

### Uma forma alternativa de ajustar o modelo com c=0, é usando a função
### LL.3 (modelo log-logístico de três parâmetros).

terb_m2_alt <- drm(rgr ~ dose, data = terbuthylazin, fct = LL.3())
summary(terb_m2_alt)

### Agora, vamos avaliar o ajuste do modelo log-logístico com o quinto parâmetro
### (f),  mas ainda fixando c=0.

terb_m3 <- drm(rgr ~ dose, data = terbuthylazin, fct = LL.5(fixed = c(NA, 0, NA, NA, NA)))
summary(terb_m3)

terb_m3_alt <- drm(rgr ~ dose, data = terbuthylazin, fct = LL.5(fixed = c(NA, NA, NA, NA, NA)))
summary(terb_m3_alt)

### Como não se rejeita a hipótese H0: f=0 (p=0.274), o modelo log-logístico 
### de cinco parâmetros não é necessário.

### Representações gráficas do ajuste
plot(terb_m2) ### Curva ajustada sobre os as médias, em cada dose.
plot(terb_m2, type = 'all') ### Curva ajustada sobre os dados amostrais.
plot(terb_m2, type="confidence") ### Curva ajustada com as bandas de confiança (95%.
plot(terb_m2, type="bars") ### Curva ajustada com os erros padrões.

plot(terb_m1) ### Curva ajustada sobre os as médias, em cada dose.
plot(terb_m1, type = 'all') ### Curva ajustada sobre os dados amostrais.
plot(terb_m1, type="confidence") ### Curva ajustada com as bandas de confiança (95%.
plot(terb_m1, type="bars") ### Curva ajustada com os erros padrões.

### Análise de resíduos
residuos <- residuals(terb_m2, typeRes = 'standardised')
fit <- fitted(terb_m2)

par(mfrow = c(1,2))
plot(residuos ~ fit, cex = 1.2, pch = 20)
qqnorm(residuos, pch = 20, cex = 1.2)
qqline(residuos)
### Há uma (talvez duas) observações mal ajustadas, possivelmente outliers.
### Essas observações poderiam ser investigadas.

### Intervalos de confiança(95%) para os parâmetros do modelo.
confint(terb_m2)

### Algumas predições. Vamos trabalhar com as seguintes doses: 5, 50 e 150.
new_doses <- data.frame(dose = c(5,50,150)) ### Doses usadas para predição.
predict(terb_m2, newdata = new_doses) ### Valores ajustados.
predict(terb_m2, se.fit = TRUE, newdata = new_doses) 
### Valores ajustados com os erros padrões.
predict(terb_m2, interval = 'confidence', newdata = new_doses) 
### Valores ajustados e ICs(95%) para a resposta média.
predict(terb_m2, interval = 'prediction', newdata = new_doses) 
### Valores ajustados e ICs(95%) para a predição de nova observação.

### Estimação de doses efetivas. Vamos estimar as doses para as seguintes
### respostas (relativas à resposta maximal): 5, 50 e 90.
ED(terb_m2, c(5, 10, 50), interval = "delta")

### Teste da falta de ajuste
modelFit(terb_m2)
### A hipótese nula, de que o modelo se ajusta bem aos dados, não é rejeitada. 

### Vamos ajustar e avaliar outros modelos.

### Modelos Weibull de três e quatro parâmetros
terb_wei_m3 <- drm(rgr ~ dose, data = terbuthylazin, fct = W1.3())
terb_wei_m4 <- drm(rgr ~ dose, data = terbuthylazin, fct = W1.4())

### Modelos Log-Normal de três e quatro parâmetros
terb_ln_m3 <- drm(rgr ~ dose, data = terbuthylazin, fct = LN.3())
terb_ln_m4 <- drm(rgr ~ dose, data = terbuthylazin, fct = LN.4())

### Vamos comparar os ajustes usando os critérios de informação.

AIC(terb_m2, terb_wei_m3, terb_wei_m4, terb_ln_m3, terb_ln_m4)
### O modelo log-normal de três parâmetros produziu menor valor de AIC
### (melhor ajuste), seguido pelo modelo log-logístico de três parâmetros.

BIC(terb_m2, terb_wei_m3, terb_wei_m4, terb_ln_m3, terb_ln_m4)
### A conclusão para o índice BIC, neste caso, é semelhante à produzida 
### pelo AIC.

### Podemos ajustar e comparar vários modelos, de maneira alternativa, usando
### a função mselect.
mselect(terb_m2, list(LL.4(), LL.5(), W1.3(), W1.4(), W2.4(), LN.3(), LN.4()))
### O modelo log-normal de três parâmetros produziu melhores indicadores.

### Voltando ao modelo log-logístico de três parâmetros, vamos verificar a
### necessidade de transformação dos dados usando o método de Box-Cox.

terb_m2_bc <- boxcox(drm(rgr ~ dose, data = terbuthylazin, fct = LL.4(fixed = c(NA, 0, NA, NA))))
summary(terb_m2_bc)
### O gráfico da log-verossimilhança perfilada indica que não há a necessidade
### de transformação. Na próxima aplicação, uma transformação será requerida. 


########################################################################
########################################################################
########################################################################
### Plantas da espécie Galium aparine, cultivadas em vasos em estufa, 
### foram pulverizadas com o herbicida fenmidipham, 
### isoladamente ou em mistura com um éster de ácido oleico. As plantas 
### foram deixadas a crescer na estufa durante 14 dias após o tratamento 
### com herbicida. Em seguida, a matéria seca foi medida por vaso.

data(G.aparine)
help(G.aparine)

### Visualização dos resultados
x11()
ggplot(G.aparine, aes(x = dose, y = drymatter)) + geom_point(aes(colour = treatment))
### Matéria seca vs dose por tratamento.

ggplot(G.aparine, aes(x = I(log(dose)), y = drymatter)) + geom_point(aes(colour = treatment))
### Considerando a dose na escala logarítmica, para melhor visualização.

ggplot(G.aparine, aes(x = I(log(dose)), y = drymatter)) + geom_point() + facet_grid(~treatment)
### Apresentando os resultados em janelas separadas.

########################################################################
### Ajuste dos modelos

### Comecemos pelo modelo log-logístico de quatro parâmetros.
G.aparine.m1 <- drm(drymatter ~ dose, curveid = treatment, data = G.aparine, fct = LL.4())
S.aparine.m1 <- summary(G.aparine.m1)
S.aparine.m1
par(mfrow = c(1,2))
plot(residuals(G.aparine.m1, type = 'standard') ~ fitted(G.aparine.m1), cex = 1.2, pch = 20)
qqnorm(residuals(G.aparine.m1, type = 'standard'), pch = 20, cex = 1.2)
qqline(residuals(G.aparine.m1, type = 'standard'))
### Os gráficos de resíduos claramente apontam problemas no ajuste, com
### variância não homogênea e alguma não normalidade.

### Vamos usar o método Box-Cox, para encontrar uma transformação que 
### contorne os problemas de ajuste detectados na análise de resíduos.
### Neste caso, usamos a estratégia de transformar ambos os lados do 
### modelo (tanto a resposta quanto o preditor não linear).

G.aparine.m2 <- boxcox(G.aparine.m1)
G.aparine.m2
### A análise indica uma transformação do tipo potência com lambda=0.25.

### Seguimos com o diagnóstico do ajuste baseado nos resíduos.
par(mfrow = c(1,2))
plot(residuals(G.aparine.m2, type = 'standard') ~ fitted(G.aparine.m1), cex = 1.2, pch = 20)
qqnorm(residuals(G.aparine.m2, type = 'standard'), pch = 20, cex = 1.2)
qqline(residuals(G.aparine.m2, type = 'standard'))
### Evidentemente o modelo está mais bem ajustado que o anterior. 

modelFit(G.aparine.m2)
### Não há evidências de falta de ajuste.

### Vamos comparar os resultados dos dois ajustes (estimativas e erros padrões)
S.aparine.m2 <- summary(G.aparine.m2)
S.aparine.m2

data_result <- data.frame(EstM1 = S.aparine.m1$coefficients[,'Estimate'],
                          SeM1 = S.aparine.m1$coefficients[,'Std. Error'],
                          EstM2 = S.aparine.m2$coefficients[,'Estimate'],
                          SeM2 = S.aparine.m2$coefficients[,'Std. Error'])
data_result
### Pode-se observar variações nas estimativas e, sobretudo, em alguns
### erros padrões.

### Usando a mesma transformação, vamos avaliar os ajustes de outros modelos.
m1 <- mselect(G.aparine.m2, list(LL.4(), LL.5(), W1.3(), W1.4(), W2.4(), LN.3(), LN.4()))
### O modelo Weibull de quatro parâmetros produziu menor valor de AIC.

G.aparine.m3 <- drm(drymatter ~ dose, curveid = treatment, data = G.aparine, 
                    fct = W1.4(), bcVal = 0.25)
AIC(G.aparine.m3)
summary(G.aparine.m3)
### Vamos seguir a análise com este modelo.

### Representações gráficas do ajuste.
plot(G.aparine.m3) ### Curva ajustada sobre os as médias, em cada dose.
plot(G.aparine.m3, type = 'all') ### Curva ajustada sobre os dados amostrais.
plot(G.aparine.m3, type="bars") ### Curva ajustada com os erros padrões.

### Vamos investigar se podemos ter um mesmo 'b' para as duas curvas.
G.aparine.m3_b <- drm(drymatter ~ dose, curveid = treatment, data = G.aparine, 
                      pmodels = data.frame(b = 1, c = treatment, d = treatment, e = treatment), 
                      fct = W1.4(), bcVal = 0.25)
anova(G.aparine.m3, G.aparine.m3_b)
### Não há diferença significativa no ajuste dos dois modelos. Ficamos
### com o modelo mais simples, em que b é o mesmo nas duas curvas.

plot(G.aparine.m3_b)
summary(G.aparine.m3_b)

### Vamos investigar agora se podemos também fixar um mesmo 'd' para as duas curvas.
G.aparine.m3_bd <- drm(drymatter ~ dose, curveid = treatment, data = G.aparine, 
                       pmodels = data.frame(b = 1, c = treatment, d = 1, e = treatment), 
                       fct = W1.4(), bcVal = 0.25)
anova(G.aparine.m3_b, G.aparine.m3_bd)
### Neste caso, a diferença dos dois ajustes é significativa. Logo, não podemos
### usar o mesmo 'd' nas duas curvas de dose-resposta.

### Estimação de doses efetivas
ED(G.aparine.m3_b, c(5, 10, 50), interval = "delta")

### Vamos calcular a potência relativa. Primeiro para o modelo final, em
### que o parâmetro de inclinação relativa é o mesmo para as duas curvas.
EDcomp(G.aparine.m3_b, percVec = c(50,50)) ### PR(50)
EDcomp(G.aparine.m3_b, percVec = c(90,90)) ### PR(90)
### O fato da inclinação relativa ser a mesma para as duas curvas implica
### que a potência relativa é a mesma, para qualquer dose efetiva.

### Vamos retomar o modelo em que as duas curvas têm inclinações diferentes.
EDcomp(G.aparine.m3, percVec = c(50,50))
EDcomp(G.aparine.m3, percVec = c(90,90))
### Observe que agora a potência relativa varia conforme a dose efetiva considerada.