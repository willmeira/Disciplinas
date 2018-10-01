require(effects)
require(car)

### Exemplo - Análise de reação química (Modelo com erros normais)

### Leitura e descritiva dos dados
dados <- read.table('https://docs.ufpr.br/~taconeli/CE22517/Normal1.txt',header=T)
names(dados) <- c('Viscosidade','Temperatura','Alimentação')
summary(dados)
plot(dados, pch=20, cex=1.5)
### Observando os gráficos, notamos indícios de existência de uma tendência 
### crescente da viscosidade com relação às outras duas variáveis 
### (sobretudo quanto à temperatura).


### Ajuste do modelo de regressão linear com as duas variáveis explicativas 

ajuste1 <- lm(Viscosidade~Temperatura+Alimentação,data=dados) 
### A função lm ajusta modelos lineares (neste caso, um modelo de regressão 
### linear com duas variáveis explicativas).
summary(ajuste1) 
model.matrix(ajuste1) ### Matriz do modelo.

### Por meio do summary, obtemos as estimativas (e os respectivos erros) 
### associados a cada parâmetro do modelo, suas significâncias, além de
### indicadores de qualidade do ajuste como os coeficientes de determinação 
### ajustado e não ajustado e o teste F para significância do modelo. 
### Pode-se atestar, pelo teste F, a significância do modelo, que é capaz 
### de explicar 92,7% da variação dos dados.
### Além disso, as evidências de tendências crescentes destacadas na análise descritiva 
### são aqui confirmadas com base nas significâncias
### dos coeficientes do modelo (p<0,001, para a temperatura, e p=0,003, 
### para a taxa de alimentação).
 
names(ajuste1) ### Retorna a relação dos resultados produzidos pela função lm. 
### Para mais informações, consultar a documentação - help('lm')

ajuste2 <- lm(Viscosidade ~ scale(Temperatura,scale=F) + scale(Alimentação,scale=F), data=dados) 
### Ajuste alternativo, 'centrando' as variáveis explicativas,
### ou seja, diminuindo de cada observação das variáveis explicativas a respectiva média.

summary(ajuste2)
### O que mudou?

### Diagnóstico do modelo

ajuste1$residuals # Vetor de resíduos ordinarios
residuos <- rstandard(ajuste1);residuos # Vetor de resíduos padronizados
rstudent(ajuste1) # Vetor de resíduos studentizados

preditos <- ajuste1$fitted.values # Vetor de valores ajustados pelo modelo.

plot(preditos, residuos,xlab='Valores ajustados', ylab='Resíduos', pch=20, cex=1.5)
### Há algum indício nos resíduos de variância não constante (a dispersão 
### aumenta conforme a viscosidade). Como poderíamos remediar (ou melhor avaliar) isso?

par(mfrow=c(1,2))
hist(residuos)
qqnorm(residuos)
qqline(residuos)
### Investigar normalidade.
shapiro.test(residuos) 
### Teste de normalidade para os resíduos. Não há evidências de não normalidade.

influence.measures(ajuste1)
### Medidas de influência. As unidades 4 e 7 são destacadas como possíveis observações influentes. 

summary(influence.measures(ajuste1))
### Ambas as observações são destacadas por com relação à medida COVR, 
### ou seja, pela alteração na precisão das estimativas. 
### Vamos ver como ficariam os resultados eliminando tais observações.

summary(ajuste1)
summary(update(ajuste1, subset = -4))
summary(update(ajuste1, subset = -7))
summary(update(ajuste1, subset = -c(4,7)))

### A eliminação das observações 4 e 7 não altera de forma substancial as 
### estimativas e não muda as inferências do modelo.

### Intervalos de confiança para os parâmetros do modelo.
confint(ajuste1)

### Algumas predições

predict(ajuste1) ### Viscosidade estimada para cada observação na base.

predict(ajuste1, interval = 'confidence') 
### Viscosidade estimada e intervalo de confiança (95%) para a viscosidade
### média.

predict(ajuste1, interval = 'prediction')
### Viscosidade estimada e intervalo de predição (95%) para a viscosidade
### de uma nova observação.

### Agora, vamos repetir os códigos considerando um par de valores diferente
### dos que aparecem na base.
predict(ajuste1, newdata = data.frame(Temperatura = 95, Alimentação = 11))
predict(ajuste1, newdata = data.frame(Temperatura = 95, Alimentação = 11),
        interval = 'confidence')
predict(ajuste1, newdata = data.frame(Temperatura = 95, Alimentação = 11),
        interval = 'prediction')

### Gráficos para a viscosidade estimada com bandas de confiança. 
plot(effect("Temperatura", ajuste1))
### Viscosidade estimada versus temperatura (alimentação fixada na média)

plot(effect("Alimentação", ajuste1))
### Viscosidade estimada versus alimentação (temperatura fixada na média)

plot(effect("Alimentação", given.values = c(Temperatura = 85), ajuste1))
### Viscosidade estimada versus alimentação (temperatura fixada em 85)

plot(effect("Temperatura", given.values = c(Alimentação = 12), ajuste1))
### Viscosidade estimada versus temperatura (alimentação fixada em 12)


########################################################################
### Exemplo 2 - vendas segundo o tipo de embalagem.

Embalagem <- rep(c('E1','E2','E3','E4'), each=3)
Vendas <- c(11,18,13,14,10,12,19,17,21,24,30,27)

# Descrição dos dados
stripchart(Vendas ~ Embalagem, vertical=T, pch=20) # Gráfico de vendas vs embalagem.
### Há fortes indícios de diferença nas vendas conforme o tipo de embalagem.

tapply(Vendas, Embalagem, mean) # Vendas médias por tipo de embalagem.
tapply(Vendas, Embalagem, sd) # Desvios padrões das vendas por tipo de embalagem.

### Ajuste do modelo de análise de variância com um fator

modelo1 <- lm(Vendas ~ Embalagem)
anova(modelo1) 
### A análise de variância confirma as evidências anteriores 
### de que as vendas, em média, variam conforme o tipo de embalagem. 
summary(modelo1) 
### Repare que nessa parametrização o intercepro corresponde 
### à média para a embalagem 1 e os demais parâmetros às diferenças das médias
### para as embalagens 2, 3 e 4 em relação à média da embalagem 1.

### Os resultados indicam que as embalagens 3 e 4 proporcionam maior venda 
### média do que a embalagem 1, não havendo diferença entre as 
### vendas médias proporcionadas pelas embalagens 1 e 2.
### Nota - se desejado, poderia prosseguir com a execução de algum teste 
### de comparações múltiplas.

names(modelo1)
model.matrix(modelo1) # Matriz do modelo (X).

### Ajuste do modelo sem intercepto (parametrização alternativa)

modelo2 <- lm(Vendas ~ Embalagem-1)
summary(modelo2) 
### Agora, os quatro parâmetros correspondem às vendas médias das quatro embalagens. 

### Diagnóstico do modelo

residuos <- rstandard(modelo1) # Vetor de resíduos
preditos <- modelo1$fitted.values # Vetor de valores ajustados

plot(preditos,residuos,pch=20,xlab='Valores ajustados',ylab='Resíduos',cex=1.5)
### Investigar heterocedasticidade, observações mal-ajustadas (outliers, pontos influentes). 
### Não há qualquer indício de problemas no ajuste.

par(mfrow=c(1,2))
hist(residuos)
qqnorm(residuos)
qqline(residuos)
### Sem problemas quanto ao pressuposto de normalidade.

shapiro.test(residuos) 
### Teste de normalidade. A hipótese nula de normalidade não é rejeitada.
bartlett.test(residuos~Embalagem) 
### Teste de igualdade de variâncias. A hipótese nula de igualdade não é rejeitada.

### Intervalos de confiança sob as duas parametrizações.
confint(modelo1)
confint(modelo2)

### Vamos testar algumas hipóteses referentes a combinações lineares dos parâmetros.
linearHypothesis(modelo2, "1*EmbalagemE4 - 1*EmbalagemE3 = 0")
### Testando a igualdade de médias das embalagens 3 e 4.

linearHypothesis(modelo2, "1*EmbalagemE3 - 0.5*EmbalagemE4 - 0.5*EmbalagemE2 = 0")
### Testando a igualdade da embalagem 3 e da média das embalagens 4 e 2.







