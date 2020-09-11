require(gamlss)
require(gamlss.demo)
require(gamlss.util)
require(faraway)
require(boot)

options(device = 'x11')

########################################################################
### Parte 1
### Vamos usar a base de dados abdom. O objetivo é modelar a circunferência
### abdominal em função da idade gestacional. Pelo gráfico, condicional à
### idade gestacional a circunferência abdominal aparenta ter distribuição
### simétrica. No entanto, fica evidente que a dispersão não é constante,
### aumentando conforme a idade gestacional.

data(abdom)
help(abdom)

options(device = 'x11')
par(cex = 1.25, las = 1, mar = c(5,4,2,2))
plot(y ~ x, data = abdom, col = 'blue', xlab = 'Idade gestacional', 
     ylab = 'Circunferência abdominal')

### Vamos começar pelo ajuste de um modelo com resposta normal, inserindo
### a covariável (idade gestacional) apenas no preditor da média.

ajusteNO1 <- gamlss(y ~ x, data = abdom, family = NO)
plot(ajusteNO1)

plot(resid(ajusteNO1) ~ fitted(ajusteNO1), col = 'blue')
lines(lowess(resid(ajusteNO1) ~ fitted(ajusteNO1)), col = 'red', lwd = 2)
### Resíduos versus valores ajustados.

wp(ajusteNO1, cex = 1.25, ylim.all = 3)
### Worm plot.

### Os resíduos não apresentam distribuição normal e não têm variância 
### constante. Além disso, o gráfico de resíduos versus valores ajustados
### apresenta certa curvatura. Vamos tentar corrigir esses problemas: 
# (I) Modelando também o parâmetro de escala (sigma) em função da covariável;
# (II) Modelando a média da distribuição por meio de um polinômio quadrático.

ajusteNO2 <- gamlss(y ~ x + I(x^2), sigma.formula =~x, data = abdom, family = NO)
plot(ajusteNO2)
wp(ajusteNO2, cex = 1.25, ylim.all = 1.2)

### Alguns dos problemas diagnosticados no primeiro ajuste foram corrigidos
### (variância não constante e tendência nos resíduos). No entanto, o worm plot
### indica que o modelo ainda apresenta problemas de ajuste. 

### O próximo passo é trocar a distribuição. Ao invés da Normal vamos considerar
### a Power Exponential (PE).
### A distribuição PE tem três parâmetros: mu é a média, sigma é um parâmetro
### de escala e nu controla a curtose. A distribuição PE tem como casos 
### particulares a distribuição de Laplace (quando nu = 1), a normal
### (quando nu = 2) e a uniforme (quando nu vai a infinito). 

demo.PE()

### Vamos modelar a média (mu) e a dispersão (sigma) com base na covariável. 
### Neste primeiro momento, não vamos inserir termos não paramétricos (suavizadores)
### ao modelo. Vamos analisar o perfil da deviance para nu. Começamos pelo ajuste
### de um modelo com resposta normal, para comparação.


### Agora, o ajuste do modelo com resposta PE.
ajustePE <- gamlss(y ~ poly(x, 2, raw = TRUE), sigma.formula =~x, data = abdom, family = PE)
summary(ajustePE)
plot(ajustePE)
wp(ajustePE, cex = 1.25, ylim.all = 1)

GAIC(ajusteNO2, ajustePE, k = 2)
### O modelo com resposta PE produz menor AIC. Vamos prosseguir com ele. 

exp(coef(ajustePE, parameter = 'nu')) ### Estimativa de nu.

### Vamos ver diferentes formas de extrair erros padrões para as estimativas
### dos parâmetros de regressão.

options(scipen = 3)
V <- vcov(ajustePE) 
V
### Matriz de variâncias e covariâncias. Da linha 1 à linha 3 temos os
### valores referentes ao parâmetro de locação (mu); nas linhas 4 e 5 
### aqueles referentes a sigma (escala). Na linha 6, finalmente, a estimativa
### referente ao parâmetro de forma (nu).

vars <- diag(V) ### Variâncias assintóticas dos beta's chapéus.
sqrt(vars) ### Erros padrões assintóticos.
summary(ajustePE) 
### Observe que são os mesmos valores retornados pelo summary.

summary(ajustePE, robust = TRUE) 
### Resumo do ajuste com erros padrões robustos.

solve(V)
### Matriz Hessiana (informação).

vcov(ajustePE, type = 'se') 
### Forma alternativa de extrair os erros padrões.

vcov(ajustePE, type = 'se', robust = TRUE) 
### Erros padrões robustos.

Cors <- vcov(ajustePE, type = 'cor'); Cors 
### Matriz de correlações.


### A função prof.dev produz o gráfico do perfil da deviance para algum
### parâmetro do modelo (mu, sigma, nu ou tau) para o qual não foi incluida
### qualquer variável explicativa (ou seja, para o qual será estimado 
### apenas o intercepto).

par(cex = 1.2, las = 1)
perf_dev <- prof.dev(ajustePE, 'nu', min = 0.75, max = 5)
### Perfil da deviance para nu. As linhas tracejadas indicam a estimativa
### de máxima verossimilhança e os limites de confiança (95%) para mu.

perf_dev$CI
### Limites de 95% baseados no perfil da verossimilhança. Vamos ver como
### ficaria usando um intervalo do tipo Wald.

summ <- summary(ajustePE)
c(exp(summ[6,1] - 1.96 * summ[6,1]), exp(summ[6,1] + 1.96 * summ[6,1]))

### Repare que o método de Wald produz IC bastante diferente daquele obtido
### pelo perfil da deviance. Neste caso, em que o perfil da deviance é
### bastante assimétrico, o intervalo de Wald não é recomendável.

### Por default, a função prof.dev avalia o perfil da deviance em apenas 
### sete pontos de nu. 

perf_dev$values
### Vamos refinar, aumentando o número de pontos, para uma melhor visualização.
### Neste caso estamos aumentando para 20 pontos.

perf_dev <- prof.dev(ajustePE, 'nu', min = 0.75, max = 5, length = 20)

### Vamos visualizar o ajuste.
plotSimpleGamlss(y, x, model = ajustePE, data=abdom, x.val=seq(15, 40, 5), 
                 ylim=c(0, 450), xlim=c(5, 45))

### Agora vamos explorar o uso da função prof.term que, diferentemente da
### função prof.dev, pode ser aplicada a qualquer parâmetro no preditor
### de mu, sigma, nu ou tau (e não apenas a uma constante (intercepto)).

modPE <- quote(gamlss(y ~ poly(x, 2, raw = TRUE), sigma.formula =~ offset(this*x), data = abdom, family = PE))
### Expressão do modelo.

prof.term(modPE, min = 0.01, max = 0.1, length = 12)
### Perfil da deviance e IC(95%) para sigma.

prof.term(modPE, min = 0.01, max = 0.1, length = 12, perc = 99)
### Perfil da deviance e IC(99%) para sigma.

### Mais adiante vamos explorar usos alternativos e recursos adicionais 
### dessas funções.

