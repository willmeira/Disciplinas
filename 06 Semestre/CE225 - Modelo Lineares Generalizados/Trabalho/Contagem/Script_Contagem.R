library(ggplot2)
library(gridExtra)
library(corrplot)
library(readxl)

dados <- read_xls('Base_fim.xls',
                  col_types = c("text", "numeric", "numeric","numeric", "numeric",
                                "numeric","numeric", "numeric", "numeric"))

###################
# Análise Descritiva
###################

str(dados)
summary(dados)
head(dados)

# Quantidade de municípios por total de incidência
tabobt <- table(dados$obit)
tabpobt <- table(round(dados$obit/sum(dados$obit)*100, digits = 2))
X11()
par(mfrow=c(1,2))
barplot(tabobt)
barplot(tabpobt)

###################
# Boxplots
###################
x11()
par(mfrow=c(3,3))
boxplot(dados$obit, xlab = '', ylab = '', main = 'Óbitos no Trânsito', las=1)
boxplot(dados$vphab, xlab = '', ylab = '', main = 'Veículos a cada 100 Habit', las=1)
boxplot(dados$dens, xlab = '', ylab = '', main = 'Densidade Demográfica', las=1)
boxplot(dados$purb, xlab = '', ylab = '', main = '% Pop Urbana', las=1)
boxplot(dados$palf, xlab = '', ylab = '', main = '% Alfabetizados', las=1)
boxplot(dados$pdes, xlab = '', ylab = '', main = '% Desmpregados', las=1)
boxplot(dados$rmed, xlab = '', ylab = '', main = '% Pop Baixa Renda', las=1)
boxplot(dados$idh, xlab = '', ylab = '', main = 'IDH', las=1)

###################
# Histogramas
###################
x11()
g1 <- ggplot(dados, aes(x=obit)) + geom_histogram()+ xlab('Óbitos no Trânsito')+ ylab('')
g2 <- ggplot(dados, aes(x=vphab)) + geom_histogram()+ xlab('Veículos a cada 100 Habit')+ ylab('')
g3 <- ggplot(dados, aes(x=dens)) + geom_histogram()+ xlab('Densidade Demográfica')+ ylab('')
g4 <- ggplot(dados, aes(x=purb)) + geom_histogram()+ xlab('% Pop Urbana')+ ylab('')
g5 <- ggplot(dados, aes(x=palf)) + geom_histogram()+ xlab('% Alfabetizados')+ ylab('')
g6 <- ggplot(dados, aes(x=pdes)) + geom_histogram()+ xlab('% Desmpregados')+ ylab('')
g7 <- ggplot(dados, aes(x=rmed)) + geom_histogram()+ xlab('Renda Média')+ ylab('')
g8 <- ggplot(dados, aes(x=idh)) + geom_histogram()+ xlab('IDH')+ ylab('')
grid.arrange(g1, g2, g3, g4, g5, g6, g7, g8, ncol=3, nrow=3)

# Pelos boxplots e histogramas verificamos uma grande assimetria na covariável
# Densidade demografica. Vale a pena fazer uma transformação nesta variável, para 
# melhorar a assimetria aplicando log.

###################
# Aplicando Log
###################
dados$ldens  <- log(dados$dens)
x11()
par(mfrow = c(1,2))
hist(log(dados$dens), main = 'log(Densidade)', xlab = '', ylab = '')
boxplot(log(dados$dens), xlab = '', ylab = '', main = 'log(Densidade)')

# Aplicando log, verificamos uma melhora considerável na assimetria dos dados da variável

###################
# Correlação
###################

# Vamos verificar a correlação entre as variáveis em estudo, substituino a varíavel densidade 
# demográfica pela log densidade.

library(corrplot)
cor <- cor(dados[ , c(2,3,10,5,6,7,8,9)])
x11()
corrplot.mixed(cor, upper = "ellipse")

# O correlograma aponta três variáveis como mais correlacionadas com a resposta: 
# o log da densidade demográfica, renda média e percetual de população urbana. 
# Nota-se também que as varíaveis IDH e Renda Média são altamente relacionadas. Outras
# correlações que merecem destaque são log da Densidade com Renda Média, Log Densidade com IDH
##
## Testar VA'S OFFSET ****************


###################
# Gráficos de Dispersão
###################

library(car)
x11()
scatterplotMatrix(dados[ , c(2,3,10,5,6,7,8,9)], 
                  smooth = list(smoother=loessLine, spread=TRUE,
                                col.smooth = 2, # Cor da média do smooth
                                col.spread = 2, # Cor do ic do smooth
                                lty.smooth=1, lwd.smooth=1.5,
                                lty.spread=3, lwd.spread=1),
                  regLine = list(method=lm, col = 1), # Cor da regressão
                  col = 3) # Cor dos pontos

## >>> MELHORAR A INTERPRETAÇÃO <<<< Copia lineu
# A matriz de gráficos de dispersão evidencia ainda alta relação entre as variáveis 
# log da frota de veículos e o log da população; quanto à variável resposta nota-se 
# a presença de valores altos atípicos, distantes da nuvem de pontos.

###################
# Ajuste dos Modelos de Regressão
###################
library(gamlss)

# GLM com resposta Poisson

# Modelo Glm Poisson
m1.1<-glm(obit~vphab+log(dens)+purb+palf+pdes+rmed+idh, family=poisson(link='log'),data=dados)
summary(m1.1)
x11()
par(mfrow = c(2,2))
plot(m1.1)

# Modelo Gamlss Poisson
m1.2<-gamlss(obit~vphab+log(dens)+purb+palf+pdes+rmed+idh, family=PO, data=dados)
summary(m1.2) 
x11()
par(mfrow = c(2,2))
plot(m1.2)

# Modelo Poisson com inflação de zeros (mas sem covariáveis para o componente do excesso de zeros)
m1.3 <- gamlss(obit~vphab+log(dens)+purb+palf+pdes+rmed+idh,family = ZIP, data = dados) 
summary(m1.3)
x11()
par(mfrow = c(2,2))
plot(m1.3)

# Modelo Poisson com inflação de zeros (incluindo as covariáveis também na modelagem 
# do componente do excesso de zeros)
m1.4<-gamlss(obit~vphab+log(dens)+purb+palf+pdes+rmed+idh,
             sigma.formula = ~ vphab+log(dens)+purb+palf+pdes+rmed+idh, 
             family = ZIP, data = dados) 
summary(m1.4)
x11()
par(mfrow = c(2,2))
plot(m1.4)

# GLM com resposta Binomial Negativa

# Modelo Glm.nb
m2.1<-glm.nb(obit~vphab+log(dens)+purb+palf+pdes+rmed+idh, data=dados)
summary(m2.1)
x11()
par(mfrow = c(2,2))
plot(m2.1)

# Modelo Gamlss Binomial Negativa tipo I
m2.2<-gamlss(obit~vphab+log(dens)+purb+palf+pdes+rmed+idh, family=NBI, data=dados)
summary(m2.2)
x11()
par(mfrow = c(2,2))
plot(m2.2)

# Modelo Gamlss Binomial Negativa tipo II
m2.3<-gamlss(obit~vphab+log(dens)+purb+palf+pdes+rmed+idh, family=NBII, data=dados)
summary(m2.3)
x11()
par(mfrow = c(2,2))
plot(m2.3)

# Modelo Gamlss Familia Binomial Negativa
m2.4<-gamlss(obit~vphab+log(dens)+purb+palf+pdes+rmed+idh, family=NBF, data=dados)
summary(m2.4)
x11()
par(mfrow = c(2,2))
plot(m2.4)

# Modelo Binomial negativa com inflação de zeros (mas sem covariáveis para o componente do excesso de zeros)
m2.5<-gamlss(obit~vphab+log(dens)+purb+palf+pdes+rmed+idh,family = ZINBI, data = dados) 
summary(m2.5)
x11()
par(mfrow = c(2,2))
plot(m2.5) #ajuste interessante

# Modelo Binomial negativa com inflação de zeros (incluindo as covariáveis 
# também na modelagem do componente do excesso de zeros)
m2.6<-gamlss(obit~vphab+log(dens)+purb+palf+pdes+rmed+idh,
             nu.formula = ~ vphab+log(dens)+purb+palf+pdes+rmed+idh,
             family = ZINBI, data = dados) 
summary(m2.6)
x11()
par(mfrow = c(2,2))
plot(m2.6)


###################
# Escolha do Modelo
###################

### Vamos comparar os modelos pela deviance, AIC's e Log-verossimilhança.
Ajuste = c('Po_Glm-1.1','Po_Gaml-1.2','Po_Zip-1.3','Po_ZipIPCov-1.4',
           'Bn_Glm-2.1','Bn_TpI-2.2','Bn_TpII-2.3','Bn_Fam-2.4','Bn_Zinbi-2.5','Bn_ZinbiCOv-2.6')
Dev    = c(deviance(m1.1),deviance(m1.2),deviance(m1.3),deviance(m1.4),
           deviance(m2.1),deviance(m2.2),deviance(m2.3),
           deviance(m2.4),deviance(m2.5),deviance(m2.6))
AIC    = c(AIC(m1.1),AIC(m1.2),AIC(m1.3),AIC(m1.4),
           AIC(m2.1),AIC(m2.2),AIC(m2.3),
           AIC(m2.4),AIC(m2.5),AIC(m2.6))
MaxVer = c(logLik(m1.1),logLik(m1.2),logLik(m1.3),logLik(m1.4),
           logLik(m2.1),logLik(m2.2),logLik(m2.3),
           logLik(m2.4),logLik(m2.5),logLik(m2.6))

data.frame(Ajuste, Dev, AIC, MaxVer)

# O modelo que apresentou menor AIC e maior verossimilhança foi o modelo Binomial Negativo 
# da função Glm.bn

library(hnp)

# Avaliando os modelos Poisson

X11()
par(mfrow = c(2,1))
hnp(m1.1, xlab = 'Percentil da N(0,1)', ylab = 'Resíduos', main = 'Gráfico Normal de Probabilidades')
hnp(m1.2, xlab = 'Percentil da N(0,1)', ylab = 'Resíduos', main = 'Gráfico Normal de Probabilidades')
hnp(m1.3, xlab = 'Percentil da N(0,1)', ylab = 'Resíduos', main = 'Gráfico Normal de Probabilidades')
hnp(m1.4, xlab = 'Percentil da N(0,1)', ylab = 'Resíduos', main = 'Gráfico Normal de Probabilidades')

# Avaliando os modelos com Binomial Negativa
X11()
par(mfrow = c(3,2))
hnp(m2.1, xlab = 'Percentil da N(0,1)', ylab = 'Resíduos', main = 'Gráfico Normal de Probabilidades')
hnp(m2.2, xlab = 'Percentil da N(0,1)', ylab = 'Resíduos', main = 'Gráfico Normal de Probabilidades')
hnp(m2.3, xlab = 'Percentil da N(0,1)', ylab = 'Resíduos', main = 'Gráfico Normal de Probabilidades')
hnp(m2.4, xlab = 'Percentil da N(0,1)', ylab = 'Resíduos', main = 'Gráfico Normal de Probabilidades')
hnp(m2.5, xlab = 'Percentil da N(0,1)', ylab = 'Resíduos', main = 'Gráfico Normal de Probabilidades')
hnp(m2.6, xlab = 'Percentil da N(0,1)', ylab = 'Resíduos', main = 'Gráfico Normal de Probabilidades')

# >>>> ERRO NOS MODELOS AJUSTADOS PELO PACOTE GAMLSS <<<<<


X11()
par(mfrow = c(1,2))
hnp(m1.1, xlab = 'Percentil da N(0,1)', ylab = 'Resíduos', main = 'Gráfico Normal de Probabilidades')
hnp(m2.1, xlab = 'Percentil da N(0,1)', ylab = 'Resíduos', main = 'Gráfico Normal de Probabilidades')

# Nota-se pelas medidas de qualidade de ajuste e o comportamento dos resíduos no gráfico
# a total falta de aderência à distribuição de Poison; alterando a distribuição da
# resposta para Binomial Negativa obteve-se um ajuste satisfatório.

###################
# Modelo Escolhido
###################

### Vamos seguir as análises fazendo uso do modelo Binomial Negativo.

###################
# Resumo do Modelo
###################

# O modelo original foi ajustado usando todas as covariáveis disponíveis. 
# Vamos verificar no resumo do modelo selecionado quais covariáveis são 
# apontadas como significativas:

summary(m2.1)

# O resumo do modelo ajustado indica que as variáveis Log da Densidade Demográfica,
# Percentual de População Urbana, Percentual de Alfabetizados e IDH foram significativas. 
# No resumo é mostrado também o valor do parâmetro de dispersão (neste caso vale 1,001), 
# bem próximo de 1, >>>> o que explica a falta de ajuste à distribuição de Poisson.<<<<<

###################
# Reajuste do Modelo
###################

# Como há um par de covariáveis altamente correlacionadas (log da frota e 
# log da população), é válido inserir as covariáveis uma a uma no modelo 
# para verificar sua significância na presença das outras; tal como o realizado
# pelo algoritmo stepwise. Notou-se que ao retirar a variável log da frota, 
# o log da população se mostra significativo

m2.1.1 <- step(m2.1, direction = "both")

# O resumo do novo modelo ajustado:
summary(m2.1.1)

# O algoritmo indica que as variáveis >>> grau de urbanização e log da frota <<< 
# são significativas e tem relação positiva com o número de acidentes de trânsito.

# Agora, vamos realizar o teste da razão de verossimilhança do modelo inicial e do reduzido:
  
anova(m2.1, m2.1.1)

# O p-valor do teste foi relativamente alto, portanto pode-se concluir 
# que o modelo restrito se ajusta aos dados amostrais tão bem quanto 
# o modelo considerando todas as covariáveis. Portanto o modelo final 
# fica expresso por:

# y_i|x_i ~ Binomial Negativa

# log(µi)= XXXXXXXXXXXX


# Medidas de Influência 
# Uma alternativa para verificação de medidas influentes está implementada no pacote car:

library(car)

influenceIndexPlot(m2.1.1, vars=c("Cook", "Studentized", "hat"), main="Medidas de Influência")

# O primeiro gráfico apresenta os valores da distância de Cook para cada observação. 
# A distância de Cook é uma medida de diferença das estimativas dos parâmetros do 
# modelo ao considerar e ao desconsiderar uma particular observação no ajuste.

# O segundo gráfico mostra os resíduos studentizados; um modelo bem ajustado 
# apresenta estes resíduos dispersos aleatóriamente em torno de 0, entre -3 e 3 desvios.

# O terceiro gráfico mostra os valores da matriz chapéu (H). Valores elevados
# são considerados potencialmente influentes. Os valores da matriz chapéu 
# estão entre 0 e 1. A soma dos elementos da diagonal da matriz H equivale ao 
# posto da matriz X de delineamento.

# Com base nesses 3 gráficos, não há indicativos fortes de outliers ou observações influentes

###################
# Resíduos Quantílicos Aleatorizados
###################

# Outra alternativa para avaliar a qualidade do ajuste é baseada nos resíduos
# quantílicos aleatorizados. A função qresiduals do pacote statmod extrai este 
# tipo de resíduos do modelo

library(statmod)

res <- qresiduals(m2.1.1)
X11()
par(mfrow=c(1,2))
plot(res)

residuos <- qresiduals(m2.1.1)
qqnorm(residuos)
qqline(residuos, col = 2)

# No gráfico da esquerda nota-se que os resíduos estão dispersos predominantemente 
# em torno de 0 entre -2 e 2. Além disso, no gráfico a direita verifica-se que os 
# resíduos apresentam razoável aderência à distribuição Normal. Há um leve indício 
# de caudas pesadas; porém, no geral, parece que há um ajuste plausível.

###################
# Gráfico Normal de Probabilidades com Envelope Simulado
###################

# Vamos verificar o comportamento do gráfico Normal de probabilidades com envelope 
# simulado para o modelo reajustado:
x11()
hnp(m2.1.1, xlab = 'Percentil da N(0,1)', ylab = 'Resíduos', main = 'Gráfico Normal de Probabilidades')
## Negative binomial model (using MASS package)

# Os resíduos estão dispersos no interior dos envelopes simulados, 
# sem aparente padrão sistemático dando indício de que o modelo está bem ajustado.

###################
# Gráficos de Efeitos
###################

# A função effects, do pacote de mesmo nome, devolve os efeitos marginais de 
# cada variável de um modelo ajustado; os gráficos de efeitos nos fornecem 
# uma forma visual de observar como cada variável explicativa afeta a resposta, 
# com as demais variáveis fixadas na média.

library(effects)
plot(allEffects(m2.1.1), type = 'response', main = '')


###################
# Predição
###################

#*****************************************************************************************
#*****************************************************************************************







