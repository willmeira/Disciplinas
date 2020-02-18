########################################################################
### Aplica��o 1 - �rvores de classifica��o. Vamos usar �rvores de classifica��o
### para classifica��o de e-mails em spam ou n�o spam.

require(kernlab)
require(rpart)
require(rpart.plot)
require(pROC)
require(partykit)
require(car)
require(TH.data)
require(survival)

data(spam)
help(spam)
head(spam)
dim(spam)
summary(spam)

### Alguns gr�ficos
with(spam, boxplot(log(charExclamation + 0.1) ~ type, ylab = 'log(frequencia)',
                   col = 'lightblue', cex = 1.45, las = 1))
with(spam, boxplot(log(capitalTotal + 0.1) ~ type, ylab = 'log(frequencia)',
                   col = 'lightblue', cex = 1.45, las = 1))
### Aparentemente, as frequencias de pontos de exclama��o ou letras mai�sculas
### s�o importantes para discriminar spams e n�o spams (spams t�m maior
### frequencia de ambos).


########################################################################
### Parte 1 - Prepara��o dos dados.

### Vamos dividir a base em duas: uma para ajuste do modelo e outra para
### valida��o. A base de valida��o com 1000 observa��es e a de ajuste com
### as demais.

set.seed(88) ### Fixando a semente para sorteio das observa��es.
select <- sample(1:nrow(spam), 1000)
### Linhas da base que ser�o usadas para valida��o.

spam_ajuste <- spam[-select,] ### Base de ajuste.
spam_predict <- spam[select,] ### Base de valida��o.

########################################################################
### Parte 2 - Ajuste do modelo.

arvore1 <- rpart(type ~ ., data = spam_ajuste)
printcp(arvore1) ### Tabela de custo-complexidade.
plotcp(arvore1) ### Curva de custo-complexidade.
### Pela an�lise da curva de custo-complexidade, a �rvore com nove n�s
### finais � selecionada.

par(mar = c(4,8,2,2))
imp <- arvore1$variable.importance
barplot(imp, horiz = TRUE, las = 1, xlab = 'Import�ncia')

### Fazendo a poda para obten��o da �rvore com nove n�s finais.
arvore2 <- prune.rpart(arvore1, cp = 0.011)
x11(width = 25, height = 15)
rpart.plot(arvore2, type = 3, extra = 1, cex = 1.2, uniform = FALSE)

### Agora, vamos dar sequencia com a predi��o dos dados na amostra de valida��o.
predmat <- predict(arvore2, newdata = spam_predict)
predmat
### As predi��es correspondem �s propor��es de spam e n�o spam nos n�s.

pred <- predict(arvore2, newdata = spam_predict, type = 'class')
pred
### Cada e-mail da base de predi��o � classificado como spam, ou n�o spam
### com base na maioria de e-mails no correspondente n�.

### Tabela de confus�o:
tabcruz <- table(pred, spam_predict$type)
tabcruz

### Sensibilidade:
341/(341+49)

### Especificidade:
561/(561+49)

### Poder preditivo positivo
341/(341+49)

### Poder preditivo negativo
561/(561+49)

### Acur�cia:
(561+341)/1000

### Vamos fazer a an�lise da curva ROC para avaliar a performance preditiva
### do modelo e pontos de corte alternativos a p=0.5 para a regra de classifica��o.

pred2 <- predmat[,2] 
### Probabilidades estimadas de spam.

### Usando os recursos do pacote pRoc.
predroc <- roc(spam_predict$type, pred2, percent = TRUE)
plot(predroc, print.thres = c(0.1, 0.5, 0.8, 0.9), print.thres.pattern.cex = 0.8)
coords(predroc, 'best') 
### A regra de classifica��o que melhor conjuga sensibilidade e especificidade 
### baseia-se em p=0.435. No entanto, a regra baseada em p = 0.5 � rigorosamente
### a mesma (por que?).

### Conforme visto na aula de regress��o log�stica, custos de m�-classifica��o
### podem ser incorporados na sele��o da melhor regra de classifica��o.
### A fun��o coords, por default, identifica o ponto de corte tal que a soma
### sensibilidade + especificidade seja m�xima. Podemos modificar este crit�rio
### atrav�s da fun��o best werights, em que declaramos:

### 1 - O custo relativo de um falso negativo quando comparado a um falso
### positivo;

### 2 - A preval�ncia ou propor��o de casos (spams) na popula��o.

### Vamos considerar que a propor��o de spams seja 1/3 e obter a regra de
### classifica��o �tima para diferentes custos.

coords(predroc, 'best', best.weights = c(0.5, 1/3)) 
coords(predroc, 'best', best.weights = c(1, 1/3)) 
coords(predroc, 'best', best.weights = c(2, 1/3)) 
coords(predroc, 'best', best.weights = c(10, 1/3)) 


########################################################################
### Aplica��o 2 - Conditional inference trees

help("airquality")

### Regress�o
airq <- subset(airquality, !is.na(Ozone))
airq2 <- airq[, 1:4]
scatterplotMatrix(airq2, cex = 1.5)

airct <- ctree(Ozone ~ ., data = airq2)
airct
plot(airct)
### A sa�da apresenta as parti��es, os n�meros de observa��es por n�s (n),
### as m�dias das observa��es em cada n� e as somas de quadrados de res�duos.

### Na especifica��o abaixo, n�o usamos corre��o de Bonferroni, de maneira
### que a signific�ncia de cada vari�vel � usada como crit�rio de parada.
control_2 <- ctree_control(testtype = 'Univariate')
airct2 <- ctree(Ozone ~ ., control = control_2,  data = airq2)
airct2

### Adotando n�vel de signific�ncia de 20%.
control_3 <- ctree_control(testtype = 'Univariate', alpha = 0.20)
airct3 <- ctree(Ozone ~ ., control = control_3,  data = airq2)
airct3
plot(airct3)


### C�digos para sum�rio de uma �rvore de regress�o via ctrees. 
sumarioArv <- function(arvore,dados){
    dados$No95 <- predict(arvore, type = 'node')
    dadosaux <- na.omit(dados)
    indicaNo <- which(!duplicated(dadosaux$No95))
    nos <- dadosaux$No95[indicaNo]
    dataPred <- dadosaux[indicaNo,]
    sumario <- cbind(nos,predict(arvore, newdata = dataPred, 
                                 FUN = function(y,w=1) c(mean(y), sd(y), min(y), quantile(y, c(0.05, 0.25, 0.50, 0.75, 0.95)), max(y))))
    colnames(sumario)[c(1:4, 10)] <- c('N�', 'M�dia', 'DP', 'M�nimo', 'M�ximo')
    sumario <- sumario[order(sumario[,'N�']),]
    
    row.names(sumario) <- NULL
    sumario
}
sumarioArv(airct, airq2)

########################################################################
### Aplica��o 3 - Um exemplo com dados censurados.


help(GBSG2)
stree <- ctree(Surv(time, cens) ~ ., data = GBSG2)
plot(stree)
stree

### Estimando o tempo mediano de sobrevida em cada n�.

s <- survfit(Surv(time, cens) ~ 1, data = GBSG2) ### Geral
s
plot(s)

names(stree)

x11()
par(mfrow = c(2,2), mar = c(4,4,2,2), las = 1)
s3 <- survfit(Surv(time, cens) ~ 1, data = GBSG2, subset = (n == pn[1])) 
s3
plot(s3, main = 'N� 3')

s4 <- survfit(Surv(time, cens) ~ 1, data = GBSG2, subset = (n == pn[2])) 
s4
plot(s4, main = 'N� 4')

s6 <- survfit(Surv(time, cens) ~ 1, data = GBSG2, subset = (n == pn[3])) 
s6
plot(s6, main = 'N� 6')

s7 <- survfit(Surv(time, cens) ~ 1, data = GBSG2, subset = (n == pn[4])) 
s7
plot(s6, main = 'N� 7')

