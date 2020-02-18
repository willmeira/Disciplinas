########################################################################
### Aplicação 1 - Árvores de classificação. Vamos usar árvores de classificação
### para classificação de e-mails em spam ou não spam.

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

### Alguns gráficos
with(spam, boxplot(log(charExclamation + 0.1) ~ type, ylab = 'log(frequencia)',
                   col = 'lightblue', cex = 1.45, las = 1))
with(spam, boxplot(log(capitalTotal + 0.1) ~ type, ylab = 'log(frequencia)',
                   col = 'lightblue', cex = 1.45, las = 1))
### Aparentemente, as frequencias de pontos de exclamação ou letras maiúsculas
### são importantes para discriminar spams e não spams (spams têm maior
### frequencia de ambos).


########################################################################
### Parte 1 - Preparação dos dados.

### Vamos dividir a base em duas: uma para ajuste do modelo e outra para
### validação. A base de validação com 1000 observações e a de ajuste com
### as demais.

set.seed(88) ### Fixando a semente para sorteio das observações.
select <- sample(1:nrow(spam), 1000)
### Linhas da base que serão usadas para validação.

spam_ajuste <- spam[-select,] ### Base de ajuste.
spam_predict <- spam[select,] ### Base de validação.

########################################################################
### Parte 2 - Ajuste do modelo.

arvore1 <- rpart(type ~ ., data = spam_ajuste)
printcp(arvore1) ### Tabela de custo-complexidade.
plotcp(arvore1) ### Curva de custo-complexidade.
### Pela análise da curva de custo-complexidade, a árvore com nove nós
### finais é selecionada.

par(mar = c(4,8,2,2))
imp <- arvore1$variable.importance
barplot(imp, horiz = TRUE, las = 1, xlab = 'Importância')

### Fazendo a poda para obtenção da árvore com nove nós finais.
arvore2 <- prune.rpart(arvore1, cp = 0.011)
x11(width = 25, height = 15)
rpart.plot(arvore2, type = 3, extra = 1, cex = 1.2, uniform = FALSE)

### Agora, vamos dar sequencia com a predição dos dados na amostra de validação.
predmat <- predict(arvore2, newdata = spam_predict)
predmat
### As predições correspondem às proporções de spam e não spam nos nós.

pred <- predict(arvore2, newdata = spam_predict, type = 'class')
pred
### Cada e-mail da base de predição é classificado como spam, ou não spam
### com base na maioria de e-mails no correspondente nó.

### Tabela de confusão:
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

### Acurácia:
(561+341)/1000

### Vamos fazer a análise da curva ROC para avaliar a performance preditiva
### do modelo e pontos de corte alternativos a p=0.5 para a regra de classificação.

pred2 <- predmat[,2] 
### Probabilidades estimadas de spam.

### Usando os recursos do pacote pRoc.
predroc <- roc(spam_predict$type, pred2, percent = TRUE)
plot(predroc, print.thres = c(0.1, 0.5, 0.8, 0.9), print.thres.pattern.cex = 0.8)
coords(predroc, 'best') 
### A regra de classificação que melhor conjuga sensibilidade e especificidade 
### baseia-se em p=0.435. No entanto, a regra baseada em p = 0.5 é rigorosamente
### a mesma (por que?).

### Conforme visto na aula de regressção logística, custos de má-classificação
### podem ser incorporados na seleção da melhor regra de classificação.
### A função coords, por default, identifica o ponto de corte tal que a soma
### sensibilidade + especificidade seja máxima. Podemos modificar este critério
### através da função best werights, em que declaramos:

### 1 - O custo relativo de um falso negativo quando comparado a um falso
### positivo;

### 2 - A prevalência ou proporção de casos (spams) na população.

### Vamos considerar que a proporção de spams seja 1/3 e obter a regra de
### classificação ótima para diferentes custos.

coords(predroc, 'best', best.weights = c(0.5, 1/3)) 
coords(predroc, 'best', best.weights = c(1, 1/3)) 
coords(predroc, 'best', best.weights = c(2, 1/3)) 
coords(predroc, 'best', best.weights = c(10, 1/3)) 


########################################################################
### Aplicação 2 - Conditional inference trees

help("airquality")

### Regressão
airq <- subset(airquality, !is.na(Ozone))
airq2 <- airq[, 1:4]
scatterplotMatrix(airq2, cex = 1.5)

airct <- ctree(Ozone ~ ., data = airq2)
airct
plot(airct)
### A saída apresenta as partições, os números de observações por nós (n),
### as médias das observações em cada nó e as somas de quadrados de resíduos.

### Na especificação abaixo, não usamos correção de Bonferroni, de maneira
### que a significância de cada variável é usada como critério de parada.
control_2 <- ctree_control(testtype = 'Univariate')
airct2 <- ctree(Ozone ~ ., control = control_2,  data = airq2)
airct2

### Adotando nível de significância de 20%.
control_3 <- ctree_control(testtype = 'Univariate', alpha = 0.20)
airct3 <- ctree(Ozone ~ ., control = control_3,  data = airq2)
airct3
plot(airct3)


### Códigos para sumário de uma árvore de regressão via ctrees. 
sumarioArv <- function(arvore,dados){
    dados$No95 <- predict(arvore, type = 'node')
    dadosaux <- na.omit(dados)
    indicaNo <- which(!duplicated(dadosaux$No95))
    nos <- dadosaux$No95[indicaNo]
    dataPred <- dadosaux[indicaNo,]
    sumario <- cbind(nos,predict(arvore, newdata = dataPred, 
                                 FUN = function(y,w=1) c(mean(y), sd(y), min(y), quantile(y, c(0.05, 0.25, 0.50, 0.75, 0.95)), max(y))))
    colnames(sumario)[c(1:4, 10)] <- c('Nó', 'Média', 'DP', 'Mínimo', 'Máximo')
    sumario <- sumario[order(sumario[,'Nó']),]
    
    row.names(sumario) <- NULL
    sumario
}
sumarioArv(airct, airq2)

########################################################################
### Aplicação 3 - Um exemplo com dados censurados.


help(GBSG2)
stree <- ctree(Surv(time, cens) ~ ., data = GBSG2)
plot(stree)
stree

### Estimando o tempo mediano de sobrevida em cada nó.

s <- survfit(Surv(time, cens) ~ 1, data = GBSG2) ### Geral
s
plot(s)

names(stree)

x11()
par(mfrow = c(2,2), mar = c(4,4,2,2), las = 1)
s3 <- survfit(Surv(time, cens) ~ 1, data = GBSG2, subset = (n == pn[1])) 
s3
plot(s3, main = 'Nó 3')

s4 <- survfit(Surv(time, cens) ~ 1, data = GBSG2, subset = (n == pn[2])) 
s4
plot(s4, main = 'Nó 4')

s6 <- survfit(Surv(time, cens) ~ 1, data = GBSG2, subset = (n == pn[3])) 
s6
plot(s6, main = 'Nó 6')

s7 <- survfit(Surv(time, cens) ~ 1, data = GBSG2, subset = (n == pn[4])) 
s7
plot(s6, main = 'Nó 7')

