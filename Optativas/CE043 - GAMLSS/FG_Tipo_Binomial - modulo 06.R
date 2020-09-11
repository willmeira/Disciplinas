################################################################################
################################################################################
################################################################################
### Distribuições discretas tipo binomial

library(gamlss)
library(gamlss.demo)
options(device = 'x11')

### Distribuições discretas tipo binomial implementadas no pacote gamlss

gamlss.demo()
### Experimente algumas distribuições, verifique quantos são os parâmetros, o que 
### eles controlam...

################################################################################
### Exemplo 1: Número de itens não conformes (def) em lotes de tamanho
### n ispecionados em uma produção.

def <- c(1,8,1,13,18,0,3,13,3,2,20,2,5,3,0,14,0,0,1,19)
n <- c(50,48,50,49,50,48,50,50,47,49,50,50,50,50,50,50,48,49,50,49)

y <- cbind(def, n-def) ### Forma de entrada dos dados.

### Começamos pelo ajuste da distribuição binomial.
BI_fit <- gamlss(y~1, family = BI)
plot(BI_fit)
wp(BI_fit, ylim.all = 5)

### A distribuição binomial claramente não ajusta bem os dados. Isso pode ter como
### causa, por exemplo, uma probabilidade não constante de peças defeituosas nos
### diferentes lotes, ou dependência nos resultados de lotes distintos (ambos produziriam
### super dispersão). Vamos tentar a distribuição beta binomial.

BB_fit <- gamlss(y~1, family = BB)
plot(BB_fit)
wp(BB_fit)

GAIC(BI_fit, BB_fit, k = 2)

### Claramente a distribuição beta binomial proporciona melhor ajuste aos dados.

################################################################################
### Exemplo 2: Números de reprovações (r) e aprovações (n-r) de alunos do primeiro
### ano de um curso superior. Todos os alunos se matricularam em oito disciplinas.

n <- 8
ap <- 0:8
y <- cbind(ap, n-ap)
y
### y armazena os nove possíveis resultados para os pares número de aprovações (ap)
### número de reprovações (n-ap). 

freq <- c(1,4,4,8,9,6,8,12,13)
### freq representa o número de alunos para cada resultado. Assim, um aluno reprovou
### em todas as disciplinas, 4 alunos aprovaram em uma disciplina e reprovaram nas
### outras sete, e assim por diante.

### Vamos dispor os dados em um data frame para ficar mais claro.
colnames(y) <- c('Aprov', 'Reprov')
dados_alunos <- data.frame(y, freq)

### Agora, usando a função chooseDist vamos ajustar as diferentes distribuições para
### dados binários. Começamos pelo ajuste da distribuição binomial.

BI_fit2 <- gamlss(y~1, weights = freq, family = BI, data = dados_alunos)
### Observe que, para evitar ter que entrar com os resultados individuais de cada
### aluno, atribuimos pesos, o que tem resultado equivalente.

ch_fit2 <- chooseDist(BI_fit2, type = 'binom', k = 2)
ch_fit2
### Observe que, dentre as distribuições consideradas, a beta-binomial produziu 
### menor valor de AIC, sendo a escolhida.

BB_fit2 <- gamlss(y~1, weights = freq, family = BB, data = dados_alunos)

### Vamos comparar as frequências observadas com as esperadas, ajustadas pelos
### modelos BI e BB.

dados_alunos$BI_pred <- round(65 * dBI(0:8, bd = 8, mu = fitted(BI_fit2)[1]), 1)
dados_alunos$BB_pred <- round(65 * dBB(0:8, bd = 8, mu = fitted(BB_fit2, what = 'mu')[1],
                                       sigma = fitted(BB_fit2, what = 'sigma')[1]), 1)
dados_alunos
### As frequências ajustadas pela distribuição beta binomial são bem mais próximas
### das observadas em relação às produzidas pela binomial.

################################################################################
### Exemplo 3: Efeito de doses crescentes de uma droga na proporção de fetos de
### ratos afetados.

require(aod)
data(rabbits)
help(rabbits)

rabbits$group <- factor(rabbits$group, levels = c('C', 'L', 'M', 'H'), labels = c('Control', 'Low', 'Medium', 'High'))

### Começamos pelo ajuste do modelo com resposta binomial
ajuste1 <- gamlss(cbind(y,n-y) ~ group, family = BI, data = rabbits)
plot(ajuste1)
wp(ajuste1)
### A distribuição binomial claramente não ajusta satisfatoriamente os dados.

### Usando a função chooseDist, vamos ajustar os demais modelos para contagens binárias.
choose_rabbits <- chooseDist(ajuste1, type = "binom", k = 2)
choose_rabbits

### A distribuição double binomial produziu menor valor de AIC. 
ajuste2 <- gamlss(cbind(y,n-y) ~ group, family = DBI, data = rabbits)
plot(ajuste2)
wp(ajuste2)

fitted(ajuste2, what = 'sigma')[1]
### Os dados exibem super dispersão, uma vez que sigma > 1.

summary(ajuste2)
term.plot(ajuste2)

### Sob dose média, a chance de fetos afetados é exp(1.51695) = 4.558 vezes em
### relação a fetos do grupo controle. Para as doses baixa e alta, a chance de
### fetos afetados não difere significativamente em relação ao grupo controle.