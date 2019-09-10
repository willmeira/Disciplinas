#
# Toenail Infection Data
#
# Os resultados de um ensaio clinico para comparar dois tratamentos concorrentes 
# antifungicos orais para infecao da unha.
#
# O arquivo de dados contem 1908 observacoes sobre as cinco seguintes variaveis:
#
# patientID: um identificador unico para cada paciente no estudo.
# outcome: grau de separacao da placa ungueal do leito ungueal (onicolise).
# treatment: um fator com niveis itraconazole e terbinafine.
# time: o tempo em meses, quando a visita realmente ocorreu.
# visit: numero de visita a qual compareceram.
#
# Backer et al. (1998) descreveram um ensaio clinico para comparar dois tratamentos 
# concorrentes antifungicos orais para o tratamento da infeccao na unha (onicomicose 
# por dermatofitos). Um total de 378 pacientes foram alocados aleatoriamente em dois 
# grupos de tratamento, um grupo recebendo 250 mg por dia de terbinafina e outro grupo
# de 200 mg por dia de itraconazol. 
#
# Os pacientes foram avaliados em sete visitas, destina-se a ser, nas semanas 0, 4, 8, 
# 12, 24, 36 e 48 para avaliar o grau de separacao da placa da unha do leito ungueal 
# (onicosquise) dicotomizados em "nenhum ou leve" e "moderado ou severo". Mas os 
# pacientes nem sempre chegam exatamente na hora prevista e na hora exata e nos meses 
# que eles fizeram o estudo foi gravado. Os dados nao sao equilibrados uma vez que nem 
# todos os pacientes foram atendidos em todas as sete visitas planejadas.
#
# Referencia
# M. D. Backer and C. D. Vroey and E. Lesaffre and I. Scheys and P. D. Keyser (1998). 
# Twelve weeks of continuous oral therapy for toenail onychomycosis caused by 
# dermatophytes: A double-blind comparative trial of terbinafine 250 mg/day versus 
# itraconazole 200 mg/day. Journal of the American Academy of Dermatology, 38, S57-S63.
#
# Leitura dos dados
#
data("toenail", package = "HSAUR2")
attach(toenail)
names(toenail)
#
# Estudo descritivo
#
library(lattice)
#
resumo.dados = table(outcome,visit,treatment)
resumo.dados
#
X11()
barchart(resumo.dados, xlab='Frequencia', auto.key = list(title='Tratamento'))
#
# Observamos que o numero de individuos com resposta "moderada ou severa" diminui
# bastante a partir da visita 5 quando o tratamento aplicado e terbinafine e, em geral, 
# desde a quarta visita o numero de individuos com resposta "moderada ou severa" diminui
# bastante. Devemos esoerar que a visita seja significativa na resposta.
# 
# Modelo
#
library(geepack)
#
# Codificando a variavel resposta
#
Y=ifelse(outcome=='none or mild',0,1)
#
modelo00 = geeglm(Y~treatment+time+factor(visit), id=patientID, family=binomial, 
                corstr='independence', scale.fix=T)
#
# Outras estruturas de correlacao
#
modelo01 = update(modelo00, corstr='exchangeable')
#
modelo02 = update(modelo00, corstr='ar1')
#
modelo03 = update(modelo00, corstr='unstructured')
#
# Escolha da estrutura de correlacao segundo AIC proposto por
# Akaike's Information Criterion in Generalized Estimating Equations
# Wei Pan, Biometrics, Vol. 57, No. 1. (Mar., 2001), pp. 120-125.
#
library(MuMIn)
#
model.sel(modelo00, modelo01, modelo02, modelo03, rank = QIC)
#
# Modelo selecionado em modelo03
#
summary(modelo03)
anova(modelo03)
#
# O modelo nao deve ser modificado
#
# Resultados
#
library(lattice)
#
X11()
xyplot(modelo03$fitted.values~factor(visit)|treatment, groups=patientID, type='l',
       ylab='Probabilidade de infecao de unha moderada ou severa')
#
# Concluimos que o tempo de visita assim como o tratamento aplicado influencia na 
# probabilidade de infecao na unha moderada ou severa. As diferentes curvas devem-se ao
# fato de que os tempos entre as visitas foram ligeiramente diferentes entre os pacientes.
# No sumario do modelo escolhido (modelo 03) a variavel time, considerada continua, 
# mostra-se nao significativa para a resposta porem na ANOVA da regressao e significativa.
# Isto supo-se deve-se a correlacoes entre os parametrso da regressao.
#
# Nem todos os individuos foram observados em todos os instantes, se interessante for 
# identificar os individuos que participaram de todo o estudo ou nao, apresentamos uma
# maneira de indentifca-los.
#
tabela=table(patientID,visit)
#
T=I(apply(tabela,1,sum)==7)
somas=apply(tabela,1,sum)
TF=rep(T,somas)
#
dados1=toenail[TF=='TRUE',]
dados2=toenail[TF=='FALSE',]
#
# O arquivo dados1 contem os individuos que participaram de todo o estudo e em dado2 
# aqueles que faltaram a um ou mais visitas.
#
# Atualizado em 02/04/2014