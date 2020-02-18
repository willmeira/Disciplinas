### EXEMPLO 01 ####

library(reshape)
library(mice)
library(plyr)
library(nlme)
library(ggplot2)
library(geepack)

datawide <-read.table("chumbo.txt",header=TRUE, dec = ".")
head(datawide)
summary(subset(datawide, Grupo=="A")[,3:6])#Grupo tratamento

summary(subset(datawide, Grupo=="P")[,3:6])#Grupo controle

## Nota-se uma pequena diminuição das médias dos níveis de chumbo no grupo controle ao longo do tempo. 
## Parao grupo tratamento há um grande decréscimo do baseline para a primeira semana e subsequentes 
## aumentos nas semanas seguintes

## Temos as seguintes estimativas para as correlações:

round(cor(datawide[,3:6]),2)#Todos os indivídos##      
round(cor(subset(datawide, Grupo=="A")[,3:6]),2)# Grupo tratamento##      

with(datawide,boxplot(Sem0,Sem1,Sem4,Sem6,ylab="pb (mg/dl)",xlab="Semana"))
axis(1, 1:4,c(0,1,4,6))

## É mais conveniente trabalharmos com os dados no formatolongo, no qual cada variável é representada 
## poruma coluna e há uma linha para cada medida repetida do indivíduo. Vamos usar a função reshape do 
## pacotede mesmo nome.
datalong <-reshape(data=datawide,direction="long", idvar="ID", v.names="chumbo",
                   varying =list(names(datawide)[3:6]), time=c(0,1,4,6), timevar="tempo")
datalong <-arrange(datalong, ID);head(datalong, 8)


## A representação gráfica mais interessante para as respostas em nível individual é o gráfico de perfis:
p1 <-ggplot(datalong,aes(x=tempo, y=chumbo, color=Grupo))+
            theme_bw()+
            geom_line(aes(group=ID))+ theme(legend.position="top")+
            scale_x_continuous(breaks=unique(datalong$tempo))
p1

## Podemos examinar as diferenças dentro de cada tempo por meio de boxplots:
p2 <-ggplot(datalong,aes(x=factor(tempo), y=chumbo, fill=Grupo))+ geom_boxplot(notch=TRUE)+theme_bw()+ 
            theme(legend.position="top")+ 
            stat_summary(fun.y="mean", geom="point", size=2,position=position_dodge(width=0.75), 
                         color="white", show.legend=FALSE)
p2

## Da ajuda do geom_boxplot: In a notched box plot, the notches extend ‘1.58IQR / sqrt(n)’. This gives 
## aroughly 95% confidence interval for comparing medians. See McGill et al. (1978) for more details.
## Os valores centrais em branco representam as médias.
p3 <-ggplot(datalong,aes(x=tempo, y=chumbo, group = Grupo, shape = Grupo))+ theme_bw()+
            stat_summary(fun.y="mean",geom="line", size=1.1,aes(linetype = Grupo))+
            theme(legend.position="top")+ 
            scale_x_continuous(breaks=unique(datalong$tempo)); p3


## Como vimos, as maiores diferenças ocorrem no tempo 1 e vão diminuindo ao longo das semanas. Passaremosaos 
## ajustes dos modelos marginais por mínimos quadrados generalizados. Consideraremos as estruturas decorrelação
## do tipoindependente,simetria composta,AR(1)enão estruturada.Formulação 1: Modelo linear de efeitos fixos 
## (com intercepto)O primeiro modelo é da formachumbo ~ tempo*Grupoe tem a seguinte representação:

## (Yij) =β1+β2I(tempoj= 1) +β3I(tempoj= 4) +β4I(tempoj= 6) +β5I(Grupoi=P)+β6I(Grupoi=P)×I(tempoj= 1) +
          β7I(Grupoi=P)×I(tempoj= 4)+β8I(Grupoi=P)×I(tempoj= 6).

## Estimador GEE
## Para fazer o ajuste GEE noRpodemos utilizar os seguintes comandos:
datalong$tempo=as.factor(datalong$tempo); datalong$Grupo=as.factor(datalong$Grupo)
gee1a.ind<-geeglm(chumbo~tempo*Grupo, corstr="independence", id=ID,family="gaussian", data=datalong)#Independente
gee1a.exch<-geeglm(chumbo~tempo*Grupo, corstr="exchangeable", id=ID,family="gaussian", data=datalong)#Simetria composta
gee1a.ar1<-geeglm(chumbo~tempo*Grupo, corstr="ar1", id=ID,data=datalong)#AR(1)
gee1a.unst<-geeglm(chumbo~tempo*Grupo, corstr="unstructured", id=ID,data=datalong)#Não estruturada

## Os resultados dos ajustes são:
## Independente
round(coef(summary(gee1a.ind)),3)
## Simetria composta
round(coef(summary(gee1a.exch)),3)

## AR(1)
round(coef(summary(gee1a.ar1)),3)

## Não estruturada 
round(coef(summary(gee1a.unst)),3)

## Note a concordância das inferências mesmo considerando estruturas de correlação de trabalho bastante distintas.
round(summary(gee1a.ind)$corr,3)
round(summary(gee1a.exch)$corr,3)
round(summary(gee1a.ar1)$corr,3)
round(summary(gee1a.unst)$corr,3)

## Vamos comparar as estimativas do modelo marginal dadas pelo GEE com aquelas do estimador GLS para asmesmas
## estruturas de correlação.

##Estimador GLS
## Fazemos o ajuste noRatravés dos seguintes comandos:


gls1a.ind<-gls(chumbo~tempo*Grupo, data=datalong)#Independente
gls1a.exch<-gls(chumbo~tempo*Grupo, correlation=corCompSymm(form=~1|ID),data=datalong)#Simetria composta
gls1a.ar1<-gls(chumbo~tempo*Grupo, correlation=corAR1(form=~1|ID),data=datalong)#AR(1)
gls1a.unst<-gls(chumbo~tempo*Grupo, correlation=corSymm(form=~1|ID),data=datalong)#Não estruturada
## Os resultados são:
# Independente
round(coef(summary(gls1a.ind)),3)
# Simetria composta
round(coef(summary(gls1a.exch)),3)
# AR(1)
round(coef(summary(gls1a.ar1)),3)
# Não estruturada
round(coef(summary(gls1a.unst)),3)


## Observe como os erros padrões estimados dependem da escolha da estrutura de correlação. Isso ocorre apesar
## da coincidência nas estimativas pontuais dos parâmetros de média.
gls1a.ind$modelStruct$corStruct
gls1a.exch$modelStruct$corStruct
gls1a.ar1$modelStruct$corStruct
gls1a.unst$modelStruct$corStruct



anova(gls1a.unst, gls1a.exch)
anova(gls1a.unst, gls1a.ar1)
anova(gls1a.exch, gls1a.ar1)


## Pelo teste da razão de verossimilhanças escolhemos o modelo com correlação não estruturada. O critério AIC
## também identifica a correlação não estruturada como a melhor escolha; já o BIC elege a correlação simetria
## composta.Como consideramos o tempo como fator e efeito de interação tempo e grupo, podemos testar a diferença
## entre os grupos para um tempo especificado. Sejaδj, j= 1,...,4a verdadeira diferença entre os grupos tratamento
## e controle no tempo j. Para j= 4(seis semanas), por exemplo, o modelo especifica:
## ˆE(Yi4|grupoi=P) =ˆβ1+ˆβ4,
## ˆE(Yi4|grupoi=A) =ˆβ1+ˆβ4+ˆβ5+ˆβ8.
## Assumindo correlação de trabalho não estruturada e estimador GLS, temos
## δ4=ˆβ5+ˆβ8



delta4 <-as.numeric(coef(gls1a.unst)[5]+coef(gls1a.unst)[8])
delta4
## A variância estimada de ˆδé dada por: 
## ̂V ar(ˆδ4) =̂V ar(ˆβ5) +̂V ar(ˆβ8) + 2̂Cov(ˆβ5,ˆβ8)

var.beta <-vcov(gls1a.unst)
round(var.beta,2)

## Logo, temos:
var.delta4 <-as.numeric(diag(var.beta)[5]+diag(var.beta)[8]+2*var.beta[5,8])
var.delta4
sqrt(var.delta4)
## A estatística de teste é:
delta4/sqrt(var.delta4)
##Observamos uma diferença estatisticamente significativa entre os grupos. Contudo, é mais direto realizar
## comparações como essa por meio de outra especificação do preditor linear. O modelo permanecerá o mesmomas 
## as comparações dentro de cada tempo serão mais diretas, pois envolverão apenas um parâmetro.



## Formulação 2: Modelo linear de efeitos fixos (sem intercepto)
## O segundo modelo é da forma chumbo ~ tempo*Grupo - Grupo - 1 e tem a seguinte representação:

## E(Yij) = β1I(tempoj= 0) +β2I(tempoj= 1) +β3I(tempoj= 4) +β4I(tempoj= 6)+β5I(Grupoi=P)×I(tempoj= 0) +
            β6I(Grupoi=P)×I(tempoj= 1)+β7I(Grupoi=P)×I(tempoj= 4) +β8I(Grupoi=P)×I(tempoj= 6).
## Os quatro últimos parâmetros permitem a comparação entre os dois grupos. Eles representam diretamente as
## estimativas de δj, j= 1,...,4. 
## Estimador GEE
gee1b.ind<-geeglm(chumbo~tempo*Grupo-Grupo-1, corstr="independence",
                  id=ID,family="gaussian", data=datalong)#Independente
gee1b.exch<-geeglm(chumbo~tempo*Grupo-Grupo-1, corstr="exchangeable", 
                   id=ID,family="gaussian", data=datalong)#Simetria composta
gee1b.ar1<-geeglm(chumbo~tempo*Grupo-Grupo-1, corstr="ar1", id=ID, data=datalong)#AR(1)
gee1b.unst<-geeglm(chumbo~tempo*Grupo-Grupo-1, corstr="unstructured", id=ID,data=datalong)#Não estruturada

#As estimativas são dadas por:
# Independente
round(coef(summary(gee1b.ind)),3)

# Simetria composta
round(coef(summary(gee1b.exch)),3)
# AR(1)
round(coef(summary(gee1b.ar1)),3)
# Não estruturada
round(coef(summary(gee1b.unst)),3)

## Estimador GLS
gls1b.ind<-gls(chumbo~tempo*Grupo-Grupo-1, data=datalong)#Independente
gls1b.exch<-gls(chumbo~tempo*Grupo-Grupo-1,correlation=corCompSymm(form=~1|ID), data=datalong)#Simetria Composta
gls1b.ar1<-gls(chumbo~tempo*Grupo-Grupo-1,correlation=corAR1(form=~1|ID), data=datalong)#AR1
gls1b.unst<-gls(chumbo~tempo*Grupo-Grupo-1,correlation=corSymm(form=~1|ID), data=datalong)#Não estruturada

# Independente
round(coef(summary(gls1b.ind)),3)

# Simetria compostaround(coef(summary(gls1b.exch)),3)
# AR(1)round(coef(summary(gls1b.ar1)),3)
# Não estruturadaround(coef(summary(gls1b.unst)),3)

## Comparando os estimadores GLS e GEE notamos que o primeiro apresenta erros padrões menores. Assim,
## assumindo um nível de significânciaα= 0.05, o efeito de grupo no GEE não é significativo em seis 
## semanas,embora o seja na primeira e na quarta. Na aleatorização (semana zero) os grupos são 
## estatisticamente iguais,como esperado. As inferências via GLS coincidem com as do GEE, exceto para a
## semana seis, para a qual adiferença encontrada é estatisticamente significativa.Observação: o modelo 
## GLS acima é homocedástico pois há apenas uma variância residual. É possível, contudo,estimar variâncias 
## diferentes por grupo por meio do argumentoweights. Por exemplo, a opção weights= varIdent(form = ~ 1 | Grupo) 
## especifica variâncias diferentes por grupo. De forma similar, é possívelespecificar variâncias heterogêneas
## por tempo. Como exercício, avalie a necessidade de considerar variânciasdistintas por grupo ou tempo.


#### EXEMPLO 02 - Dados de Crescimento ####

## Potthoff & Roy (1964) apresentaram um conjunto de dados de crescimento de 11 meninas e 16 meninos. As 
## medidas referem-se à distância entre dois marcos faciais (do centro da pituitária à fissura do maxilar) 
## em quatro idades (8, 10, 12 e 14 anos). O objetivo é descrever e comparar o crescimento de meninos e
## meninas.Análise ExploratóriaOs dados estão disponíveis noRno pacotemicee podem ser acessados como:
data(potthoffroy)
head(potthoffroy)

## A seguir um resumo dos dados por sexo:
with(potthoffroy,by(potthoffroy[,-c(1,2)],sex,summary,digits=3))
## Notamos que as meninas possuem menores valores médios que os meninos. As correlações marginais são
## dadas a seguir no geral e por sexo.
cor(potthoffroy[,-c(1:2)])


## Os dados mostram forte correlação positiva.
with(potthoffroy,by(potthoffroy[,-c(1,2)],sex,cor))

## Contudo, as meninas apresentam correlação entre as medidas repetidas consideravelmente maiores que os
## meninos. Além disso, as correlações para o grupo dos meninos é comparativamente mais variável enquanto
## para as meninas é mais homogênea.A seguir transformamos os dados para o formato longo.
dados <-reshape(data=potthoffroy,direction="long", idvar="id", v.names="resp",
                varying =list(names(potthoffroy)[3:6]), time=c(8,10,12,14), timevar="tempo")
dados <-arrange(dados, id)#Ordenamos os dados por ID, função do pacote plyr
head(dados, 8)

## Na sequência o gráfico de perfis:
p1 <-ggplot(dados,aes(x=tempo, y=resp, color=sex))+  theme_bw()+geom_line(aes(group=id))+ 
            theme(legend.position="top")+scale_x_continuous(breaks=unique(dados$tempo))
p1

## Uma linha de regressão linear ou suavizada pode ser adicionada ao gráfico fazendo
library(gridExtra)
p11 <- p1+ geom_smooth(method="lm",se=FALSE,size=2)
p12 <- p1+ geom_smooth(method="loess",se=FALSE,size=2)
grid.arrange(p11,p12,ncol=2)

##Como vemos, o comportamento longitudinal é aproximadamente linear e um modelo com interação sexo e 
## tempo parece ser adequado. O modelo a ser ajustado é dado por
## E(Yij) =β0+β1×sexoi+β2×tempoj+β3×tempoj×sexoi.

## Estimador GLS
## Consideraremos novamente as estruturas de correlação do tipoindependente,simetria composta,AR(1)e não 
## estruturada. Para fins de análise as idades foram centradas em um valor comum, no caso a média de 11 anos.
dados$tempo=dados$tempo-11
gls2.ind<-gls(resp~sex*tempo, data=dados)#Independente
gls2.exch<-gls(resp~sex*tempo, correlation=corCompSymm(form=~1|id), data=dados)#Simetria composta
gls2.ar1<-gls(resp~sex*tempo, correlation=corAR1(form=~1|id), data=dados)#AR(1)
gls2.unst<-gls(resp~sex*tempo, correlation=corSymm(form=~1|id), data=dados)#Não estruturada
#Os resultados dos ajustes são mostrados a seguir:
# Independente 
round(coef(summary(gls2.ind)),3)
## Simetria composta
round(coef(summary(gls2.exch)),3)
# AR(1)
round(coef(summary(gls2.ar1)),3)
# Não estruturada
round(coef(summary(gls2.unst)),3)
## Note como as estimativas das estruturas independente e simetria composta são similares. Interessante notar
## como o valor p é bastante pequeno parasimetria composta e não estruturada e alto para as demais estruturas.
#  Assim, diferentes escolhas para a correlação levam a diferentes inferências quanto ao efeito de interação.
gls2.ind$modelStruct$corStruct
gls2.exch$modelStruct$corStruct


gls2.ar1$modelStruct$corStruct
gls2.unst$modelStruct$corStruct
## Das correlações marginais vimos que as estruturas independente e autorregressiva não são adequadas a esses 
## dados. Vamos comparar as diferentes estruturas via medidas de informação e testes formais:
anova(gls2.unst, gls2.exch)
anova(gls2.unst, gls2.ar1)
anova(gls2.exch, gls2.ar1)

## A estrutura escolhida por ambos é a simetria composta.

























