library(nlme)
library(ggplot2)
setwd("")#Mudar diretório aqui
#
# Descrição: 
# 
# O estudo envolvendo o Tratamento de Crianças Expostas ao chumbo (TLC)
# foi clínico aleatorizado para placebo e um agente succimer em crianças
# com níveis de chumbo no sangue entre 20-44 micrograms/dL.
# Estes dados consistem de quatro medidas repetidas de níveis de chumbo
# no sangue obtidos na baseline (semana 0), semana 1, semana 4 e semana 6
# em 100 crianças aleatoriamente alocadas entre tratamento e placebo.
 
datawide<-read.table("chumbo.txt",header=T)
cor(datawide[,3:6])

with(datawide, boxplot(Sem0,Sem1,Sem4,Sem6,ylab="pb (mg/dl)",xlab="Semana"))
axis(1, 1:4, c(0,1,4,6))
### Cuidado pois o Boxplot não considera a estrutura longitudinal ####

# Transformando o banco wide para longo
datalong<-reshape(data=datawide,direction="long", idvar="ID", varying = list(names(datawide)[3:6]), v.names="chumbo", time= c(0,1,4,6), timevar="tempo")

# Gráficos de perfis				  
p1=ggplot(datalong, aes(x=tempo, y=chumbo, color=Grupo)) + geom_point() +	theme_bw() + 
   geom_line(aes(group=ID),size=0.05) + theme(legend.position="top") + 
   scale_x_continuous(breaks=unique(datalong$tempo))
				  
# Boxplots				  
p2=ggplot(datalong, aes(x=factor(tempo), y=chumbo, fill=Grupo)) + geom_boxplot(notch=TRUE) + 
   theme(legend.position="top") 

   
# Gráfico de Interação
with(datalong, interaction.plot(tempo, Grupo, chumbo))
 
  


## MODELO 1
## Modelo linear de efeito fixo (com intercepto)
#Independente  
datalong$tempo=as.factor(datalong$tempo)
out1.ind<-lm(chumbo ~  tempo*Grupo,data=datalong)
round(summary(out1.ind)$coef,3)

#Simetria Composta 
out1.exch<-gls(chumbo ~  tempo*Grupo, correlation=corCompSymm (form=~1|ID),data=datalong) 
round(coef(summary(out1.exch)),3)
 
#AR1
out1.ar1<-gls(chumbo ~  tempo*Grupo, correlation=corAR1 (form=~1|ID),data=datalong) 
round(coef(summary(out1.ar1)),3)
 
#Não estruturada 
out1.unst<-gls(chumbo ~  tempo*Grupo, correlation=corSymm (form=~1|ID),data=datalong) 
round(coef(summary(out1.unst)),3)
 
## MODELO 2
## Modelo linear de efeito fixo (sem intercepto)
#      mais simples de comparar os grupos em cada tempo
#Independente 
out2.ind<-lm(chumbo ~ tempo*Grupo -1,data=datalong)
round(summary(out2.ind)$coef,3)
 
#Simetria Composta  
out2.exch<-gls(chumbo ~  tempo*Grupo -1, correlation=corCompSymm (form=~1|ID),data=datalong) 
round(coef(summary(out2.exch)),3)
 
#AR1 
out2.ar1<-gls(chumbo ~  tempo*Grupo -1, correlation=corAR1 (form=~1|ID),data=datalong) 
round(coef(summary(out2.ar1)),3)

 
#Não estruturada
out2.unst<-gls(chumbo ~ tempo*Grupo -1, correlation=corSymm (form=~1|ID),data= datalong) 
round(coef(summary(out2.unst)),3)

  