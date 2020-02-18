rm(list = ls())

dados <-read.table("fev1.csv",h=TRUE,dec=',',sep=";")
head(dados, 15)# duas primeiras meninas

dim(dados)
summary(dados)

plot(table(dados$Id),yaxt="n",ylab="Número de medidas");axis(2,at=1:12)

library(ggplot2)
p1 <-ggplot(dados,aes(x=Age, y=LogFEV1))+ geom_line(aes(group=Id))+ theme_bw()+theme(legend.position="top")+
            scale_x_continuous(breaks=c(6,10,14,18))+geom_point(size=0.5)
p1
