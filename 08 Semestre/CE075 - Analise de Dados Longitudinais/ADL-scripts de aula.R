set.seed(423)
n=1000
y=matrix(rnorm(n*3),ncol=3)
head(y)
cor(y)


y2=y
for(i in 1:n)
{
y2[1,]=y[i,]+rexp(1,1/3)
}

head(y)
head(y2)
cor(y2)


#############################################################################################
# Aula 04/11
#############################################################################################

library(foreign);
library(plyr);
library(ordinal);
library(multgee)

ds <-read.dta("arthritis.dta")
head(ds)

install.packages("multgee")


dslong <-reshape(ds, idvar="id", varying=c("y1","y2","y3","y4"),
                 v.names="y",timevar="time", time=1:4, direction="long")

dslong <-arrange(dslong, id, time);head(dslong)

attach(dslong)
month <- 2*(time-1); 
sqrtmonth <- month^0.5

## Comparamos as proporções das categorias de resposta por grupo:
round(100*prop.table((table(time[trt==0],y[trt==0])),1),2)#proporções placebo

round(100*prop.table((table(time[trt==1],y[trt==1])),1),2)#proporções auranofina

## A seguir são mostrados os resultados do ajuste de máxima verossimilhança.
fit1 <-clmm(ordered(y)~trt+sqrtmonth+trt:sqrtmonth+(1|id), nAGQ = 50)
summary(fit1)#considerar o sinal inverso para \beta


fit.gee <-ordLORgee(y~trt+sqrtmonth+trt:sqrtmonth, id=id, LORstr = "uniform")
summary(fit.gee)

