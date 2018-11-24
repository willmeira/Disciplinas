## ----global_options, include=FALSE---------------------------------------
knitr::opts_chunk$set(fig.width=6, fig.height=4, fig.path='Figs/', echo=T, warning=T,message=FALSE,cache=F,comment=NULL,fig_retina=NULL,purl=T)

## ------------------------------------------------------------------------
library(FactoMineR)
library(SensoMineR)
library(RVAideMemoire)
library(gdata)

## ------------------------------------------------------------------------
maca<-read.xls('cata.xls',h=T,sheet=1,row.names=1)

## ------------------------------------------------------------------------
maca

## ------------------------------------------------------------------------
chisq.test(maca)

## ------------------------------------------------------------------------
maca.t<-read.xls('cata.xls',h=T,sheet=2)

## ------------------------------------------------------------------------
head(maca.t)

## ------------------------------------------------------------------------
tail(maca.t)

## ------------------------------------------------------------------------
names(maca.t)

## ------------------------------------------------------------------------
maca.t$Muestra<-as.factor(maca.t$Muestra)
maca.t$Consumidor<-as.factor(maca.t$Consumidor)

## ------------------------------------------------------------------------
cochran.qtest(Acida~Muestra|Consumidor, data=maca.t)

## ------------------------------------------------------------------------
maca.ca<-CA(maca)

## ------------------------------------------------------------------------
ellipseCA(maca.ca,ellipse="row",col.col.ell=c(rep("blue",2),rep("transparent",3)),
     invisible=c("row.sup","col.sup"))

## ------------------------------------------------------------------------
ellipseCA(maca.ca,ellipse="col",col.row.ell=c(rep("red",2),rep("transparent",3)),
     invisible=c("row.sup","col.sup"))

## ------------------------------------------------------------------------
ellipseCA(maca.ca)

