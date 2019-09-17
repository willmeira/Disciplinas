library(reshape)
library(plyr)
library(nlme)
library(ggplot2)
library(geepack)
install.packages("reshape")

datawide <-read.table("https://docs.ufpr.br/~jlpadilha/CE075/Datasets/chumbo.txt",header=TRUE)
head(datawide)
