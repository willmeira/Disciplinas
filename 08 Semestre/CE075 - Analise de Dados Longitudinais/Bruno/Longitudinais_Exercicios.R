# Longitudinais

# install.packages("geepack")
data("toenail", package = "HSAUR2")
require(lme4)
require(geepack)
attach(toenail)
names(toenail)


Y    =ifelse(outcome=='none or mild',0,1)

fit1 <- geeglm(Y ~ visit + treatment + visit:treatment - treatment ,
               family = binomial, id = patientID,corstr='exchangeable')
summary(fit1)


fit2 <- glmer(Y ~ visit + treatment + visit:treatment - treatment + (1|patientID),
               family = binomial)
summary(fit2)
names(summary(fit2))
class(summary(fit2)$coefficients)
dim(summary(fit2)$coefficients)

# Quadratura
# 2 ,3 , 10, 20, 30, 50
parametros <- NULL
erros_p <- NULL
number <- c(2 ,3 , 10, 20, 30, 50)
i <- 1
while (i <= length(number)){
  fit00 <- glmer(Y ~ visit + treatment + visit:treatment - treatment + (1|patientID),
                family = binomial, nAGQ = number[i])
  
  parametros <- rbind(parametros,summary(fit00)$coefficients[,1])
  erros_p <- rbind(erros_p,summary(fit00)$coefficients[,2])
  i <- i + 1
}

parametros <- as.data.frame(parametros)
erros_p <- as.data.frame(erros_p)
parametros$quadratura <- NA
erros_p$quadratura <- NA
i <- 1
while (i <= length(number)){
  parametros[i,4] <- number[i]
  erros_p[i,4] <- number[i]
    i <- i + 1
}
parametros
erros_p


