## teste de modelagem Tabela de vendas de anúncios ML MZ atacados
## aplicando modelo poisson

vendas <- read_excel("D:/GitHub/MZ_vendas.xlsx", 
                     +     col_types = c("factor", "factor", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric"))


str(vendas)
summary(vendas)
vendas$Destaque <- as.factor(vendas$Destaque)

mod1 <- lm(vendas$vendas~vendas$Destaque)
summary(mod1)

str(vendas2)
dim(vendas2)
summary(vendas2)
vendas2$`Número de publicação` <- as.factor(vendas2$`Número de publicação`)

mod2 <- lm(vendas~Destaque, data = vendas2)
summary(mod2)

mod3 <- glm(vendas~Destaque, data = vendas2, family = poisson(link = "log"))
summary(mod3)
confint(mod3)

mod4 <- glm(vendas~Destaque+perg, data = vendas2, family = poisson(link = "log"))
summary(mod4)

mod5 <- glm(vendas~Destaque+Visit, data = vendas2, family = poisson(link = "log"))
summary(mod5)
BIC(mod5)

mod6 <- glm(vendas~Destaque+perg+Visit, data = vendas2, family = poisson(link = "log"))
summary(mod6)
names(mod6)

mod6$model
modelo <- c("mod3","mod4","mod5","mod6")
vet_aic <-c(mod3$aic,mod4$aic,mod5$aic,mod6$aic)
vet_bic <-c(BIC(mod3),BIC(mod4),BIC(mod5),BIC(mod6))
vet_dev <-c(mod3$deviance,mod4$deviance,mod5$deviance,mod6$deviance)
vet_dfr <-c(mod3$df.residual,mod4$df.residual,mod5$df.residual,mod6$df.residual)

tab_diag <-data.frame(modelo,vet_aic,vet_bic,vet_dev,vet_dfr)
names(tab_diag)<- c("Modelo","AIC","BIC","Deviance","GrausLib")
