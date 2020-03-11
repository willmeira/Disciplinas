library(survival)
library(survminer)

#Considere como exemplo os dados de hepatite.
tempos <-c(1,2,3,3,3,5,5,16,16,16,16,16,16,16,16,1,1,1,1,4,5,7,8,10,10,12,16,16,16)
cens <-c(0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,1,1,1,0,0,1,1,1,1,0,0,0,0,0)
grupos <-c(rep("Controle",15),rep("Esteroide",14))
dados <-data.frame(tempos, cens, grupos)
#Criando o objeto sobrevida (tempo, censura):
Surv(tempos, cens)

ekm <-survfit(Surv(tempos,cens)~grupos, data=dados)
summary(ekm)
ggsurvplot(ekm) ## ver mais argumentos na função do gráfico

ekm0 <-survfit(Surv(tempos,cens)~grupos, conf.type="plain",data=dados)#fórmula anterior apresentada para Var(S(t))
summary(ekm0)
ggsurvplot(ekm0)

sob.NA <-survfit(coxph(Surv(tempos,cens)~grupos,data=subset(dados, grupos=="Esteroide"),method = "breslow"))
summary(sob.NA)
