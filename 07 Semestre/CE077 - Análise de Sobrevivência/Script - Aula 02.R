require(survival)
tempos<- c(1,2,3,3,3,5,5,16,16,16,16,16,16,16,16,1,1,1,1,4,5,7,8,10,10,12,16,16,16)
cens<-c(0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,1,1,1,0,0,1,1,1,1,0,0,0,0,0)
grupos<-c(rep(1,15),rep(2,14))
ekm<- survfit(Surv(tempos,cens)~grupos)
summary(ekm)
plot(ekm, lty=c(2,1), xlab="Tempo (semanas)",ylab="S(t) estimada")
legend(1, 0.3, lty=c(2,1), c("Controle","Esteróide"),lwd=1, bty="n")

ekm<- survfit(Surv(tempos,cens)~grupos, conf.type="plain")
summary(ekm)

ekm<- survfit(Surv(tempos,cens)~grupos, conf.type="log-log")
summary(ekm)

ekm<- survfit(Surv(tempos,cens)~grupos, conf.type="log")
summary(ekm)
ekm<- survfit(Surv(tempos,cens)~grupos)
summary(ekm)


