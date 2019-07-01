temp<-c(65,156,100,134,16,108,121,4,39,143,56,26,22,1,1,5,65)
cens<-rep(1,17)
lwbc<-c(3.36,2.88,3.63,3.41,3.78,4.02,4.00,4.23,3.73,3.85,3.97,4.51,4.54,5.00,5.00,4.72,5.00)
dados<-cbind(temp,cens,lwbc)

require(survival)
dados<-as.data.frame(dados)
i<-order(dados$temp)
dados<-dados[i,]
ekm<- survfit(Surv(dados$temp,dados$cens)~1)
summary(ekm)
st<-ekm$surv
temp<-ekm$time
invst<-qnorm(st)
par(mfrow=c(1,3))
plot(temp, -log(st), pch=16, xlab="Tempos", ylab="-log(S(t))")
plot(log(temp), log(-log(st)), pch=16, xlab="log(tempos)", ylab="log(-log(S(t))")
plot(log(temp), invst, pch=16, xlab="log(tempos)", ylab=expression(Phi^-1*(S(t))))

  ajust1<-survreg(Surv(dados$temp, dados$cens)~dados$lwbc, dist='exponential')
ajust1
ajust1$loglik
ajust2<-survreg(Surv(dados$temp, dados$cens)~dados$lwbc, dist='weibull')
ajust2
ajust2$loglik
gama<-1/ajust2$scale
gama


#**************************************************************

temp<-c(65,156,100,134,16,108,121,4,39,143,56,26,22,1,1,5,65)
cens<-rep(1,17)
lwbc<-c(3.36,2.88,3.63,3.41,3.78,4.02,4.00,4.23,3.73,3.85,3.97,4.51,4.54,5.00,5.00,4.72,5.00)
dados<-cbind(temp,cens,lwbc)
require(survival)
dados<-as.data.frame(dados)
i<-order(dados$temp)
dados<-dados[i,]
ajust1<-survreg(Surv(dados$temp, dados$cens)~dados$lwbc, dist='exponential')
x<-dados$lwbc
t<-dados$temp
bo<- ajust1$coefficients[1]
b1<- ajust1$coefficients[2]
res<- t*exp(-bo-b1*x)
ekm <- survfit(Surv(res,dados$cens)~1, type=c("kaplan-meier"))
summary(ekm)
par(mfrow=c(1,2))
plot(ekm, conf.int=F, lty=c(1,1), xlab="resíduos", ylab="S(e) estimada")
res<-sort(res)
exp1<-exp(-res)
lines(res,exp1,lty=3)
legend(2,0.8, lty=c(1,3), c("Kaplan-Meier","Exponencial(1)"), lwd=1, bty="n", cex=0.7)
st<-ekm$surv
t<-ekm$time
sexp1<-exp(-t)
plot(st, sexp1, xlab="S(e) - Kaplan-Meier", ylab= "S(e) - Exponencial(1)", pch=16)





