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


Apêndice B.2 - Figura 4.3

dados<-as.data.frame(cbind(temp,cens,lwbc))
ajust1<-survreg(Surv(dados$temp, dados$cens)~dados$lwbc, dist='exponential')
ajust1
x1<-4.0
temp1<-0:150
ax1<-exp(ajust1$coefficients[1]+ajust1$coefficients[2]*x1)
ste1<-exp(-(temp1/ax1))
x1<-3.0
temp2<-0:150
ax2<-exp(ajust1$coefficients[1]+ajust1$coefficients[2]*x1)
ste2<-exp(-(temp2/ax2))
par(mfrow=c(1,1))
plot(temp1, temp1*0, pch=" ", ylim=range(c(0,1)), xlim=range(c(0,150)), xlab="Tempos", 
     ylab="S(t) estimada", bty="n")
lines(temp1, ste1, lty=2)
lines(temp2, ste2, lty=4)
abline(v=100, lty=3)
legend(10,0.3, lty=c(2,4), c("lwbc = 4.0","lwbc = 3.0"), lwd=1, bty="n")


Apêndice B.3 - Figura 4.4

temp<-c(65,156,100,134,16,108,121,4,39,143,56,26,22,1,1,5,65,56,65,17,7,16,22,3,4,2,3,8,4,3,30,4,43)
cens<-c(rep(1,17),rep(1,16))
lwbc<-c(3.36,2.88,3.63,3.41,3.78,4.02,4,4.23,3.73,3.85,3.97,4.51,4.54,5,5,4.72,
        5,3.64,3.48,3.6,3.18,3.95,3.72,4,4.28,4.43,4.45,4.49,4.41,4.32,4.90,5,5)
grupo<-c(rep(0,17),rep(1,16))
require(survival)
ekm1<-survfit(Surv(temp,cens)~grupo)
summary(ekm1)
st1<-ekm1[1]$surv
time1<-ekm1[1]$time
invst1<-qnorm(st1)
st2<-ekm1[2]$surv
time2<-ekm1[2]$time
invst2<-qnorm(st2)
par(mfrow=c(1,3))
plot(time1, -log(st1), pch=16, xlab="tempos", ylab="-log(S(t))")
points(time2, -log(st2))
legend(100, 0.6, pch=c(16,1), c("Ag+","Ag-"), bty="n")
plot(log(time1), log(-log(st1)), pch=16, xlab="log(tempos)", ylab="log(-log(S(t)))")
points(log(time2), log(-log(st2)))
legend(3, -1.5, pch=c(16,1), c("Ag+","Ag-"), bty="n")
plot(log(time1), invst1,pch=16, xlab="log(tempos)", ylab=expression(Phi^-1*(S(t))))
points(log(time2), invst2)
legend(0.5,-1, pch=c(16,1), c("Ag+","Ag-"), bty="n")


Apêndice B.4 - Modelos Ajustados na Seção 4.5.2

temp<-c(65,156,100,134,16,108,121,4,39,143,56,26,22,1,1,5,65,56,65,17,7,16,22,3,4,2,3,8,4,3,30,4,43)
cens<-c(rep(1,17),rep(1,16))
lwbc<-c(3.36,2.88,3.63,3.41,3.78,4.02,4,4.23,3.73,3.85,3.97,4.51,4.54,5,5,4.72,
        5,3.64,3.48,3.6,3.18,3.95,3.72,4,4.28,4.43,4.45,4.49,4.41,4.32,4.90,5,5)
grupo<-c(rep(0,17), rep(1,16))
dados<-as.data.frame(cbind(temp,cens,lwbc,grupo))
attach(dados)
require(survival)
ajust1<-survreg(Surv(temp,cens)~1, dist='exponential')
ajust1
ajust2<-survreg(Surv(temp,cens)~lwbc, dist='exponential')
ajust2
ajust3<-survreg(Surv(temp,cens)~grupo, dist='exponential')
ajust3
ajust4<-survreg(Surv(temp,cens)~lwbc + grupo, dist='exponential')
ajust4
ajust5<-survreg(Surv(temp,cens)~lwbc + grupo + lwbc*grupo, dist='exponential')
ajust5
summary(ajust4)


Apêndice B.5 - Figura 4.5

t<-temp
x1<-lwbc
x2<-grupo
bo<-6.83
b1<--0.7
b2<--1.02
res<- t*exp(-bo-b1*x1-b2*x2)
ekm <- survfit(Surv(res,dados$cens)~1, type=c("kaplan-meier"))
par(mfrow=c(1,2))
plot(ekm, conf.int=F, lty=c(1,1), xlab="residuos", ylab="S(res) estimada")
res<-sort(res)
exp1<-exp(-res)
lines(res, exp1, lty=3)
legend(2, 0.8, lty=c(1,3), c("Kaplan-Meier","Exponencial(1)"), lwd=1, bty="n", cex=0.8)
st<-ekm$surv
t<-ekm$time
sexp1<-exp(-t)
plot(st, sexp1, xlab="S(res): Kaplan-Meier", ylab= "S(res): Exponencial(1)", pch=16)


Apêndice B.6 - Figura 4.6

x1<-4.0
x2<-0.0
temp1<-0:150
ax1<-exp(6.83-0.70*x1-1.02*x2)
ste1<-exp(-(temp1/ax1))
x1<-3.0
x2<-0.0
temp2<-0:150
ax2<-exp(6.83-0.70*x1-1.02*x2)
ste2<-exp(-(temp2/ax2))
par(mfrow=c(1,2))
plot(temp1, temp1*0, pch=" ", ylim=range(c(0,1)), xlim=range(c(0,150)), xlab="Tempos", 
     ylab="S(t) estimada", bty="n")
lines(temp1, ste1, lty=1)
lines(temp2, ste2, lty=2)
legend(75, 0.8, lty=c(1,2), c("lwbc = 4.0","lwbc = 3.0"), lwd=1, bty="n", cex=0.8)
title("Ag+")
x1<-4.0
x2<-1.0
temp1<-0:150
ax1<-exp(6.83-0.70*x1-1.02*x2)
ste1<-exp(-(temp1/ax1))
x1<-3.0
x2<-1.0
temp2<-0:150
ax2<-exp(6.83-0.70*x1-1.02*x2)
ste2<-exp(-(temp2/ax2))
plot(temp1, temp1*0, pch=" ", ylim=range(c(0,1)), xlim=range(c(0,150)), xlab="Tempos", 
     ylab="S(t) estimada", bty="n")
lines(temp1, ste1, lty=1)
lines(temp2, ste2, lty=2)
legend(75, 0.8, lty=c(1,2), c("lwbc = 4.0","lwbc = 3.0"), lwd=1, bty="n", cex=0.8)
title("Ag-")


Apêndice B.7 - Figura 4.7

x1<-4.0
x2<-0.0
temp1<-0:150
risco1<-1/(exp(6.83-0.70*x1-1.02*x2))
risco1<-rep(risco1, 151)
x1<-3.0
x2<-0.0
temp2<-0:150
risco2<-1/(exp(6.83-0.70*x1-1.02*x2))
risco2<-rep(risco2, 151)
plot(temp1,temp1*0, pch=" ", ylim=range(c(0,0.1)), xlim=range(c(0,150)), xlab="Tempo",
     ylab="Risco estimado", bty="n")
lines(temp1, risco1, lty=1)
lines(temp2, risco2, lty=2)
legend(100, 0.08, lty=c(1,2), c("lwbc = 4.0","lwbc = 3.0"), lwd=1, bty="n", cex=0.8)
title("Ag+")
x1<-4.0
x2<-1.0
temp1<-0:150
risco1<-1/(exp(6.83-0.70*x1-1.02*x2))
risco1<-rep(risco1,151)
x1<-3.0
x2<-1.0
temp2<-0:150
risco2<-1/(exp(6.83-0.70*x1-1.02*x2))
risco2<-rep(risco2, 151)
plot(temp1, temp1*0, pch=" ", ylim=range(c(0,0.1)), xlim=range(c(0,150)), xlab="Tempo",
     ylab="Risco estimado", bty="n")
lines(temp1, risco1, lty=1)
lines(temp2, risco2, lty=2)
legend(100, 0.08, lty=c(1,2), c("lwbc = 4.0","lwbc = 3.0"), lwd=1, bty="n", cex=0.8)
title("Ag-")