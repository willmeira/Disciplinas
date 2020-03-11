#-----------------------------------------------------------------------------
# Códigos em Linguagem R Apresentados no Apêndice B
#-----------------------------------------------------------------------------

# Capítulo 4 - Modelos de Regressão Paramétricos
#-----------------------------------------------------------------------------
# Apêndice B.1 - Figura 4.2

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


# Apêndice B.2 - Figura 4.3

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


# Apêndice B.3 - Figura 4.4

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


# Apêndice B.4 - Modelos Ajustados na Seção 4.5.2

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


# Apêndice B.5 - Figura 4.5

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


# Apêndice B.6 - Figura 4.6

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


# Apêndice B.7 - Figura 4.7

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

# Capítulo 5 - Modelo de Regressão de Cox
#-----------------------------------------------------------------------------
# Apêndice B.8 - Figura 5.6

laringe<-read.table("https://docs.ufpr.br/~giolo/Livro/ApendiceA/laringe.txt", h=T)     
attach(laringe)
fit4<-coxph(Surv(tempos,cens) ~ factor(estagio) + idade + factor(estagio)*idade, data=laringe, 
            x = T, method="breslow")

Ht<-basehaz(fit4, centered=F)
tempos<-Ht$time
H0<-Ht$hazard
S0<- exp(-H0)
round(cbind(tempos,S0,H0), digits=5)

tt<-sort(tempos)
aux1<-as.matrix(tt)
n<-nrow(aux1)
aux2<-as.matrix(cbind(tempos,S0))
S00<-rep(max(aux2[,2]),n)
for(i in 1:n){
  if(tt[i]> min(aux2[,1])){
    i1<- aux2[,1]<= tt[i]
    S00[i]<-min(aux2[i1,2])}}
ts0<-cbind(tt,S00)
ts0

b<-fit4$coefficients
id<-50
st1<-S00^(exp(b[4]*id))                # S(t|x) estágio I   e idade = 50 anos
st2<-S00^(exp(b[1]+((b[4]+b[5])*id)))  # S(t|x) estágio II  e idade = 50 anos
st3<-S00^(exp(b[2]+((b[4]+b[6])*id)))  # S(t|x) estágio III e idade = 50 anos
st4<-S00^(exp(b[3]+((b[4]+b[7])*id)))  # S(t|x) estágio IV  e idade = 50 anos
id<-65
st11<-S00^(exp(b[4]*id))               # S(t|x) estágio I   e idade = 65 anos
st21<-S00^(exp(b[1]+((b[4]+b[5])*id))) # S(t|x) estágio II  e idade = 65 anos
st31<-S00^(exp(b[2]+((b[4]+b[6])*id))) # S(t|x) estágio III e idade = 65 anos
st41<-S00^(exp(b[3]+((b[4]+b[7])*id))) # S(t|x) estágio IV  e idade = 65 anos

par(mfrow=c(1,2))
plot(tt,st1, type="s", ylim=range(c(0,1)), xlab="Tempos", ylab="S(t|x)", lty=1)
lines(tt,st2, type="s", lty=2)
lines(tt,st3, type="s", lty=3)
lines(tt,st4, type="s", lty=4)
legend(0, 0.2, lty=c(1,2,3,4), c("estágio I","estágio II","estágio III","estágio IV"), 
       lwd=1, bty="n", cex=0.7)
title("Idade = 50 anos")

plot(tt,st11, type="s", ylim=range(c(0,1)), xlab="Tempos", ylab="S(t|x)", lty=1)
lines(tt,st21,type="s",lty=2)
lines(tt,st31,type="s",lty=3)
lines(tt,st41,type="s",lty=4)
legend(0, 0.2, lty=c(1,2,3,4), c("estágio I","estágio II","estágio III","estágio IV"), 
       lwd=1, bty="n", cex=0.7)
title("Idade = 65 anos")


# Apêndice B.9 - Figura 5.7

Ht1<- -log(st1)
Ht2<- -log(st2)
Ht3<- -log(st3)
Ht4<- -log(st4)
Ht11<- -log(st11)
Ht21<- -log(st21)
Ht31<- -log(st31)
Ht41<- -log(st41)
par(mfrow=c(1,2))
plot(tt, Ht1, type="s", ylim=range(c(0,4)), xlab="Tempos", ylab="Risco Acumulado", lty=1)
lines(tt, Ht2, type="s", lty=2)
lines(tt, Ht3, type="s", lty=3)
lines(tt, Ht4, type="s", lty=4)
legend(0.5,3.5, lty=c(1,2,3,4), c("estágio I","estágio II","estágio III","estágio IV"),
       lwd=1, bty="n", cex=0.7)
title("Idade = 50 anos")
plot(tt, Ht11, type="s", ylim=range(c(0,4)), xlab="Tempos", ylab="Risco Acumulado", lty=1)
lines(tt, Ht21, type="s", lty=2)
lines(tt, Ht31, type="s", lty=3)
lines(tt, Ht41, type="s", lty=4)
legend(0.5,3.5, lty=c(1,2,3,4), c("estadio I","estágio II","estágio III","estágio IV"),
       lwd=1, bty="n", cex=0.7)
title("Idade = 65 anos")


# Apêndice B.10 - Figura 5.8

desmame<-read.table("https://docs.ufpr.br/~giolo/Livro/ApendiceA/desmame.txt", h=T)
attach(desmame)
require(survival)
par(mfrow=c(2,2))
fit1<-coxph(Surv(tempo[V1==0],cens[V1==0])~1,data=desmame, x=T, method="breslow")
ss<- survfit(fit1)
s0<-round(ss$surv, digits=5)
H0<- -log(s0)
plot(ss$time, log(H0), xlim=range(c(0,20)), xlab="Tempos", ylab=expression(log(Lambda[0]*(t))),
     bty="n",type="s")
fit2<-coxph(Surv(tempo[V1==1],cens[V1==1])~1, data=desmame, x=T, method="breslow")
ss<- survfit(fit2)
s0<-round(ss$surv, digits=5)
H0<- -log(s0)
lines(ss$time, log(H0), type="s", lty=2)
legend(10,-3, lty=c(2,1), c("V1 = 1 (Não)","V1 = 0 (Sim)"), lwd=1,bty="n",cex=0.7)
title("V1: Experiência Amamentação")

# Obs: análogo para as demais covariaveis.


# Apêndice B.11 - Figura 5.10

leucc<-read.table("https://docs.ufpr.br/~giolo/Livro/ApendiceA/leucc.txt", h=T)
attach(leucc)
require(survival)
par(mfrow=c(2,3))
fit<-coxph(Surv(tempos[leuinic==1],cens[leuinic==1]) ~ 1, data=leucc, x = T, method="breslow")
ss<- survfit(fit)
s0<-round(ss$surv, digits=5)
H0<- -log(s0)
plot(ss$time, log(H0), xlab="Tempos", ylim=range(c(-5,1)), ylab = expression(log(Lambda[0]* (t))),
     bty="n", type="s")
fit<-coxph(Surv(tempos[leuinic==0],cens[leuinic==0]) ~ 1, data=leucc, x = T, method="breslow")
ss<- survfit(fit)
s0<-round(ss$surv, digits=5)
H0<- -log(s0)
lines(ss$time, log(H0), type="s", lty=4)
legend(1.5, -4, lty=c(4,1), c("leuini < 75","leuini > 75 "), lwd=1, bty="n", cex=0.8)
title("LEUINI")

# Obs: Análogo para as demais covariáveis.

# Capítulo 6 - Extensões do Modelo de Regressão de Cox
#-----------------------------------------------------------------------------
# Apêndice B.12 - Resultados e Figuras - Seção 6.6.1

hg2<-read.table("https://docs.ufpr.br/~giolo/Livro/ApendiceA/hg2.txt", h=T) 
attach(hg2)
require(survival)
rendac<-ifelse(renda<4,1,2)
alt<-ifelse(ialtura<120,1,2)
fit3<-coxph(Surv(tempos,cens)~factor(raca)+factor(trauma) + factor(recemnas) + factor(rendac) +
              factor(trauma)*factor(recemnas) + strata(alt), data=hg2, method="breslow")
summary(fit3)
fit4<-coxph(Surv(tempos,cens)~factor(raca) + factor(trauma) + factor(rendac) + strata(alt),
            data=hg2, method="breslow")
summary(fit4)
cox.zph(fit4, transform="identity")
par(mfrow=c(1,3))
plot(cox.zph(fit4))
H0<-basehaz(fit4, centered=F)
H0
H01<-as.matrix(H0[1:21,1])
H02<-as.matrix(H0[22:39,1])
tempo1<-H0$time[1:21]
S01<-exp(-H01)
round(cbind(tempo1,S01,H01), digits=5)
tempo2<- H0$time[22:39]
S02<-exp(-H02)
round(cbind(tempo2,S02,H02), digits=5)

par(mfrow=c(1,2))
plot(tempo2, H02, lty=4, type="s", xlab="Tempos", xlim=range(c(10,50)), ylab=expression(Lambda[0]*(t)))
lines(tempo1, H01, type="s", lty=1)
legend(10, 25, lty=c(1,4), c("altura inicial < 120cm","altura inicial >= 120cm"),
       lwd=1, bty="n", cex=0.8)
plot(c(0,tempo2),c(1,S02), lty=4, type="s", xlab="Tempos",
     ylim=range(c(0,1)), xlim=range(c(10,50)), ylab="So(t)")
lines(c(0,tempo1),c(1,S01), lty=1, type="s")
legend(25,0.85, lty=c(1,4), c("altura inicial < 120cm", "altura inicial>=120cm"), lwd=1, 
       bty="n", cex=0.8)


# Análise gráfica dos resíduos de Cox-Snell do modelo ajustado.

hg2a<-na.omit(hg2[,c(1:5,7)])
rendac<-ifelse(hg2a$renda<4,1,2) 
alt<-ifelse(hg2a$ialtura<120,1,2)
fit4<-coxph(Surv(tempos,cens)~factor(raca) + factor(trauma) + factor(rendac) + strata(alt),
            data=hg2a, method="breslow")
summary(fit4)
resm<-resid(fit4, type="martingale") 
res<-hg2a$cens-resm 
ekm <- survfit(Surv(res, hg2a$cens)~1)
summary(ekm)

par(mfrow=c(1,2))
plot(ekm, mark.time=F, conf.int=F, xlab="resíduos", ylab="S(e) estimada")
res<-sort(res)
exp1<-exp(-res)
lines(res, exp1, lty=3)
legend(1, 0.8, lty=c(1,3), c("Kaplan Meier","Exponencial(1)"), lwd=1, bty="n", cex=0.7)
st<-ekm$surv
t<-ekm$time
sexp1<-exp(-t)
plot(st, sexp1, xlab="S(e): Kaplan-Meier", ylab= "S(e): Exponencial(1)", pch=16)

# Capítulo 8 - Censura Intervalar e Dados Grupados
#-----------------------------------------------------------------------------
# Apêndice B.13 - Figura 8.6

mang1<-read.table("https://docs.ufpr.br/~giolo/Livro/ApendiceA/dadmang.txt", h=T)
attach(mang1)
fit1<-glm(y~-1+int1+int2+int3+int4+int5+int6+int7+int8+int9+int10+int11+int12+
            factor(bloco,levels=5:1)+as.factor(copa), family=binomial(link="cloglog"))

cf<-as.vector(fit1$coefficients[1:12])  #(gama_i)*
gi<-exp(-exp(cf))                       #(gama_i)

S0<-gi
for(i in 1:11){
  S0[i+1]<-prod(gi[1:(i+1)])}
S0<-c(1,S0)                             # So(t)

cf1<-fit1$coefficients[18:22] 
Sc1<-S0 
Sc2<-(S0)^exp(cf1[1])
Sc3<-(S0)^exp(cf1[2]) 
Sc4<-(S0)^exp(cf1[3]) 
Sc5<-(S0)^exp(cf1[4])
Sc6<-(S0)^exp(cf1[5])

t<-c(0,2,3,4,10,12,14,15,16,17,18,19,21)
cbind(t,Sc1,Sc2,Sc3,Sc4,Sc5,Sc6)
par(mfrow=c(1,1))
plot(t,Sc1, type="s", lty = 1, ylim=range(c(0,1)), xlab="Tempo de Vida (anos)",
     ylab="Sobrevivência Estimada")
points(t,Sc1,pch=21) 
lines(t,Sc2,type="s",lty=2)
points(t,Sc2,pch=15) 
lines(t,Sc3,type="s",lty=3)
points(t,Sc3,pch=14) 
lines(t,Sc4,type="s",lty=4)
points(t,Sc4,pch=8)  
lines(t,Sc5,type="s",lty=5)
points(t,Sc5,pch=16) 
lines(t,Sc6,type="s",lty=6)
points(t,Sc6,pch=17)
legend(1, 0.5, lty=c(1,2,3,4,5,6), pch=c(21,15,14,8,16,17), c("Copa 1-Extrema","Copa 2-Oliveira",
                                                              "Copa 3-Pahiri","Copa 4-Imperial","Copa 5-Carlota","Copa 6-Bourbon"), bty="n", cex=0.9)
title("Modelo de Riscos Proporcionais")


# Apêndice B.14 - Figura 8.7

mang1<-read.table("https://docs.ufpr.br/~giolo/Livro/ApendiceA/dadmang.txt", h=T)
attach(mang1)
fit2<-glm(y~-1+int1+int2+int3+int4+int5+int6+int7+int8+int9+int10+int11+int12+
            factor(bloco,levels=5:1)+as.factor(copa), family=binomial(link="logit"))

cf<-fit2$coefficients[1:12]       #(gama_i)*
gi<-exp(cf)                       #(gama_i)

cf1<-fit2$coefficients[18:22]
qi1<-(1/(1+gi))
Slc1<-qi1
for(i in 1:11){
  Slc1[i+1]<-prod(qi1[1:(i+1)])}
Slc1<-c(1,Slc1)                   #S(t) copa 1
qi2<-(1/(1+gi*exp(cf1[1])))
Slc2<-qi2
for(i in 1:11){
  Slc2[i+1]<-prod(qi2[1:(i+1)])}
Slc2<-c(1,Slc2)                   #S(t) copa 2
qi3<-(1/(1+gi*exp(cf1[2])))
Slc3<-qi3
for(i in 1:11){
  Slc3[i+1]<-prod(qi3[1:(i+1)])}
Slc3<-c(1,Slc3)                   #S(t) copa 3
qi4<-(1/(1+gi*exp(cf1[3])))
Slc4<-qi4
for(i in 1:11){
  Slc4[i+1]<-prod(qi4[1:(i+1)])}
Slc4<-c(1,Slc4)                   #S(t) copa 4
qi5<-(1/(1+gi*exp(cf1[4])))
Slc5<-qi5
for(i in 1:11){
  Slc5[i+1]<-prod(qi5[1:(i+1)])}
Slc5<-c(1,Slc5)                   #S(t) copa 5
qi6<-(1/(1+gi*exp(cf1[5])))
Slc6<-qi6
for(i in 1:11){
  Slc6[i+1]<-prod(qi6[1:(i+1)])}
Slc6<-c(1,Slc6)                   #S(t) copa 6

t<-c(0,2,3,4,10,12,14,15,16,17,18,19,21)
cbind(t,Slc1,Slc2,Slc3,Slc4,Slc5,Slc6)
par(mfrow=c(1,1))
plot(t, Slc1, type="s", lty=1, ylim=range(c(0,1)), xlab="Tempo de Vida (anos)",
     ylab="Sobrevivência Estimada")
points(t, Slc1, pch=21)
lines(t, Slc2, type="s", lty=2)
points(t, Slc2, pch=15)
lines(t, Slc3, type="s", lty=3)
points(t, Slc3, pch=14)
lines(t, Slc4, type="s", lty=4)
points(t, Slc4, pch=8)
lines(t, Slc5, type="s", lty=5)
points(t, Slc5, pch=16)
lines(t, Slc6, type="s", lty=6)
points(t, Slc6, pch=17)
legend(1, 0.5, lty=c(1,2,3,4,5,6), pch=c(21,15,14,8,16,17), c("Copa 1-Extrema","Copa 2-Oliveira",
                                                              "Copa 3-Pahiri", "Copa 4-Imperial","Copa 5-Carlota","Copa 6-Bourbon"), bty="n", cex=0.9)
title("Modelo Logístico")

#-----------------------------------------------------------------------------
# Revisado em 15/02/2018.
