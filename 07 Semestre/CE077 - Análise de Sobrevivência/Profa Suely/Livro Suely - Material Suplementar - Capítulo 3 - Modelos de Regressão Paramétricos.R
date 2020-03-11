#-------------------------------------------------------------------------------
# Material Suplementar
#-------------------------------------------------------------------------------

# Capítulo 3 - Modelos de Regressão Paramétricos
#-------------------------------------------------------------------------------

# Além do método delta para obtenção da var(tm) no caso do modelo lognormal ou outros 
# modelos paramétricos, pode-se também obtê-la via reamostragem bootstrap, como 
# ilustrado a seguir.

# Obtenção da var(tm) usando reamostragem bootstrap - Modelo lognormal

tempos<-c(3,5,6,7,8,9,10,10,12,15,15,18,19,20,22,25,28,30,40,45)
cens<-c(1,1,1,1,1,1,1,0,1,1,0,1,1,1,1,1,1,1,1,0)
data<-as.data.frame(cbind(tempos,cens))

d<-5000                             # numero de reamostragens bootstrap
n<-length(tempos)                   # n = tamanho amostral 
tmb<-matrix(0,d,1)                 # tempos médios das reamostragens 
for(j in 1:d){
  row<-sample(n, replace=T)        # reamostragem com reposição      
  dados<-data[row,]
  mod<-survreg(Surv(tempos,cens)~1, dist='lognorm', data=dados)
  tmb[j]<-exp(mod$coef[1]+(mod$scale^2/2))
}
var(tmb)                               # var(tm) bootstrap    
hist(tmb, breaks=20)               # histograma das variâncias

# I.C.(tm)_95% utilizando a variância bootstrap

mod1<-survreg(Surv(tempos,cens)~1, dist='lognorm', data=data)
mu<-mod1$coef[1] 
sigma<-mod1$scale
tm<-as.numeric(exp(mu+(sigma^2/2)))
li<-tm - 1.96*sqrt(var(tmb))
ls<-tm + 1.96*sqrt(var(tmb))
cbind(tm, li, ls) 

# Obtenção da var(tm) usando reamostragem bootstrap - Modelo Weibull

tempos<-c(3,5,6,7,8,9,10,10,12,15,15,18,19,20,22,25,28,30,40,45)
cens<-c(1,1,1,1,1,1,1,0,1,1,0,1,1,1,1,1,1,1,1,0)
data<-as.data.frame(cbind(tempos,cens))

d<-5000                             # numero de reamostragens bootstrap
n<-length(tempos)                   # n = tamanho amostral 
tmb<-matrix(0,d,1)                 # tempos médios das d reamostragens 
for(j in 1:d){
  row<-sample(n, replace=T)        # reamostragem com reposição      
  dados<-data[row,]
  mod<-survreg(Surv(tempos,cens)~1, dist='weibull', data=dados)
  a<-exp(mod$coef[1]) 
  g<-1/mod$scale 
  tmb[j]<-a*gamma(1+1/g)
}
var(tmb)                           # variância bootstrap
hist(tmb, breaks=20)               # histograma das variâncias

# I.C.(tm)_95% utilizando a variância bootstrap

mod1<-survreg(Surv(tempos,cens)~1, dist='weibull', data=data)
a<-exp(mod$coef[1])
g<-1/mod$scale 
tm<-as.numeric(a*gamma(1+1/g))
li<-tm - 1.96*sqrt(var(tmb)) 
ls<-tm + 1.96*sqrt(var(tmb))
cbind(tm, li, ls) 

# Analogamente, a variância do tempo mediano também pode ser obtida por meio 
# de reamostragem bootstrap.

# Obtenção da var(tmed) usando reamostragem bootstrap - Modelo lognormal

tempos<-c(3,5,6,7,8,9,10,10,12,15,15,18,19,20,22,25,28,30,40,45)
cens<-c(1,1,1,1,1,1,1,0,1,1,0,1,1,1,1,1,1,1,1,0)
data<-as.data.frame(cbind(tempos,cens)) 
d<-5000                        # numero de reamostragens bootstrap
n<-length(tempos)              # n = tamanho amostral 
tmd<-matrix(0,d,1)            # tempos medianos das reamostragens 
for(j in 1:d){
  row<-sample(n, replace=T)   # reamostragem com reposição      
  dados<-data[row,]
  mod<-survreg(Surv(tempos,cens)~1, dist='lognorm', data=dados)
  tmd[j]<-exp(mod$coef[1])
}
v<-var(tmd); v                # variância bootstrap
hist(tmd, breaks=20)          # histograma das variâncias

# I.C.(tmed)_95% utilizando a variância bootstrap

mod1<-survreg(Surv(tempos,cens)~1, dist='lognorm', data=data)
tmed<-as.numeric(exp(mod1$coef[1]))
li<-tmed - 1.96*sqrt(v) 
ls<-tmed + 1.96*sqrt(v)
cbind(tmed, li, ls)  