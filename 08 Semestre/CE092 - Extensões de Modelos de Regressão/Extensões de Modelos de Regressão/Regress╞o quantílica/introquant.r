##
## Intrudução
## Média, mediana e quartis como resultado de otimização
## sob diferentes critérios 

## sumulando um conjunto de dados
x <- rnorm(50, m=50, sd=10)

## média: Função perda quadrática 
mf <- function(m, dados){
    loss <- sum((dados - m)^2)
    return(loss)
}
optimize(mf, interval = c(min(x), max(x)), dados=x)
mean(x)

## mediana: função perda linear
mdf <- function(q, dados){
    loss <- sum(abs(dados - q))
    return(loss)
}
optimize(mdf, interval = c(min(x), max(x)), dados=x)
quantile(x, prob=0.5)
median(x)


## quantis: funções perda lineares assimétricas
loss <- function(erro, prob){
    if(any(prob < 0 | prob > 1)) stop("prob deve ser um valor entre 0 e 1") 
    loss <- (ifelse(erro >= 0, prob, 1-prob)*abs(erro))
    return(loss)
}
curve(loss(x,prob=0.5), from=-10, to=10)
curve(loss(x,prob=0.25), from=-10, to=10, col=2, add=T)
curve(loss(x,prob=0.75), from=-10, to=10, col=4, add=T)

## função perda para obtenção quantis
qf <- function(q, prob, dados){
    if(any(prob < 0 | prob > 1)) stop("prob deve ser um valor entre 0 e 1") 
    loss <- sum(ifelse(dados-q >= 0, prob, (1-prob)) * abs(dados-q))
    return(loss)
}

## exemplo: obtenção de quartis
c(optimize(qf, interval = c(min(x), max(x)), dados=x, prob=0.25)$min,
optimize(qf, interval = c(min(x), max(x)), dados=x, prob=0.5)$min,
optimize(qf, interval = c(min(x), max(x)), dados=x, prob=0.75)$min)

fivenum(x)[2:4]
quantile(x, prob=c(0.25, 0.5, 0.75))



##
## Regressão quantilica (linear simples)
##

## Carregando um dado como exemplo
data(Nile
y<-as.numeric(Nile)
x<-1:length(Nile)

## regressão linear simples usual
plot(y~ x)
(mod1<-lm(y~x))
abline(coef(mod1))

## obtenção do mesmo resultado por minimização de uma função perda quadrática (regressao L2)
mse<-function(pars,y,x){
  e <- sum((y-(pars[1]+pars[2]*x))^2)
  return(e)
}

(mod2<-optim(c(1200,-2),mse,y=y,x=x))$par
abline(mod2$par, col=2)

## Mundando agora para uma função perda linear
## (regressão "mediana" ou L1)

lad<-function(pars,y,x){
  e <- sum(abs(y-(pars[1]+pars[2]*x)))
  return(e)
}

(mod3<-optim(c(1200,-2),lad,y=y,x=x))$par

## comparando resultados até aqui
t<-data.frame(b0=c(coef(mod1)[1],mod2$par[1],mod3$par[1]),
              b1=c(coef(mod1)[2],mod2$par[2],mod3$par[2]))
rownames(t)<-c('Al2.lm','Al2.optim','AL1.optim');t

## plotando novamente 
plot(y~ x)
cor=c('red','blue','green','pink','violet')
for(i in 1:3){
    abline(t[i,] ,col=cor[i], lwd=2, lty=i)
}
legend('topright',rownames(t), col=cor, lwd=2)
  

## Generalizando a função perda linear: Regressão Quantilica 
wad <-function(pars, x, y, prob){
    erro <- y - (pars[1]+pars[2]*x)
    i <- which(erro<0)
    e1 <- sum((1-prob) * abs(y[i]-( pars[1] + pars[2]*x[i] ) ) )
    e2 <- sum(prob * abs(y[-i]-( pars[1] + pars[2]*x[-i] ) ) )
    e  <- (e1+e2)/length(y)
    return(e)
}

## mediana (comparar com resultado anterior)
(mod4<-optim(c(1400,-3), wad, y=y, x=x, prob=0.5))$par
(mod3<-optim(c(1400,-3), lad, y=y, x=x))$par

## regressão no 1o quartil
(mod5<-optim(c(1400,-3), wad, y=y, x=x, prob=0.25))$par
abline(mod5$par, lwd=2, lty=2)

grid<-c(0.1,0.3,0.5,0.7,0.9)
plot(y~x)
for (i in 1:length(grid)){
    modi <- optim(c(1200,-3),wad,y=Nile,x=x,prob=grid[i])
    abline(modi,col=cor[i],lwd=2)
}
legend('topright',legend=grid,col=cor,lwd=2)


grid<-c(0.025,0.5, 0.975)
plot(y~x)
for (i in 1:length(grid)){
    modi <- optim(c(1200,-3),wad,y=Nile,x=x,prob=grid[i])
    abline(modi,col=4,lwd=2)
}

fit<-predict(mod1,interval = "prediction")
lines(fit[,1],col='red')
lines(fit[,2],col='red')
lines(fit[,3],col='red')
legend('topright',legend=c('Rq',"IC(0.95)"),col=c('blue','red'),lwd=2)

##
## Pacote do R para regressão quantílica
##
#install.packages('quantreg')
#http://cran.r-project.org/web/packages/quantreg/vignettes/rq.pdf

require(quantreg)
rq(Nile ~ x, tau = .5)
rq(Nile ~ x, tau = .25)
rq(Nile ~ x, tau = .75)

