x <- c(0.4,1.2,1.8,1.9,2.0,6.8,7.6,8.3,8.7,9.3,10.7,11.3,13.0,13.4,14.2)
y <- c(0.8,2.4,1.8,2.4,2.4,2.9,3.6,3.7,3.1,4.9,3.6,3.2,4.1,4.6,3.8)
plot(x,y)
ajust <- lm(y~x)
summary(ajust)
par(mfrow=c(2,2))
plot(ajust)

par(mfrow=c(1,1))
## --------------------------------------------------------------------
getwd()
df <- read.table("df02.txt", head=TRUE)
par(mfrow=c(1,2))

with(df,plot(x,Y1))
fit1 <- lm(Y1~1,data = df)
abline(fit1,col = 'red')

with(df,plot(x,Y2))
fit2 <- lm(Y2~1,data = df)
abline(fit2,col = 'red')
##----
with(df,plot(x,Y1))
fit1 <- lm(Y1~x,data = df)
abline(fit1,col = 'red')

with(df,plot(x,Y2))
## plot(Y2~x,data = df)
fit2 <- lm(Y2~x,data = df)
abline(fit2,col = 'red')

  
