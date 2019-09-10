set.seed(423)
n=1000
y=matrix(rnorm(n*3),ncol=3)
head(y)
cor(y)


y2=y
for(i in 1:n)
{
y2[1,]=y[i,]+rexp(1,1/3)
}

head(y)
head(y2)
cor(y2)