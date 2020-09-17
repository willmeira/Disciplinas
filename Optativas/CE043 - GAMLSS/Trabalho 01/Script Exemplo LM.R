library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(corrplot)
library(gamlss)
library(gamlss.data)
library(gamlss.dist)

mydata = read.csv("SolarPrediction.csv",sep = ",", dec = ".", header = TRUE)

mydata$Data = as.character(mydata$Data)
Data_splitted = mydata$Data %>% str_split(" ", n=2, simplify =TRUE)
date= Data_splitted[,1]

month_day = date %>% str_split("/", n=3, simplify =TRUE)
month_day = month_day[,c(1,2)]

str(month_day)

month_day = data.frame(month_day)

month_day$X1 = as.numeric(month_day$X1) +8
month_day$X2 = as.numeric(month_day$X2)


mydata = mydata[,-c(1,2,3)]
str(mydata)

#Making arrangements on TimeSunRise column
timesunrise = mydata[,7]
timesunrise = as.character(timesunrise)
str(timesunrise)
splitted_timesunrise = timesunrise %>% str_split(":", n = 3, simplify = TRUE)
str(splitted_timesunrise)
splitted_timesunrise = data.frame(splitted_timesunrise,stringsAsFactors = FALSE)
colnames(splitted_timesunrise)
splitted_timesunrise$X1 = as.numeric(splitted_timesunrise$X1)
splitted_timesunrise$X2 = as.numeric(splitted_timesunrise$X2)
splitted_timesunrise$X3 = as.numeric(splitted_timesunrise$X3)

beginningtime = rep(NA,32686)
for(i in 1:32686){
  beginningtime[i] = splitted_timesunrise$X1[i]*60 +splitted_timesunrise$X2[i]  
}

#Making arrangements on TimeSunSet column
timesunset = mydata[,8]
timesunset = as.character(timesunset)
str(timesunset)
splitted_timesunset = timesunset %>% str_split(":", n = 3, simplify = TRUE)
str(splitted_timesunset)
splitted_timesunset = data.frame(splitted_timesunset,stringsAsFactors = FALSE)
colnames(splitted_timesunset)
splitted_timesunset$X1 = as.numeric(splitted_timesunset$X1)
splitted_timesunset$X2 = as.numeric(splitted_timesunset$X2)
splitted_timesunset$X3 = as.numeric(splitted_timesunset$X3)
str(splitted_timesunset)

endingtime = rep(NA,32686)
for(i in 1:32686){
  endingtime[i] = splitted_timesunset$X1[i]*60 +splitted_timesunset$X2[i]  
}

mydata$TimeSunRise = beginningtime
mydata$TimeSunSet = endingtime

mydata = data.frame(mydata,month = month_day$X1 , day = month_day$X2) 

dim(mydata)
summary(mydata)

ggplot(mydata, aes(day, Radiation)) + 
  geom_boxplot() + 
  facet_grid(.~month) +
  geom_jitter(aes(color = factor(month)),alpha = 0.3) +
  ggtitle("Boxplot of Radiation values for each month") +
  theme_light()

september = mydata %>% filter( month == 9 )
october = mydata %>% filter( month == 10 )
november = mydata %>% filter( month == 11 )
december = mydata %>% filter( month == 12 ) 

mean(september$Radiation)
mean(october$Radiation)
mean(november$Radiation)
mean(december$Radiation)

# september, october, december are close to each other. November is the coolest month.
head(mydata)

K <- cor(mydata[,1:10])
K
corrplot(K, method="circle")

# according to the plot, we can say that temperature is positively correlated with radiation, 0.7349
# according to the plot, we can say that temperature and pressure are positively correlated, 0.3111
# according to the plot, we can say that humidity and temperature are negatively correlated, -0.2850
# according to the plot, we can say that timesunset and timesunrise are negatively correlated, -0.7382
# according to the plot, we can say that month and timesunset are positively correlated , 0.5017

## Manipulating temperature
mydata$Temperature = (mydata$Temperature - 32)* (5/9)

mydata$month = as.factor(mydata$month)
sun_duration = mydata$TimeSunSet - mydata$TimeSunRise
mydata = mydata[,-c(7,8)]
mydata = data.frame(mydata,sun_duration)
str(mydata)

## split data into a train and test set
index <- 1:nrow(mydata)
testindex <- sample(index, trunc(length(index)/4))
testset <- mydata[testindex,]
trainset <- mydata[-testindex,]

lm.fit= lm(Radiation ~ poly(Temperature,4) + Humidity + month + sun_duration ,data=trainset) 
lm.fit$coefficients
summary(lm.fit)

## 64.13% adjusted r-squared

par(mfrow = c(2,2))
plot(lm.fit)
wp(lm.fit)


plot(mydata)

















