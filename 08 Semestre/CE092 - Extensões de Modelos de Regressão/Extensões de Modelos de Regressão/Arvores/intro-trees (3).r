require(faraway)
data(ozone)
head(ozone)

plot(ozone)
summary(ozone)

with(ozone, plot(O3 ~temp))

oztemp <- ozone[, c("O3", "temp")]
oztemp <- with(oztemp, oztemp[order(temp),])

aov(O3 ~ 1, oztemp)
aov(O3 ~ cut(temp, c(20, 50, 100)), oztemp)
aov(O3 ~ cut(temp, c(20, 60, 100)), oztemp)
aov(O3 ~ cut(temp, c(20, 70, 100)), oztemp)

m <- aov(O3 ~ cut(temp, c(20, 50, 100)), oztemp)
names(m)
anova(m)
names(anova(m))
anova(m)[[2]]
anova(m)[["Sum Sq"]][2]

## código "amador", não otimizado!
rssC <- function(pc, y, x)
    anova(aov(O3 ~ cut(temp, c(min(x), pc, max(x)), include=TRUE), oztemp))[["Sum Sq"]][2]

with(oztemp, rssC(50, O3, temp))
with(oztemp, rssC(60, O3, temp))
with(oztemp, rssC(70, O3, temp))

foo <- c(50,60,70)
with(oztemp, sapply(foo, rssC, y=O3, x=temp))

(aptemp <- unique(oztemp$temp))
(cortes <- aptemp[-length(aptemp)]+diff(aptemp)/2)

rss1 <- with(oztemp, sapply(cortes, rssC, y=O3, x=temp))

plot(cortes, rss1, type="b")
(c1 <- cortes[which.min(rss1)])

with(oztemp, plot(O3 ~temp))

oztemp <- transform(oztemp, I1 = ifelse(temp < c1, 0, 1))
(fit1 <- lm(O3 ~ I1, data=oztemp))

noztemp <- with(oztemp, data.frame(temp=seq(min(temp), max(temp), l=501)))
noztemp <- transform(noztemp, I1 = ifelse(temp < c1, 0, 1))
noztemp <- transform(noztemp, y1 = predict(fit1, newdata=noztemp))

with(noztemp, lines(y1 ~ temp, col=2))


oztemp2 <- subset(oztemp, temp < c1)
oztemp3 <- subset(oztemp, temp >= c1)

dim(oztemp2)
dim(oztemp3)



