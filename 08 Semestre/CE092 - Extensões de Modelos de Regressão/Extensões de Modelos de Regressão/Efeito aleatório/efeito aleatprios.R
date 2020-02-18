## Ver: https://rpubs.com/melinatarituba/309285



Lab <- c("A","A","A","B","B","B","B","B","C","C","C","D","D","D","E","E","E","F","F","F","G","G","G","H","H","H")
MgO <- c(1.86,1.88,1.86,2.00,2.00,1.99,2.02,2.01,1.84,1.83,1.83,1.64,1.73,1.68,0.28,0.31,0.68,
         1.88,1.87,1.86,1.87,1.87,1.86,1.85,1.86,1.85)


mgo <- data.frame(Lab,MgO)
summary(mgo)
mgo$Lab <- as.character(Lab)

mod1 <- lm(MgO~Lab, data = mgo)
summary(mod1)

require(lme4)
require(nlme)

mgo.lme <- lme(MgO ~ 1, random= ~1 | Lab, mgo) 

mgo.lme$coeff$fixed

mgo.lmer <- lmer(MgO ~ (1 | Lab), mgo) 
mgo.lme$res

summary(mgo.lme)
summary(mgo.lmer)
lme4::

coflme <- fixef(mgo.lme)
cof <- fixef(mgo.lmer)

novo <- expand.grid(Lab=unique(mgo$Lab),
                    NAP = seq(-1.3,2.2,0.5))

preditoslme <- predict(mgo.lme, newdata = novo)
preditoslmer <- predict(mgo.lmer, newdata = novo)

dados.preditoslme <- data.frame(pred = preditoslme, novo)
dados.preditoslmer <- data.frame(pred = preditoslmer, novo)

library(ggplot2)

funi <- function(x){cof[1]+cof[2]*x} # para plotar a predição dos efeitos fixos
mgo$Lab = as.factor(mgo$Lab)# transformando laboratório em fator

ggplot(data = mgo, aes(y = MgO,x=1:26, color = Lab)) + # dados e eixos
  geom_point(size = 3, shape = 19) +  # plotando os pontos dos laboratórios 
  geom_line(data = dados.preditoslmer, aes(y = pred, x=0,
                                       col = as.factor(Lab))) + # retas de cada praia
  stat_function(fun = funi, col = "black", size = 2) +  # reta do modelo fixo 
  scale_color_brewer(palette = "Set1") + # a partir daqui estética do gráfico
  theme_bw() +
  theme(axis.text = element_text(size = 13), 
        axis.title = element_text(size = 15),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        legend.key.size = unit(0.6, "cm"),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13)) +
  xlab("") +
  ylab("Mgo")


plot(mgo1$MgO, col=mgo1$Lab)
legend("topright", col=1:8, lwd=5,
       legend=levels(mgo$Lab),bty = "n")
