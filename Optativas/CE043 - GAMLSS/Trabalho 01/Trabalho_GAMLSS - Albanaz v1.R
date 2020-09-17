require(gamlss)
require(GGally)

dados <- read.csv2("E:\\Install\\Docs\\UFPR\\CE043\\Trabalho\\SolarPredictionAjustada.csv")

dados$Radiation <- as.numeric(dados$Radiation)
dados$Speed <- as.numeric(dados$Speed)
dados$Pressure <- as.numeric(dados$Pressure)
dados$WindDirection <- as.factor(dados$WindDirection)
head(dados)
summary(dados)
names(dados)

#Selecionando somente as variáveis que utilizaremos para a análise
dados <- dados[dados$DayNight==1,] #somente dia
dados <- dados[c("Radiation","Temperature","Pressure","Speed","WindDirection","Month","TimeHour")]

x11(width = 10, height = 10)
par(cex = 1.2, las = 1)

#Vamos ver como a variável Radiation se comporta
truehist(dados$Radiation)

#Verificar a correlação entre as variáveis
ggpairs(dados)

#analisando o comportamento de cada covariável em relação à variável resposta
x11(width = 10, height = 10)
par(mfrow=c(2,3))
plot(Radiation ~ Temperature, data=dados)
plot(Radiation ~ Pressure, data=dados)
plot(Radiation ~ Speed, data=dados)
plot(Radiation ~ WindDirection, data=dados)
plot(Radiation ~ Month, data=dados)
plot(Radiation ~ TimeHour, data=dados)


par(cex = 1.2, las = 1)

#Vamos começar a fazer os ajustes com uma regressão linear
mod_lm <- gamlss(Radiation ~ Temperature + Pressure + Speed + WindDirection + Month + TimeHour, data = dados)
plot(mod_lm)
wp(mod_lm, ylim.all = 0.8)
GAIC(mod_lm, k=2)


#tentando encaixar em uma distribuição exponencial
mod_exp <- gamlss(Radiation ~ Temperature + Pressure + Speed + WindDirection + Month + TimeHour, data = dados, family = EXP)
plot(mod_exp)
wp(mod_exp, ylim.all = 0.8)
GAIC(mod_lm, mod_exp, k=2)

#tentando ajustar em gama
mod_ga <- gamlss(Radiation ~ Temperature + Pressure + Speed + WindDirection + Month + TimeHour, data = dados, family = GA)
plot(mod_ga)
wp(mod_ga, ylim.all = 8, xlim.all = 8)
GAIC(mod_lm, mod_exp, mod_ga, k=2)

#utilizando a funçãO fitDist para identificar as distribuições que melhor se encaixam na variável resposta
fit <- fitDist(Radiation, data=dados, type="realplus")
head(fit$fits) #GB2, BCPE, BCPEo

mod_gb2 <- gamlss(Radiation ~ Temperature + Pressure + Speed + WindDirection + Month + TimeHour, data = dados, family = GB2)
mod_gb2 <- refit(mod_gb2)
mod_gb2 <- update(mod_gb2,method=CG(),c.crit=0.1)
plot(mod_gb2)
wp(mod_gb2, ylim.all = 8, xlim.all = 8)
GAIC(mod_lm, mod_exp, mod_ga, mod_gb2, k=2)

mod_gb2_sig <- update(mod_gb2, sigma.fo = Radiation ~ Temperature + Pressure + Speed + WindDirection + Month + TimeHour, 
                      data = dados, method=RS(),c.crit=0.1)
mod_gb2_sig <- refit(mod_gb2_sig, method=CG(),c.crit=0.1)

