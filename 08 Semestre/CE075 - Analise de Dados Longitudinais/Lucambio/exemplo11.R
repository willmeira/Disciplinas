#
# Exemplos do pacote HSAUR
#
data("BtheB", package = "HSAUR")
#
# Dados longitudinais de um ensaio clínico de um programa interativo, multimídia 
# conhecido como "Beat the Blues" projetado para oferecer terapia cognitiva 
# comportamental para pacientes com depressão por meio de um terminal de 
# computador. Os doentes com depressão recrutados na atenção primária foram 
# randomizados para o programa "Beating the Blues" ou "Tratamento usual (TAU)".
#
# Variáveis:
# drug: se o paciente toma antidepressivo (No or Yes).
# length: a duração do episódio actual de depressão, um factor com níveis
#          <6m (less than six months) and >6m (more than six months).
# treatment: fator de níveis TAU (Tratamento usual) e BtheB (Beat the Blues)
# bdi.pre: Escala de depressão de Beck II antes do tratamento
# bdi.2m: Escala de depressão de Beck II depois de dois meses
# bdi.4m: Escala de depressão de Beck II depois de quatro meses
# bdi.6m: Escala de depressão de Beck II depois de seis meses
# bdi.8m: Escala de depressão de Beck II depois de oito meses
#
# Preparando a base de dados para ser interpretada como longitudinal
#
BtheB$subject = factor(rownames(BtheB))
nobs = nrow(BtheB)
BtheB_long = reshape(BtheB, idvar = "subject",
                      varying = c("bdi.2m", "bdi.4m", "bdi.6m", "bdi.8m"), direction = "long")
#
BtheB_long$time = rep(c(2, 4, 6, 8), rep(nobs, 4))
#
# Mostrando os resultados para os tres primeiros individuos (subject's)
#
subset(BtheB_long, subject %in% c("1", "2", "3"))
#
# Estudo descritivo
#
library(lattice)
xyplot(bdi~time |length, groups=subject, xlab="Tempo" ,data=BtheB_long, type='l',
       main="Resultado na escala de depressão de Beck \n segundo duração do episódio actual de depressão")
#
xyplot(bdi~time |treatment, groups=subject, xlab="Tempo" ,data=BtheB_long, type='l',
       main="Resultado na escala de depressão de Beck \n segundo o tratamento ao qual foram alocados")
#
xyplot(bdi~time |drug, groups=subject, xlab="Tempo" ,data=BtheB_long, type='l',
       main="Resultado na escala de depressão de Beck \n segundo o paciente tome ou não antidepressivos")
#
# Percebe-se que na primeira situação, a resposta segundo a duração do episódio atual de depressão, os 
# pacientes com episódio mais prolongado tiveram respostas superiores durante o estudo. Nas outras duas 
# situações não foram percebidas diferenças entre os grupos.
#
# Uma outra forma de apresentar a resposta é através de gráficos de box-plot, para isso organiza-se a 
# base de dados da seguinte forma.
#
ylim = range(BtheB[,grep("bdi", names(BtheB))],na.rm = TRUE)
#
tau = subset(BtheB, treatment == "TAU")[,grep("bdi", names(BtheB))]
#
# Para apresentar os dois gráficos na mesma janela fazemos
#
layout(matrix(1:2, nrow = 1))
#
boxplot(tau, main = "Treated as usual", ylab = "BDI",
        xlab = "Time (in months)", names = c(0, 2, 4, 6, 8),ylim = ylim)
#
btheb = subset(BtheB, treatment == "BtheB")[,grep("bdi", names(BtheB))]
#
boxplot(btheb, main = "Beat the Blues", ylab = "BDI",
        xlab = "Time (in months)", names = c(0, 2, 4, 6, 8),lim = ylim)
#
# Utilizaremos modelos de regressão mistos, isto é, modelos de regressão normais com efeitos fixos e 
# aleatórios. A utilização dos efeitos aleatórios objetiva absorver a variabilidade dos dados para melhor
# estimação e verificação da influência dos efeitos fixos na resposta.
#
attach(BtheB_long)
library("nlme")
#
# A primeira observção é de que a variabilidade ou dispersão entre as resposta seja causa das próprias 
# unidades experimentais (pacientes), por isso consideramos coo efeito aleatório cada paciente
#
BtheB.1 = lme(bdi ~ bdi.pre + time + treatment + drug + length, random=~ 1 | subject, na.action = na.omit)
#
# Agora vamos considerar que a dispersão das respostas dependa do instante de tempo e não mais de cada 
# paciente
#
BtheB.2 = lme(bdi ~ bdi.pre + time + treatment + drug + length, random=~ factor(time) | subject,na.action = na.omit)
#
# Vejamos se os modelos diferem
#
anova(BtheB.1, BtheB.2)
#
# Concluimos que os modelos são semelhantes, por isso, escolhemos o mais simples
#
AIC(BtheB.1,BtheB.2)
#
# Desta forma escolhemos como mais adequado o modelo em BtheB.1
#
summary(BtheB.1, cor=FALSE)
intervals(BtheB.1, which="fixed")
#
# Observamos que a variável length não é significativa, a retiramos do modelo
#
BtheB.3 = update(BtheB.1, ~.-length)
#
summary(BtheB.3, cor=FALSE)
intervals(BtheB.3, which="fixed")
#
# Decidimos que a tratamento (treatment) permaneça no modelo, pelo interesse experimental
#
# Vejamos os resíduos
#
library(car)
#
qqPlot(residuals(BtheB.3), ylab="Resíduos", xlab="Quantis normais")
#
plot(BtheB.3, subject ~ resid(.), cex=0.8)
#
plot(BtheB.3, bdi ~ resid(.), pch=19, cex=0.8)
#
# Aceitamos o modelo em BtheB.3
#
# Coeficientes estimados nos termos fixos
#
fixef(BtheB.3)
#
# Apresentando os resultados
#
linha1 = c(fixef(BtheB.3)%*%c(1,mean(bdi.pre),2,1,1),fixef(BtheB.3)%*%c(1,mean(bdi.pre),4,1,1),
           fixef(BtheB.3)%*%c(1,mean(bdi.pre),6,1,1),fixef(BtheB.3)%*%c(1,mean(bdi.pre),8,1,1))
#
linha2 = c(fixef(BtheB.3)%*%c(1,mean(bdi.pre),2,0,1),fixef(BtheB.3)%*%c(1,mean(bdi.pre),4,0,1),
           fixef(BtheB.3)%*%c(1,mean(bdi.pre),6,0,1),fixef(BtheB.3)%*%c(1,mean(bdi.pre),8,0,1))
#
linha3 = c(fixef(BtheB.3)%*%c(1,mean(bdi.pre),2,1,0),fixef(BtheB.3)%*%c(1,mean(bdi.pre),4,1,0),
           fixef(BtheB.3)%*%c(1,mean(bdi.pre),6,1,0),fixef(BtheB.3)%*%c(1,mean(bdi.pre),8,1,0))
#
linha4 = c(fixef(BtheB.3)%*%c(1,mean(bdi.pre),2,0,0),fixef(BtheB.3)%*%c(1,mean(bdi.pre),4,0,0),
           fixef(BtheB.3)%*%c(1,mean(bdi.pre),6,0,0),fixef(BtheB.3)%*%c(1,mean(bdi.pre),8,0,0))
#
y1 = min(linha1,linha2,linha3,linha4)
y2 = max(linha1,linha2,linha3,linha4)
#
par(mar=c(4,5,1,1))
plot(c(2,4,6,8), linha1, ylim=c(y1-1,y2+1), xlab="Tempos", 
     ylab="Resposta média estimada \n ao teste de depressão de Beck", type="l")
lines(c(2,4,6,8), linha2, col="red")
lines(c(2,4,6,8), linha3, col="green")
lines(c(2,4,6,8), linha4, col="yellow")
#
legend(4.5,20, legend=c("TAU sem droga","Beat the Blues sem droga"), col=c("yellow","green"), lty=1, cex=0.7)
legend(2,11, legend=c("Beat the Blues com droga","TAU com droga"), col=c("black","red"), lty=1, cex=0.7)
