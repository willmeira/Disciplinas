install.packages('astsa')

library(astsa)
par(mar=c(4,4,3,1))
plot(jj, type="o", xlab="Tempo", ylab="Lucro trimestral por ação", pch=19,
       main="Johnson & Johnson ganhos trimestrais por ação\n 84 trimestres, 1960-I a 1980-IV")


w = rnorm(500,0,1) # 500 N(0,1) variáveis
par(mfrow=c(1,1),mar=c(4,3,1,1),mgp=c(1.6,.6,0))
plot.ts(w, xlab="Tempo", main="Ruído branco")
grid()



v = filter(w, sides=2, rep(1/3,3)) # médias móveis
par(mfrow=c(1,1),mar=c(4,3,1,1),mgp=c(1.6,.6,0))
plot.ts(v, xlab="Tempo", main="Médias móveis")
grid()


w = rnorm(550,0,1) # 50 extras para evitar problemas de inicialização
x = filter(w, filter=c(1,-.9), method="recursive")[-(1:50)] # removendo os primeiros 50
plot.ts(x, xlab="Tempo", main="Autoregressão")
grid()


