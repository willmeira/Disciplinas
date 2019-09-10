# LINK do material http://leg.ufpr.br/~lucambio/STemporais/STemporaisI.html

install.packages('astsa')

library(astsa)


##########################################################################################
## I. Características da série temporal
##########################################################################################

###------------------------------------------------------------------------------------###
### I.1 A natureza dos dados da série temporal
###------------------------------------------------------------------------------------###

#-----------------------------------------------------------------------------------------
# Exemplo I.1. Lucro trimestral da Johnson & Johnson
#-----------------------------------------------------------------------------------------
par(mar=c(4,4,3,1))
plot(jj, type="o", xlab="Tempo", ylab="Lucro trimestral por ação", pch=19,
       main="Johnson & Johnson ganhos trimestrais por ação\n 84 trimestres, 1960-I a 1980-IV")
jj
as.numeric(jj)
jj1 = ts(as.numeric(jj), start = c(1960,1), frequency = 4)
jj1


#-----------------------------------------------------------------------------------------
# Exemplo I.2. Aquecimento global
#-----------------------------------------------------------------------------------------
par(mar=c(4,4,3,1))
plot(gtemp, type="o", xlab="Tempo", ylab="Desvios Globais de Temperatura", pch=19, 
       main="Desvios de temperatura global média anual entre 1880 e 2009\n em graus centígrados")


#-----------------------------------------------------------------------------------------
# Exemplo I.3. Dados de fala
#-----------------------------------------------------------------------------------------
par(mar=c(4,4,3,1))
plot(speech, type="l", xlab="Tempo", ylab="Articulação",
       main="Gravação da fala da sílaba aaaa ... hhhh\n amostrada a 10.000 
	pontos por segundo, com n = 1020 pontos")


#-----------------------------------------------------------------------------------------
# Exemplo I.4. Bolsa de Valores de Nova Iorque
#-----------------------------------------------------------------------------------------
par(mar=c(4,4,3,1))
plot(nyse, xlab="Tempo", ylab="Retorno da Bolsa de Valores de Nova Iorque (NYSE)",
       main="Retornos de mercado ponderados pelo valor diário\n 
	  entre 2 de fevereiro de 1984 e 31 de dezembro de 1991")


#-----------------------------------------------------------------------------------------
# Exemplo I.5. El Niño e a População de Peixes
#-----------------------------------------------------------------------------------------
par(mar=c(3,3,1,1), mfrow=c(2,1)) # a opção mfrow=c(2,1) divide o mesmo gráfico em duas linhas
plot(soi, ylab="", xlab="", main="Índice de Oscilação do Sul")
plot(rec, ylab="", xlab="", main="Recrutamento")

## adicionando espaço extra à margem direita do gráfico dentro do quadro
par(mar=c(3, 6, 1, 1) + 0.1)

## Plotando o primeiro conjunto de dados e desenhando seu eixo
plot(soi, pch=16, axes=FALSE, ylim=c(-1,1), xlab="", ylab="", type="b", col="black", main="")
axis(2, ylim=c(-1,1), col="black", las=1)  ## las=1 faz etiquetas horizontais
mtext("Índice de Oscilação do Sul", side=2, line=2.5)
box()
par(new=TRUE)

## Plotando o segundo gráfico e colocando a escala do eixo à direita
plot(rec, pch=15, xlab="", ylab="", ylim=c(0,100), axes=FALSE, type="b", col="red")
axis(2, ylim=c(0,100), lwd=2, line=3.5, col="red", col.axis = "red")
mtext("Recrutamento", side=2, line=5.3, col="red")


#-----------------------------------------------------------------------------------------
# Exemplo I.6. Imagem fMRI
#-----------------------------------------------------------------------------------------
par(mfrow=c(2,1), mar=c(3,2,1,0)+.5, mgp=c(1.6,.6,0))
ts.plot(fmri1[,2:5], lty=c(1,2,4,5), ylab="BOLD", xlab="", main="Córtex")
ts.plot(fmri1[,6:9], lty=c(1,2,4,5), ylab="BOLD", xlab="", main="Tálamo e Cerebelo")
mtext("Tempo (1 pt = 2 seg)", side=1, line=2)


#-----------------------------------------------------------------------------------------
# Exemplo I.7. Terremotos e explosões
#-----------------------------------------------------------------------------------------
par(mfrow=c(2,1), mar=c(3,2,1,0)+.5, mgp=c(1.6,.6,0))
plot(EQ5, xlab="Tempo", main="Terremoto")
plot(EXP6, xlab="Tempo", main="Explosão")


###------------------------------------------------------------------------------------###
### I.2 Modelos estatísticos de séries temporais
###------------------------------------------------------------------------------------###

#-----------------------------------------------------------------------------------------
# Exemplo I.8. Ruído branco
#-----------------------------------------------------------------------------------------
w = rnorm(500,0,1) # 500 N(0,1) variáveis
par(mfrow=c(1,1),mar=c(4,3,1,1),mgp=c(1.6,.6,0))
plot.ts(w, xlab="Tempo", main="Ruído branco")
grid()


#-----------------------------------------------------------------------------------------
# Exemplo I.9. Médias Móveis
#-----------------------------------------------------------------------------------------
v = filter(w, sides=2, rep(1/3,3)) # médias móveis
par(mfrow=c(1,1),mar=c(4,3,1,1),mgp=c(1.6,.6,0))
plot.ts(v, xlab="Tempo", main="Médias móveis")
grid()


#-----------------------------------------------------------------------------------------
# Exemplo I.10. Autoregressões
#-----------------------------------------------------------------------------------------
w = rnorm(550,0,1) # 50 extras para evitar problemas de inicialização
x = filter(w, filter=c(1,-.9), method="recursive")[-(1:50)] # removendo os primeiros 50
plot.ts(x, xlab="Tempo", main="Autoregressão")
grid()


#-----------------------------------------------------------------------------------------
# Exemplo I.11. Passeio aleatória com tendência
#-----------------------------------------------------------------------------------------
set.seed(154) # para que você possa reproduzir os resultados
w = rnorm(200); x = cumsum(w) # dois comandos em uma linha
wd = w +.2; xd = cumsum(wd)
plot.ts(xd, xlab="Tempo", ylim=c(-5,55), main="Passeio aleatório", ylab='')
lines(x, col=4); abline(h=0, col=4, lty=2); abline(a=0, b=.2, lty=2)
grid()


#-----------------------------------------------------------------------------------------
# Exemplo I.12. Sinal no ruído
#-----------------------------------------------------------------------------------------
cs = 2*cos(2*pi*1:500/50 + .6*pi); w = rnorm(500,0,1)
par(mfrow=c(3,1), mar=c(3,3,2,1), cex.main=1.5)
plot.ts(cs, main=expression(2*cos(2*pi*t/50+.6*pi)))
grid()
plot.ts(cs+w, main=expression(2*cos(2*pi*t/50+.6*pi) + N(0,1)))
grid()
plot.ts(cs+5*w, main=expression(2*cos(2*pi*t/50+.6*pi) + N(0,25)))
grid()

#-----------------------------------------------------------------------------------------
# Trabalho - Parte I - Exercícios 1 , 2, 4, 5, 17 e 18
#-----------------------------------------------------------------------------------------




#-----------------------------------------------------------------------------------------
# Trabalho - Parte  II - Exercícios 6(a) e (b), 9 e 10
#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
# Exemplo
#-----------------------------------------------------------------------------------------






































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


