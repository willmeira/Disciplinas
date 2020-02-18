#
# Considere a anomalia global da temperatura média do ar na superfície e os dados globais de 
# concentração de C02. Ajuste um modelo apropriado a esses dados, assumindo que a concentração 
# de C02 seja a variável de entrada.
#
# O termo anomalia de temperatura significa um desvio de um valor de referência ou média de longo 
# prazo. Uma anomalia positiva indica que a temperatura observada foi mais quente que o valor de 
# referência, enquanto uma anomalia negativa indica que a temperatura observada foi mais baixa que 
# o valor de referência.
#
# Este produto é uma ferramenta de diagnóstico climático em escala global e fornece uma visão geral 
# das temperaturas globais médias em comparação com um valor de referência.
#
# As temperaturas da superfície da terra estão disponíveis na Rede Global de Clima Histórico 
# Mundial (GHCNm). As temperaturas da superfície do mar são determinadas usando a análise de 
# temperatura da superfície do mar reconstruída estendida (ERSST). O ERSST usa o mais recente 
# conjunto de dados abrangente sobre a atmosfera do oceano (ICOADS) e métodos estatísticos que 
# permitem reconstrução estável usando dados esparsos. A análise mensal começa em janeiro de 1854, 
# mas devido a dados muito esparsos, nenhuma média global é calculada antes de 1880. Com mais 
# observações após 1880, o sinal é mais forte e mais consistente ao longo do tempo.
#
# Estimativas absolutas da temperatura média global da superfície são difíceis de compilar por vários 
# motivos. Algumas regiões têm poucas estações de medição de temperatura (por exemplo, o deserto do 
# Saara) e a interpolação deve ser feita em grandes regiões com poucos dados. Nas áreas montanhosas, 
# a maioria das observações vem dos vales habitados; portanto, o efeito da elevação na temperatura 
# média de uma região também deve ser considerado. Por exemplo, um mês de verão em uma área pode ser 
# mais frio que a média, tanto no topo de uma montanha quanto em um vale próximo, mas as temperaturas 
# absolutas serão bem diferentes nos dois locais. O uso de anomalias nesse caso mostrará que as 
# temperaturas para os dois locais estavam abaixo da média.
#
# O uso de valores de referência calculados em escalas, mais locais, menores no mesmo período estabelece 
# uma linha de base a partir da qual as anomalias são calculadas. Isso normaliza efetivamente os dados 
# para que possam ser comparados e combinados para representar com mais precisão os padrões de temperatura 
# em relação ao normal para diferentes locais dentro de uma região.
#
# Por esses motivos, os resumos de grandes áreas incorporam anomalias, não a temperatura em si. As 
# anomalias descrevem com mais precisão a variabilidade climática em áreas maiores do que as temperaturas 
# absolutas e fornecem um quadro de referência que permite comparações mais significativas entre locais 
# e cálculos mais precisos das tendências de temperatura.
#
# A série temporal global é produzida a partir do conjunto de dados terrestres e oceânicos combinados de 
# Smith and Reynolds (Smith et al., 2008). Esse conjunto de dados consiste em anomalias de temperatura 
# média mensal em uma grade de 5°x5° em superfícies terrestres e oceânicas. Essas caixas de grade são 
# então calculadas para fornecer uma anomalia média da temperatura global. Um esquema de área ponderada 
# é usado para refletir a realidade de que as caixas são menores próximas aos pólos e maiores próximas 
# ao equador. As anomalias da média global são calculadas em uma escala de tempo mensal e anual. Anomalias 
# de temperatura média também estão disponíveis para superfícies terrestres e oceânicas separadamente 
# e os hemisférios norte e sul separadamente. As anomalias globais e hemisféricas são fornecidas em 
# relação ao período 1901-2000, a média do século XX.
#
# A média global de dióxido de carbono atmosférico em 2018 foi de 407.4 partes por milhão (ppm para abreviar), 
# com uma faixa de incerteza de mais ou menos 0,1 ppm. Hoje, os níveis de dióxido de carbono são mais 
# altos do que em qualquer ponto dos últimos 800.000 anos.

Anomaly = c(-0.11,-0.13,-0.01,-0.04,-0.42,-0.23,-0.25,-0.45,-0.23,0.04,-0.22,-0.55,
            -0.40,-0.39,-0.32,-0.32,-0.27,-0.15,-0.21,-0.25,-0.05,-0.05,-0.30,-0.35,
            -0.42,-0.25,-0.15,-0.41,-0.30,-0.31,-0.21,-0.25,-0.33,-0.28,-0.02,0.06,
            -0.20,-0.46,-0.33,-0.09,-0.15,-0.04,-0.09,-0.16,-0.11,-0.15,0.04,-0.05,0.01,
            -0.22,-0.03,0.03,0.04,-0.11,0.05,-0.08,0.01,0.12,0.15,-0.02,0.14,0.11,
            0.10,0.06,0.10,-0.01,0.01,0.12,-0.03,-0.09,-0.17,-0.02,0.03,0.12,-0.09,-0.09,
            -0.18,0.08,0.10,0.05,-0.02,0.10,0.05,0.03,-0.25,-0.15,-0.07,-0.02,-0.09,
            0.00,0.04,-0.10,-0.05,0.18,-0.06,-0.02,-0.21,0.16,0.07,0.13,0.27,0.40,
            0.10,0.34,0.16,0.13,0.19,0.35,0.42,0.28,0.49,0.44,0.16,0.18,0.31,0.47,
            0.36,0.40,0.71,0.43,0.41,0.56,0.70,0.66,0.60)


CO2 = c(290.7,291.2,291.7,292.1,292.6,293.0,293.3,293.6,293.8,294.0,294.2,294.3,
        294.5,294.6,294.7,294.8,294.9,295.0,295.2,295.5,295.8,296.1,296.5,296.8,
        297.2,297.6,298.1,298.5,298.9,299.3,299.7,300.1,300.4,300.8,301.1,301.4,301.7,
        302.1,302.4,302.7,303.0,303.4,303.8,304.1,304.5,305.0,305.4,305.8,306.3,306.8,307.2,
        307.7,308.2,308.6,309.0,309.4,309.8,310.0,310.2,310.3,310.4,310.4,310.3,
        310.2,310.1,310.1,310.1,310.2,310.3,310.5,310.7,311.1,311.5,311.9,312.4,
        313.0,313.6,314.2,314.9,315.8,316.6,317.3,318.1,318.7,319.2,320.0,321.1,
        322.0,322.9,324.2,325.2,326.1,327.2,328.8,329.7,330.7,331.8,333.3,334.6,
        336.9,338.7,339.9,341.1,342.8,344.4,345.9,347.2,348.9,351.5,352.9,354.2,
        355.6,356.4,357.0,358.9,360.9,362.6,363.8,366.6,368.3,369.5,371.0,373.1,375.6,377.4)
# Fonte dos dados: http://data.giss.nasa.gov.gistemp/

Anomaly = ts(Anomaly, start = 1880, end = 2004, frequency = 1)
CO2 = ts(CO2, start = 1880, end = 2004, frequency = 1)

#
# Estudo descritivo
#

par(mar=c(3, 7, 1, 1) + 0.1)
## Plotando o primeiro conjunto de dados e desenhando seu eixo
plot.ts(Anomaly, pch=16, axes=FALSE, ylim=c(-0.6,0.8), xlab="", ylab="", type="b", col="black", main="")
axis(2, ylim=c(-1,1), col="black", las=1)  ## las=1 faz etiquetas horizontais
mtext("Anomalia global da temperatura média do ar na superfície em °C", side=2, line=2.5)
box()
par(new=TRUE)
## Plotando o segundo gráfico e colocando a escala do eixo à direita
plot.ts(CO2, pch=15, xlab="", ylab="", ylim=c(290,380), axes=FALSE, type="b", col="red")
axis(2, ylim=c(0,100), lwd=2, line=3.5, col="red", col.axis = "red")
mtext(expression(paste(CO[2]," em ppmv")), side=2, line=5.3, col="red")
axis(1)
grid()

library(astsa)

#
# Considerando somente os dados a partir de 1965
#

which(time(Anomaly)==1965.000)  # 86

Anomaly1 = ts(Anomaly[86:length(Anomaly)], start = 1965, freq = 1)
CO21 = ts(CO2[86:length(CO2)], start = 1965, freq = 1)

par(mar=c(3, 7, 1, 1) + 0.1)
## Plotando o primeiro conjunto de dados e desenhando seu eixo
plot.ts(Anomaly1, pch=16, axes=FALSE, ylim=c(-0.6,0.8), xlab="", ylab="", type="b", col="black", main="")
axis(2, ylim=c(-1,1), col="black", las=1)  ## las=1 faz etiquetas horizontais
mtext("Anomalia global da temperatura média do ar na superfície em °C", side=2, line=2.5)
box()
par(new=TRUE)
## Plotando o segundo gráfico e colocando a escala do eixo à direita
plot.ts(CO21, pch=15, xlab="", ylab="", ylim=c(290,380), axes=FALSE, type="b", col="red")
axis(2, ylim=c(0,100), lwd=2, line=3.5, col="red", col.axis = "red")
mtext(expression(paste(CO[2]," em ppmv")), side=2, line=5.3, col="red")
axis(1)
grid()


par(mar=c(4, 4, 1, 2))

global.temp = ts.intersect(Resp = Anomaly1, Resp1 = lag(Anomaly1,-1), Resp2 = CO21)
(adj01 = lm(Resp ~ Resp1+Resp2, data = global.temp, na.action=NULL))

acf2(resid(adj01)) # sugere levemente AR(5)

adj02 = sarima(global.temp[,1], 5, 0, 0, xreg = global.temp[,2:3]) 

adj02[["ttable"]]

library(forecast)

adj03 = Arima(global.temp[,1], order = c(5, 0, 0), xreg = global.temp[,2:3])

par(mar=c(4, 4, 1, 2))
ts.plot(Anomaly1, fitted(adj03), col = c(6,4), xlab="Tempo")
grid()
