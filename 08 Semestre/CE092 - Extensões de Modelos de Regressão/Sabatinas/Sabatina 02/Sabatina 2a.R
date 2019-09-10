#ATENÇÃO. O Moodle faz correção automaticamente. Considere que hajam n sentenças, das quais m estão corretas e o valor total da questão 1. O moodle considera que marcar k alternativas incorretas desconta k/(n???m) na pontuação acumulada em alternativas corretas. Portanto, evite "chutes". Caso não tenha total segurança se a afirmação é verdadeira, deixe-a em branco.

#Considere os dados disponíveis no objeto tb1 criado com o código R do bloco a seguir.

tb1 <- data.frame(x = seq(0, 2, by = 0.1),
                  y1 = c(5.5, 7.2, 10.4, 7.4, 7.2, 9.1, 15.9, 12.7,
                         11.3, 14.4, 14.8, 17.1, 31.4, 17.5, 27.6, 19.6,
                         27.1, 21, 33.8, 30.2, 45.1),
                  y2 = c(0.8, 1.5, 2.6, 1.5, 1.4, 2.1, 3.9, 3.3, 3, 3.9,
                         4.2, 4.8, 6.9, 5.5, 7.3, 6.7, 8.2, 8.1, 10.2,
                         10.7, 12.8))

library(lattice)
library(latticeExtra)

xyplot(y1 + y2 ~ x, data = tb1, outer = TRUE, scales = "free")

#Digrama de dispersão.

#Essa tabela de dados contém os valores de duas variáveis respostas, y1 e y2, como função de uma variável quantitativa x. Apesar de estarem na mesma tabela, y1 e y2 não são medidas observadas juntas mas sim de experimentos diferentes mas coincidentemente observadas sob os mesmos valores da variável independente x.

#Considerando tais variáveis, ajuste os modelos listados abaixo para assinalar as sentenças adiante.

#Modelo gaussiano para y1 com preditor linear em x e função de ligação canônica.
#Modelo gaussiano para y1 com preditor quadrático em x e função de ligação canônica.
#Modelo gaussiano para log(y1) com preditor linear em x e função de ligação canônica.
#Modelo gaussiano para y1????????? com preditor linear em x e função de ligação canônica.
#Modelo gaussiano para BoxCox(y1)=y??1???1?? com preditor linear em x e função de ligação canônica. O valor de ?? é o que otimiza a log-verossimilhança perfilhada. Veja MASS::boxcox().
#Modelo gama para y1 com preditor linear em x e função de ligação canônica.
#Modelo gaussiano inverso para y1 com preditor linear em x e função de ligação canônica.
#Modelo gaussiano para y1 com preditor linear em x e função de ligação logarítmica.
#Para cada modelo dessa lista

#Obtenha o valor da log-verossimilhança, fazendo correções para considerar a transformação da variável resposta quando for o caso.
#Obtenha os valores preditos na escala da variável resposta para a sequência de valores de x em 0,0.05,0.10,.,2.
#Assinale as sentenças verdadeiras.

#Escolha uma ou mais:

#a. No intervalo x???[0.75,1.5], os valores preditos pelo modelo 7 estão mais próximas do modelo 1 que o modelo 6 do modelo 1.
#b. O uso da função de ligação logarítmica no modelo 8 deu log-verossmilhaça inferior ao modelo 1 que usou função de ligação identitidade.
#c. A log-verossimilhança do modelo 3 foi superior a do 1.
#d. A log-verossimilhança do modelo 4 foi superior a do 1.
#e. A log-verossimilhança do modelo 3, corrigida para a escala natural da variável resposta, foi -54.70.
#f. A transformação da resposta no modelo 3 é mais apropriada que a transformação feita no modelo 4.
#g. O modelo 2 com termo quadrático no preditor produziu log-verossimilhança inferior ao modelo 1.
#h. Dentre os modelos que não transformaram a variável resposta, a maior log-verossmilhança é a do modelo 8.
#i. No intervalo x???[0.5,1.5], o valor predito, y^, pelo modelo 1 é superior ao valor predito pelo modelo 2.
#j. A distribuição considerada no modelo 6 é mais apropriada para os dados que a do modelo 7.
#k. Considerando apenas os modelos que não fizeram transformação da variável resposta, os modelos com valores preditos mais próximos um do outro foram 2 e 8.
#l. Na origem (x=0), o modelo 3 tem valor predito inferior ao do modelo 4
