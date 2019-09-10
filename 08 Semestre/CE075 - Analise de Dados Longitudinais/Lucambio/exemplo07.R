#
# Leitura dos dados
#
dados=read.table('http://people.ufpr.br/~lucambio/CE075/1S2011/exemplo07.txt',h=T)
#
attach(dados)
names(dados)
#
# Estudo descritivo
#
require(lattice)
#
X11()
xyplot(velocidade ~ factor(minutos)|factor(Concentracao),
       groups=Palato, ,ylab='Velocidade de transporte mucociliar relativa (mm/s)',
       xlab=expression(paste('Concentração em ',mu,'M de peróxido de hidrogênio')),
       main='Efeito da exposição a maiores concentrações de agentes oxidantes \n 
        na principal defesa do sistema respiratório contra agentes externos',type='l')
#
# No grupo controle permanece constante a velocidade de transporte mucociliar relativa. 
# Nas outras situacoes, percebemos comportamentos erraticos em baixas concentracoes.
# Em concentracoes elevadas a velocidade de transporte mucociliar diminui fortemente
# conforme aumenta o tempo de exposicao. Na maior concentracao a resposta e quase 
# zero em algumas situacoes.
#
# Estimando a matriz de correlacoes
#
M.05=dados[minutos==5,4]
M.10=dados[minutos==10,4]
M.15=dados[minutos==15,4]
M.20=dados[minutos==20,4]
M.25=dados[minutos==25,4]
M.30=dados[minutos==30,4]
M.35=dados[minutos==35,4]
#
# Existem dados faltantes, logo deve ser colocada a opcao use='complete.obs' para 
# calcular a correlacao somente com as observacoes completas.
#
Corr=cor(cbind(M.05,M.10,M.15,M.20,M.25,M.30,M.35),use='complete.obs')
#
Corr
#
# Grafico de dispersao segundo Mostrando a correlacao 
#
require(car)
#
scatterplotMatrix(Corr,main='Matriz de correlações',smooth=F,reg.line=F,
  var.labels=c('5 minutos','10 minutos','15 minutos','20 minutos','25 minutos',
               '30 minutos','35 minutos'))
#
# Percebemos que existe uma forte correlacao na resposta entre os tempos de exposicao
# e que diminui conforme transcorre o tempo.
#
# Atualizado em: Terca-feira 29/04/2013 (Fernando Lucambio, lucambio@ufpr.br)
