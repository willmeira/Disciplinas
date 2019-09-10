#
# Leitura dos dados
#
dados=read.table('http://people.ufpr.br/~lucambio/CE075/1S2010/exemplo01.txt',h=T)
#
attach(dados)
names(dados)
#
# Estudo descritivo
#
plot(Erro~Bloco,type='n',pch=19,main='Erro de resposta',ylab='Media dos valores absolutos',xlab='Tentativa')
#
for(i in 1:9){ 
  lines(Erro[Voluntario==i]~Bloco[Voluntario==i],col=i)
  points(Erro[Voluntario==i]~Bloco[Voluntario==i],col=i,cex=0.5) }
#