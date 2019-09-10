#
# Leitura dos dados
#
dados=read.table('http://people.ufpr.br/~lucambio/CE075/1S2011/exemplo05.txt',h=T)
#
attach(dados)
names(dados)
#
# Estudo descritivo
#
require(lattice)
#
X11()
xyplot(Pressao ~ factor(Teor,levels=c('Baixo','Alto')),groups=Paciente, ,ylab='Pressão arterial',xlab='Teor de sódio',
       main='Efeito da ingestão de sódio na pressão arterial \n de pacientes hipertensos',type='l')
#
# Observamos aumento medio da pressao arterial na maioria dos pacientes quando 
# aumentado o teor de sodio.
#
# Estimando a matriz de correlacoes
#
TBaixo=dados[Teor=='Baixo',3]
TAlto=dados[Teor=='Alto',3]
#
Corr=cor(cbind(TBaixo,TAlto))
#
Corr=matrix(Corr, nrow=2,ncol=2,dimnames=list(
  c('Teor Baixo','Teor Alto'),c('Teor Baixo','Teor Alto')))
#
Corr
#
# Atualizado em: Segunda-feira 22/04/2013 (Fernando Lucambio, lucambio@ufpr.br)