library(ggplot2)
library(gridExtra)
library(corrplot)

dados <- read_xls('Base_final.xls',
                  col_types = c("text", "numeric", "numeric","numeric", "numeric",
                                "numeric","numeric", "numeric", "numeric","numeric",
                                "numeric"))

head(dados)
str(dados)
str(dados[ , 12:15])
summary(dados)
summary(dados[ , 12:15])

# Boxplots

par(mfrow=c(2,2))
boxplot(dados$Obitos, xlab = '', ylab = '', main = 'Óbitos no Trânsito', las=1)
boxplot(dados$Frota, xlab = '', ylab = '', main = 'Frota de Veículos', las=1)
boxplot(dados$Pop, xlab = '', ylab = '', main = 'População', las=1)
boxplot(dados$DensDem, xlab = '', ylab = '', main = 'Densidade Demográfica', las=1)

par(mfrow=c(2,3))
boxplot(dados$Alfab, xlab = '', ylab = '', main = '% Alfabetizados', las=1)
boxplot(dados$PopUrb, xlab = '', ylab = '', main = '% Pop Urbana', las=1)
boxplot(dados$Desemp, xlab = '', ylab = '', main = '% Desmpregados', las=1)
boxplot(dados$BaxRen, xlab = '', ylab = '', main = '% Pop Baixa Renda', las=1)
boxplot(dados$Renda, xlab = '', ylab = '', main = 'Renda Média', las=1)
boxplot(dados$IDH, xlab = '', ylab = '', main = 'IDH', las=1)


# Histogramas
g1  <- ggplot(dados, aes(x=Obitos)) + geom_histogram()+ xlab('Óbitos no Trânsito')+ ylab('')
g2  <- ggplot(dados, aes(x=Frota)) + geom_histogram()+ xlab('Frota de Veículos')+ ylab('')
g3  <- ggplot(dados, aes(x=Pop)) + geom_histogram()+ xlab('População')+ ylab('')
g4  <- ggplot(dados, aes(x=DensDem)) + geom_histogram()+ xlab('Densidade Demográfica')+ ylab('')

grid.arrange(g1, g2, g3, g4, ncol=2, nrow=2)

g5  <- ggplot(dados, aes(x=Alfab)) + geom_histogram()+ xlab('% Alfabetizados')+ ylab('')
g6  <- ggplot(dados, aes(x=PopUrb)) + geom_histogram()+ xlab('% Pop Urbana')+ ylab('')
g7  <- ggplot(dados, aes(x=Desemp)) + geom_histogram()+ xlab('% Desmpregados')+ ylab('')
g8  <- ggplot(dados, aes(x=BaxRen)) + geom_histogram()+ xlab('% Pop Baixa Renda')+ ylab('')
g9  <- ggplot(dados, aes(x=Renda)) + geom_histogram()+ xlab('Renda Média')+ ylab('')
g10 <- ggplot(dados, aes(x=IDH)) + geom_histogram()+ xlab('IDH')+ ylab('')

grid.arrange(g5, g6, g7, g8, g9, g10, ncol=2, nrow=3)


# Pelos boxplots e histogramas verificamos assimetria nas variaveis:
# DensDem, Obitos, Frota e Pop


# Testando o log das variáveis para corrigir assimetria

dados$lObitos  <- log(dados$Obitos)
dados$lDensDem <- log(dados$DensDem)
dados$lFrota   <- log(dados$Frota)
dados$lPop     <- log(dados$Pop)

str(dados[ , 12:15])
summary(dados[ , 12:15])


x11()
par(mfrow = c(2,2))
hist(dados$lObitos, main = 'log(óbitos)', xlab = '', ylab = '')
hist(dados$lFrota, main = 'log(Frota)', xlab = '', ylab = '')
hist(dados$lPop, main = 'log(Popução)', xlab = '', ylab = '')
hist(dados$lDensDem, main = 'log(Densidade)', xlab = '', ylab = '')

x11()
par(mfrow = c(2,2))
boxplot(dados$lObitos, xlab = '', ylab = '', main = 'log(óbitos)')
boxplot(dados$lFrota, xlab = '', ylab = '', main = 'log(Frota)')
boxplot(dados$lPop, xlab = '', ylab = '', main = 'log(Popução)')
boxplot(dados$lDensDem, xlab = '', ylab = '', main = 'log(Densidade)')


ajuste01<-glm(Obitos~.-Munic,family = poisson(link = 'log'),data = dados)
summary(ajuste01)
