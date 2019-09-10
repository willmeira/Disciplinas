library(labestData)

tb <- PimentelEx6.6.3
tb <- transform(tb,
                leitegada = factor(leitegada),
                coluna = factor(coluna),
                castracao = factor(castracao,
                                   levels = c("7", "21", "56", "Test")))

str(tb)
summary(tb)
#-------------------------------------------------------------------------------------------------------
library(ExpDes.pt)

dados <- dql(tb$castracao, tb$leitegada, tb$coluna, tb$peso, quali = TRUE, mcomp = "tukey", sigT = 0.05, sigF = 0.05)
#-------------------------------------------------------------------------------------------------------


matrix(tb$castracao, nrow = 4)
tb$castracao <- as.character(tb$castracao)
tb$castracao[which(tb$castracao == "Test")] <- 0
tb$castracao <- as.numeric(tb$castracao)

tb$test <- rep(0,16)
tb$test[which(tb$castracao == 0)] <- 1

tb$test <- as.factor(tb$test)


var.resp <- tb$peso
trat <- tb$castracao
linhas <- tb$leitegada
colunas <- tb$coluna

t <- length(unique(trat))
n <- t^2


m0 <- lm(peso ~ ., data = tb)
summary(m0)

m0$coefficients