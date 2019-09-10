library(tidyverse)

url <- "https://raw.githubusercontent.com/pet-estatistica/labestData/devel/data-raw/PimentelEx6.6.3.txt"
tb <- read.table(url, sep = "\t", header = TRUE)
tb <- transform(tb,
                leitegada = factor(leitegada),
                coluna = factor(coluna),
                castracao = factor(castracao,
                                   levels = c("7", "21", "56", "Test")))

matrix(tb$castracao, nrow = 4)

var.resp <- tb$peso
trat <- tb$castracao
linhas <- tb$leitegada
colunas <- tb$coluna

t <- length(unique(trat))
n <- t^2

# Variável resposta
Y <- matrix(var.resp, ncol = 1)
Y

# Vetor de médias \mu
mu <- matrix(rep(1, n), ncol = 1)

# Matriz dos tratamentos
mt <- matrix(0, nrow = n, ncol = 4)
# mt[cbind(seq_along(trat), ifelse(trat == "Test", 2, 1))] <- as.character(trat)
mt[cbind(seq_along(trat), trat)] <- as.character(trat)
mt[mt == "Test"] <- 1
mt <- matrix(as.numeric(mt), ncol = 4)
mt

# Matriz de linhas
ml <- matrix(0, nrow = n, ncol = t)
ml[cbind(seq_along(linhas), linhas)] <- 1
ml

# Matriz de colunas
mc <- matrix(0, nrow = n, ncol = t)
mc[cbind(seq_along(colunas), colunas)] <- 1
mc

# Matriz linhas x colunas
mlc <- diag(1, nrow = n)

# ------------------------------------------------
# Matriz da média geral
Mu <- mu %*% solve(t(mu) %*% mu) %*% t(mu)

# Matriz da média geral para linha
Ml <- ml %*% solve(t(ml) %*% ml) %*% t(ml)

# para coluna
Mc <- mc %*% solve(t(mc) %*% mc) %*% t(mc)

# linha x coluna
Mlc <- mlc %*% solve(t(mlc) %*% mlc) %*% t(mlc)

# para tratamento
Mt <- mt %*% solve(t(mt) %*% mt) %*% t(mt)
Mt

# matriz ortogonais
## Para linha
Ql <- Ml - Mu

## Para coluna
Qc <- Mc - Mu

## Para tratamento
Qt <- Mt - Mu

## Para linhas x colunas
Qlc <- Mlc - Ml - Mc + Mu

## Para residuo
Qres <- Mlc -Ml -Mc + Mu -Mt + Mu


# ------------------------------------------------
(GLl <- sum(diag(Ql))) # t - 1
(GLc <- sum(diag(Qc))) # t - 1
(GLlc <- sum(diag(Qlc))) # (t - 1)^2
(GLt <- sum(diag(Qt))) # t - 1
(GLres <- sum(diag(Qres))) # (t - 1) * (t - 2)

# ------------------------------------------------
## Soma de quadrados
(SQl <- t(Y) %*% Ql %*% Y)
(SQc <- t(Y) %*% Qc %*% Y)
(SQlc <- t(Y) %*% Qlc %*% Y)
(SQt <- t(Y) %*% Qt %*% Y)
(SQres <- t(Y) %*% Qres %*% Y)

## Quadrado médio
(QMl <- SQl/GLl)
(QMc <- SQc/GLc)
(QMt <- SQt/GLt)
(QMres <- SQres/GLres)
(Fcalc.l <- QMl/QMres)
(Fcalc.c <- QMc/QMres)
(Fcalc.t <- QMt/QMres)
(p.valor.l <- pf(Fcalc.l, GLl, GLres, lower.tail = FALSE))
(p.valor.c <- pf(Fcalc.c, GLc, GLres, lower.tail = FALSE))
(p.valor.t <- pf(Fcalc.t, GLt, GLres, lower.tail = FALSE))


## Tabela final
tab.final <- data.frame("GL" = c(GLl, GLc, GLlc, GLt, GLres),
                        "SQ" = c(SQl, SQc, SQlc, SQt, SQres),
                        "QM" = c(QMl, QMc, NA, QMt, QMres),
                        "F"  = c(Fcalc.l, Fcalc.c, NA, Fcalc.t, NA),
                        "p-valor" = c(p.valor.l, p.valor.c, NA, p.valor.t, NA),
                        row.names = c("Linhas", "Colunas", "Linhas#Colunas",
                                      "  Tratamentos", "  Resíduo"))
tab.final
