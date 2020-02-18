#-----------------------------------------------------------------------
# Extensões de modelos de regressão · CE 092
# web.leg.ufpr.br/ensino/EMR
#
#                                            Prof. Dr. Walmes M. Zeviani
#                                       Prof. Dr. Paulo Justiniano R. Jr
#                      Laboratory of Statistics and Geoinformation (LEG)
#                Department of Statistics · Federal University of Paraná
#                                       2019-Nov-12 · Curitiba/PR/Brazil
#-----------------------------------------------------------------------

#-----------------------------------------------------------------------
# Pacotes.
rm(list = ls())

dados<-iris

library(tidyverse)
library(lme4)     # Funções para modelos de efeitos aleatórios.
library(lmerTest) # Complementar a lme4.
library(emmeans)  # Estimated marginal means.

#=======================================================================
# Caso 1. Experimento em blocos incompletos.

# Dados contidos no labestData.
# da <- as_tibble(labestData::PimentelPg185)

url <- "https://raw.githubusercontent.com/pet-estatistica/labestData/devel/data-raw/PimentelPg185.txt"
da <- read_tsv(url)
da <- da %>%
    mutate_at(c("bloc", "trat"), "factor")
attr(da, "spec") <- NULL

str(da)
summary(da)
# São 6 repetições de cada tratamento.
# O tamanho do bloco é 3.
addmargins(xtabs(~trat + bloc, data = da))

# Quantas vezes cada par ocorre junto.
by(data = da$trat,
   INDICES = da$bloc,
   FUN = function(x) {
       apply(combn(sort(x), m = 2),
             MARGIN = 2,
             FUN = paste0,
             collapse = "_")
   }) %>%
    flatten_chr() %>%
    table()

# Obtém as coordenadas radiais para gráfico de conexão.
da <- da %>%
    do({
        k <- nlevels(.$trat)
        a <- seq(0, 2 * pi, length.out = k + 1)[-(k + 1)]
        cbind(.,
              data.frame(coord_x = sin(a)[as.integer(.$trat)],
                         coord_y = cos(a)[as.integer(.$trat)]))
    })

# Gráfico de conexão.
ext <- c(-1.2, 1.2)
ggplot(data = da,
       mapping = aes(coord_x, coord_y)) +
    facet_wrap(facets = ~bloc, nrow = 2) +
    geom_point() +
    geom_polygon(fill = NA, color = "orange") +
    geom_label(mapping = aes(label = trat)) +
    expand_limits(x = ext, y = ext) +
    coord_equal()

# Visualização dos dados.
ggplot(data = da,
       mapping = aes(x = trat, y = y, color = bloc)) +
    geom_point() +
    stat_summary(mapping = aes(group = 1),
                 geom = "line",
                 fun.y = "mean") +
    geom_text(mapping = aes(label = bloc),
              hjust = 0,
              nudge_x = 0.02,
              show.legend = FALSE)

library(igraph)

 edg <- by(data = da$trat,
           INDICES = da$bloc,
           FUN = combn,
           m = 2) %>%
     flatten_int()

 ghp <- graph(edg, directed = FALSE)
 plot(ghp,
      layout = layout_in_circle,
      edge.curved = FALSE)

#-----------------------------------------------------------------------
# Ajuste dos modelos.

# Ajuste do modelo de efeitos fixos para blocos.
 
 m0 <- lm(y ~ bloc + trat,
         data = da,
         contrasts = list(bloc = contr.sum))

# Quadro de teste de F.
anova(m0)

# Estimativas dos parâmetros.
summary(m0)

# Médias ajustadas marginais.
emm_m0 <- emmeans(m0, specs = ~trat)
emm_m0

# Constrastes par a par, ou seja, contrastes de Tukey.
contrast(emm_m0, method = "tukey")

# Ajuste do modelo de efeito aleatório de bloc.
m1 <- lmer(Sepal.Length ~ (1|Species)+Sepal.Width + Petal.Length + Petal.Width,
            data = dados)
summary(m1)
mm0 <- lmer(y ~ (1 | bloc) + trat, data = da)

str(dados)
summary(dados)

# Quadro de teste de Wald.
anova(mm0)

# Estimativas dos parâmetros.
summary(mm0, correlation = FALSE)
summary(mm0)
# Extração das estimativas dos parâmetros de efeito fixo.
fixef(mm0)

# Extração das predições dos coeficientes de efeito aleatório.
ranef(mm0)

# Componentes de variância.
VarCorr(mm0)

# Valores preditos individuais: X\beta + Zb.
#   formula(mm0, fixed.only = TRUE)
#   formula(mm0, random.only = TRUE)
model.matrix(~trat, data = da) %*% fixef(mm0) +
    model.matrix(~0 + bloc, data = da) %*% ranef(mm0)$bloc$`(Intercept)`
predict(mm0)



# Valores preditos populacionais: X\beta.
unique(model.matrix(~trat, data = da)) %*% fixef(mm0)
unique(predict(mm0, re.form = ~0))

# Veja as matrizes X e Z extraídas do modelo ajustado.
model.matrix(mm0, type = "fixed")  # X.
model.matrix(mm0, type = "random") # Z.

# Médias marginais ajustadas.
emm_mm0 <- emmeans(mm0, specs = ~trat)
emm_mm0

# Constrastes par a par, ou seja, contrastes de Tukey.
contrast(emm_mm0, method = "pairwise")

# O efeito de encolhimento (shirinkage).
ef_mm0 <- c(ranef(mm0)$bloc$`(Intercept)`)
ef_m0 <- coef(m0)[m0$assign == 1]
ef_m0 <- c(ef_m0, bloc10 = -sum(ef_m0))

# Estimativas ao lado das predições para o efeito de blocos.
cbind(ef_m0, ef_mm0)

ef <- tibble(bloc = names(ef_m0),
             ef_m0 = as.vector(ef_m0),
             ef_mm0 = as.vector(ef_mm0)) %>%
    gather(key = "ef", value = "val", -bloc)

ggplot(data = ef,
       mapping = aes(x = ef, y = val, group = bloc)) +
    geom_point() +
    geom_line() +
    geom_hline(yintercept = 0, lty = 2, size = 0.25) +
    labs(x = "Modelo usado para estimar/predizer o efeito",
         y = "Valor estimado/predito para o efeito")

#=======================================================================
# Caso 2. Experimento com muitos avaliadores.

# 42 provadores (aleatório) e efeito de concentração (fixo).
url <- "http://leg.ufpr.br/~walmes/data/frango_sensorial.txt"
aval <- read_tsv(url, comment = "#")
attr(aval, "spec") <- NULL
aval <- aval %>%
    mutate(conc = factor(conc),
           aval = factor(aval))
str(aval)

aval_l <- aval %>%
    gather(key = "resp", value = "value", -(1:3))

# Criando uma nota média.
aval_g <- aval_l %>%
    group_by(aval, conc) %>%
    summarise(value = mean(value)) %>%
    ungroup()
aval_g

# Acessa o efeito marginal da concentação.
ggplot(data = aval_g,
       mapping = aes(x = conc, y = value, group = 1)) +
    geom_jitter(width = 0.05, pch = 1) +
    stat_summary(geom = "line", fun.y = "mean")

aval_m  <- aval_g %>%
    group_by(conc) %>%
    summarise_at("value", "mean")

# Acessa o efeito dos avaliadores.
ggplot(data = aval_g,
       mapping = aes(x = conc, y = value, group = aval)) +
    facet_wrap(facets = ~aval) +
    geom_point() +
    geom_line() +
    geom_line(data = aval_m,
              inherit.aes = FALSE,
              mapping = aes(x = conc, y = value, group = 1),
              size = 0.5,
              color = "orange")

# NOTE: não é curioso que absolutamente nenhum dos provadores tenha dado
# a mesma nota para as avaliações? Não estariam os avaliadores
# inclinados a dar nota diferente por saberem que existem tratamentos
# diferentes sem de fato existir diferença entre os tratamentos?
# Concorda que uma balança, fita métrica ou cronômetro é imparcial
# diferente do avaliador humano não treinado?

# Ajuste do modelo de efeitos fixos para avaliador.
m0 <- lm(value ~ aval + conc,
         data = aval_g,
         contrasts = list(aval = contr.sum))

# Ajuste do modelo de efeito aleatório de bloc.
mm0 <- lmer(value ~ (1 | aval) + conc, data = aval_g)

# O efeito de encolhimento (shirinkage).
ef_mm0 <- c(ranef(mm0)$aval$`(Intercept)`)
ef_m0 <- coef(m0)[m0$assign == 1]
ef_m0 <- c(ef_m0, aval42 = -sum(ef_m0))

# Estimativas ao lado das predições para o efeito dos avaliadores.
cbind(ef_m0, ef_mm0)[order(ef_m0), ]

# ATTENTION: devido à resposta ser discreta e limitada em alguns níveis,
# vários avaliadores tiveram a mesma estimativa/predição devido ao
# padrão de avaliação igual.

ef <- tibble(bloc = names(ef_m0),
             ef_m0 = as.vector(ef_m0),
             ef_mm0 = as.vector(ef_mm0)) %>%
    gather(key = "ef", value = "val", -bloc)

ggplot(data = ef,
       mapping = aes(x = ef, y = val, group = bloc)) +
    geom_point() +
    geom_line() +
    geom_hline(yintercept = 0, lty = 2, size = 0.25) +
    labs(x = "Modelo usado para estimar/predizer o efeito",
         y = "Valor estimado/predito para o efeito")

summary(mm0)

# Contraste entre níveis de concentração.
contrast(emmeans(mm0, specs = ~conc),
         method = "pairwise")

#=======================================================================
# Caso 3. Dados longitudinais.

url <- "http://leg.ufpr.br/~walmes/data/gado_crescimento.txt"
gado <- read_tsv(url, comment = "#")
attr(gado, "spec") <- NULL
str(gado)

# Seleciona apenas uma estação.
gado <- gado %>%
    filter(estacao %in% c("seca1")) %>%
    mutate_at(c("vaca", "helmintico", "estacao"), "factor")

# Renomeia níveis para ficar enxuto os outputs.
gado <- gado %>%
    mutate(helmintico = factor(helmintico,
                               labels = sprintf("T%d",
                                                1:nlevels(helmintico))),
           vaca = factor(vaca,
                         labels = sprintf("V%d",
                                                1:nlevels(vaca))))

gado <- gado %>%
    arrange(vaca, mes)

# Tem estrutura de tratamentos e avaliações.
xtabs(~helmintico + mes, data = gado)

# Gráfico dos perfils com animais agrupados por tratamento.
ggplot(data = gado,
       mapping = aes(x = mes,
                     y = peso,
                     color = vaca,
                     group = vaca)) +
    facet_wrap(facets = ~helmintico) +
    geom_point(show.legend = FALSE) +
    geom_line(show.legend = FALSE)

# Sem acomodar o efeito de vaca.
m0 <- lm(peso ~ helmintico * mes, data = gado)
anova(m0)

# Acomodar o efeito de vaca com efeitos fixos.
m1 <- lm(peso ~ (vaca + helmintico) * mes,
         data = gado,
         contrast = list(vaca = contr.sum))
anova(m1)

# ATTENTION: algumas vacas tem apenas uma observação do valor de peso, o
# que impede estimar a curva de crescimento, com declaração de
# interação, em um modelo de efeitos fixos.

anova(m0, m1)

# Acomodar o efeito de vaca com efeitos aleatórios.
mm0 <- lmer(peso ~ (1 + mes | vaca) + helmintico * mes,
            data = gado)
summary(mm0)

# Cria malha para a predição.
pred <- gado %>%
    distinct(vaca, helmintico) %>%
    mutate(mes = list(seq(5, 9, by = 1))) %>%
    unnest()
pred

# Predições individuais e populacionais pelos modelos.
pred$m0_pop <- predict(m0, newdata = pred)
pred$m1_ind <- predict(m1, newdata = pred)
pred$mm0_ind <- predict(mm0, newdata = pred)
pred$mm0_pop <- predict(mm0, newdata = pred, re.form = ~0)

# Efeito de vaca acomodado via efeitos fixos.
ggplot(data = pred,
       mapping = aes(x = mes,
                     y = m1_ind,
                     color = helmintico,
                     group = vaca)) +
    facet_wrap(facets = ~vaca) +
    geom_line(show.legend = FALSE) +
    geom_line(data = pred,
              mapping = aes(x = mes,
                            y = m0_pop,
                            color = helmintico,
                            group = vaca),
              linetype = 2,
              show.legend = FALSE)

# Efeito de vaca acomodado via efeitos aleatórios.
ggplot(data = pred,
       mapping = aes(x = mes,
                     y = mm0_ind,
                     color = helmintico,
                     group = vaca)) +
    facet_wrap(facets = ~vaca) +
    geom_line(show.legend = FALSE) +
    geom_line(data = pred,
              mapping = aes(x = mes,
                            y = mm0_pop,
                            color = helmintico,
                            group = vaca),
              linetype = 2,
              show.legend = FALSE)

# Sobreposição das predições individuais do modelo de efeitos fixos e
# aleatórios.
ggplot(data = pred,
       mapping = aes(x = mes,
                     y = mm0_ind,
                     group = vaca)) +
    facet_wrap(facets = ~vaca) +
    geom_line(color = "orange") +
    geom_line(data = pred,
              mapping = aes(x = mes,
                            y = m1_ind,
                            group = vaca),
              color = "purple",
              linetype = 2) +
    geom_point(data = gado,
               mapping = aes(x = mes,
                             y = peso,
                             group = vaca))

# Comparando as estimativas/predições para os efeitos aleatórios.
ef_m1 <- tibble(b0 = coef(m1)[m1$assign == 1],
                b1 = coef(m1)[m1$assign == 4])

ef_m1 <- rbind(ef_m1,
               -colSums(ef_m1))

ef_mm0 <- as_tibble(ranef(mm0)$vaca)
names(ef_mm0) <- names(ef_m1)

ef_m1$vaca <- levels(gado$vaca)
ef_m1$ef <- "fixo"

ef_mm0$vaca <- rownames(ranef(mm0)$vaca)
ef_mm0$ef <- "aleatório"

ef <- bind_rows(ef_m1, ef_mm0) %>%
    mutate_at(c("ef", "vaca"), "factor") %>%
    mutate(ef = fct_rev(ef)) %>%
    gather(key = "param", value = "value", b0, b1)

# IMPORTANT!
ef %>%
    inner_join(distinct(gado, vaca, helmintico)) %>%
    group_by(helmintico, param, ef) %>%
    summarise(m = mean(value)) %>%
    spread(key = "ef", value = "m")

# No modelo de efeitos aleatórios, o somatório do efeito de vaca (seja
# em b0 ou b1) para um mesmo nível de helmintico é 0. No entanto, no
# modelo de efeitos fixos isso não aconteceu. Por que? Porque pelo
# espaço das colunas do efeito de vaca conterem o espaço coluna o efeito
# de helmintico, helmintico "desapareceu" do modelo porque não são
# estimáveis juntamente com o efeito de vaca sem que sejam feitas
# restrições paramétricas adicionais. Então o gráfico abaixo para
# comparar o encolhimento NÃO está correto!

ggplot(data = ef,
       mapping = aes(x = ef, y = value, group = vaca)) +
    facet_wrap(facets = ~param, scale = "free_y") +
    geom_point() +
    geom_line() +
    geom_hline(yintercept = 0, lty = 2, size = 0.25) +
    labs(x = "Modelo usado para estimar/predizer o efeito",
         y = "Valor estimado/predito para o efeito")

# Para obter uma visualização correta, será usada diferença nos valores
# preditos. Observe.

# Cria malha para a predição.
pred <- gado %>%
    distinct(vaca, helmintico) %>%
    mutate(mes = list(0:1)) %>%
    unnest()

# Predições individuais e populacionais pelos modelos.
pred$m1_ind <- predict(m1, newdata = pred)
pred$mm0_ind <- predict(mm0, newdata = pred)

u <- pred %>%
    gather(key = "ef", value = "value", -(1:3)) %>%
    spread(key = "mes", value = "value") %>%
    mutate(b1 = `1` - `0`) %>%
    rename("b0" = `0`) %>%
    select(-`1`) %>%
    gather(key = "param", value = "value", b0, b1)

# Só que agora não está centrado em 0.
ggplot(data = u,
       mapping = aes(x = ef, y = value, group = vaca)) +
    facet_wrap(facets = ~param, scale = "free_y") +
    geom_point() +
    geom_line() +
    geom_hline(yintercept = 0, lty = 2, size = 0.25) +
    labs(x = "Modelo usado para estimar/predizer o efeito",
         y = "Valor estimado/predito para o efeito")

# Como obter as curvas populacionais estimadas para cada tratamento em
# cada modelo? No de efeitos aleatórios é simples pois a estimativa já
# sai pronta. Mas e no de efeitos fixos?

# Qual a razão para os NA no modelo de efeitos fixos?
cbind(m1 = coef(m1)[!(m1$assign %in% c(1, 4))],
      mm0 = fixef(mm0))

# Valor da função em f(mes = 0) := b0.
emm <- emmeans(m1, specs = ~helmintico, at = list(mes = 0))
emm

params <- as.data.frame(emm) %>%
    select(1:2) %>%
    rename("b0" = "emmean")

# Valor da função em f(mes = 1) := b0 + b1 * 1.
emm <- emmeans(m1, specs = ~helmintico, at = list(mes = 1))
emm

params <- emm %>%
    as.data.frame() %>%
    select(1:2) %>%
    rename("b0 + b1" = "emmean") %>%
    inner_join(params)

# Estimativas dos parâmetros das curvas populacionais com o modelo de
# efeitos fixos.
params <- params %>%
    mutate(b1 = `b0 + b1` - `b0`)
params

# Estimativas dos parâmetros das curvas populacionais com o modelo de
# efeitos aleatórios.
K <- cbind(1, attr(C(gado$helmintico, contr.treatment), "contrasts"))
cbind(b0 = as.vector(cbind(K, K * 0) %*% fixef(mm0)),
      b1 = as.vector(cbind(K * 0, K) %*% fixef(mm0)))

# Os resultados são muuuuito próximos devido ao balanceamento. Deve
# haver alguma diferença quando os indivíduos tiverem número não igual
# de observações. Tente incluir missings na resposta para gerar
# desbalanceamento e refaça todas as análises.

#=======================================================================
# Caso 4. Hierarquia mais profunda.

url <- "http://leg.ufpr.br/~walmes/data/farms.txt"
farm <- read_tsv(url, comment = "#")
attr(farm, "spec") <- NULL
str(farm)

farm <- farm %>%
    mutate_at(c("farm", "block", "trt"), "factor")

mm0 <- lmer(resp ~ (1 | farm/block) + trt, data = farm)
summary(mm0)

em_mm0 <- emmeans(mm0, specs = ~trt)
em_mm0
contrast(em_mm0, method = "pairwise")

# ATTENTION: esse modelo ignora a estrutura hierárquica de casualização.
m0 <- lm(resp ~ farm/block + trt, data = farm)
summary(m0)

em_m0 <- emmeans(m0, specs = ~trt)
em_m0
contrast(em_m0, method = "pairwise")

# Esteja atento aos erros padrões das médias e dos contrastes entre
# médias.

# Correlação entre estimativas das médias marginais dos tratamentos.
K <- attr(em_m0, "linfct")
vcov_m0 <- K %*% vcov(m0) %*% t(K)

K <- attr(em_mm0, "linfct")
vcov_mm0 <- K %*% as.matrix(vcov(mm0)) %*% t(K)

round(vcov_m0, 5)  # Menor variância e covariância 0.
round(vcov_mm0, 5) # Maior variância, mas maior covariância.

# Levando para escala de correlação.
cov2cor(vcov_m0)  # Efeito fixo.
cov2cor(vcov_mm0) # Efeito aleatório.

# Aplicando a regra, o erro padrão do contraste é:
sqrt(vcov_m0[1, 1] + vcov_m0[2, 2] - 2 * vcov_m0[1, 2])
sqrt(vcov_mm0[1, 1] + vcov_mm0[2, 2] - 2 * vcov_mm0[1, 2])

#-----------------------------------------------------------------------
