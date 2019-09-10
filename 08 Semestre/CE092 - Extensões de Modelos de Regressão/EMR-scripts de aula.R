# Aula 13/08/2019 - Walmes >> http://leg.ufpr.br/~walmes/mpaer/blocos-incompletos-balanceados.html

library(agricolae) # BIB.test() e design.bib().
library(emmeans)   # emmeans().+++
library(multcomp)  # glht().+++
library(lme4)      # lmer().
library(lmerTest)  # anova() para classe lmerMod.
library(tidyverse) # Manipulação e visualização.

library(igraph)
# Funções.
source("mpaer_functions.R")

# Dados contidos no labestData.
da <- as_tibble(labestData::PimentelPg185)
str(da)


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


#-----------------------------------------------------------------------
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


#--------------------------------------------------------
# Aula 20/08 - PJ
#--------------------------------------------------------


par(mfrow = c(1,1))
plot(Nile)

head(Nile)

Dados airquality$Ozone


MASS::fitdistr(y, family = "Gau") # ver sintaxe]
MASS::fitdis''