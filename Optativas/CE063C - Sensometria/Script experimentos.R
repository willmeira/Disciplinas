## Experimento 01 - Teste Triangular 

require("SensoMineR")
require("FactoMineR")

library("SensoMineR")
library("FactoMineR")

triangle.design(2,24)
triangle.pair.test(16,24)



## Experimento 02 - Teste Tetraedrico

require("SensoMineR")
require("FactoMineR")

library("SensoMineR")
library("FactoMineR")

triangle.design(2,24)
triangle.pair.test(16,24)

install.packages("sesnR")


delineamento <- triangle.design(2,24)
resultado <- triangle.pair.test(16,24)

