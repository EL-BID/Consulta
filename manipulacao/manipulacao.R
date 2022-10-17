# trata as informações coletadas

dir.create("manipulacao/dados", showWarnings = FALSE)
dir.create("visualizacao/dados", showWarnings = FALSE)

# Cruza as pessoas
source("manipulacao/pessoas.R")
rm(list=ls())
gc()

# Locais de atendimento
source("manipulacao/relacoes_geo.R")
rm(list=ls())
gc()

# Lotes
source("manipulacao/lotes.R")
rm(list=ls())
gc()
