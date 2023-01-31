# trata as informações coletadas

dir.create("manipulacao/dados", showWarnings = FALSE)
dir.create("visualizacao/dados", showWarnings = FALSE)

source("config.R", encoding = "UTF-8")

# Informações de atualização
status <- NULL
status$atualizacao <- Sys.Date()
status$teste <- teste_ver
status |> saveRDS("manipulacao/dados/status.RDS")

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
