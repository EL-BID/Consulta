############# Consulta ##################
##
## Autor: Rodrigo Franklin
##
## BR-T1496
##
####

## Variáveis de configurações ####
source("config.R", encoding = "UTF-8")

## instalando e carregando pacotes requeridos ####
source("utils/pacotes.R")

## Fatias ####
# Coleta
source("coleta/coleta.R")

# Manipulação
source("manipulacao/manipulacao.R")

# Visualização: grava dados e reinicia a aplicação
status <- NULL
status$atualizacao <- Sys.Date()
status$teste <- teste_ver
status |> saveRDS("visualizacao/dados/status.RDS")
file.copy("manipulacao/dados", "visualizacao", recursive = TRUE)
file.create("visualizacao/restart.txt")
print("Aplicação atualizada...")