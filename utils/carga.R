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
file.copy("manipulacao/dados", "visualizacao", recursive = TRUE)
file.copy("coleta/dados/lotes.RDS", "visualizacao/dados", recursive = TRUE)
file.create("visualizacao/restart.txt")
print("Aplicação atualizada...")