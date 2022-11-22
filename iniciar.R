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

## Autenticação para a visualização ####
dir.create("visualizacao/dados", showWarnings = FALSE)
source("utils/cria_chave_db.R")
source("utils/inicia_db.R")

## carga e manipulação dos dados ####
source("utils/carga.R")