## Autenticação para a visualização ####

library(keyring)
source("config.R", encoding = "UTF-8")

# Cria chave para o banco de dados
key_set_with_value(service = "R-shinymanager-key", "consulta", chave)
