## Autenticação para a visualização ####

# library(keyring)
# source("config.R", encoding = "UTF-8")
# 
# # Cria chave para o banco de dados
# keyring_unlock(password= SENHA_DO_ROOT)
# key_set_with_value(service = "R-shinymanager-key", 
#          username = "consulta", 
#          password = chave,
#          keyring = NULL)

chave |> saveRDS("visualizacao/dados/chave.RDS")
