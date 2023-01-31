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

chave |> charToRaw() |> sha256() |> saveRDS("dados/chave_db.RDS")
chave |> charToRaw() |> sha256() |> saveRDS("visualizacao/dados/chave_db.RDS")
