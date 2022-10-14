## Autenticação para a visualização ####

library(shinymanager)
source("config.R", encoding = "UTF-8")

# Define credenciais iniciais (MODIFICAR NA PRIMEIRA UTILIZAÇÃO!)
credentials <- data.frame(
  user = c(usuario_inicial),
  password = c(senha_inicial),
  admin = c(TRUE),
  stringsAsFactors = FALSE
)

# Criação inicial do banco de senhas
create_db(
  credentials_data = credentials,
  sqlite_path = "visualizacao/dados/database.sqlite",
  passphrase = key_get("R-shinymanager-key", "consulta")
)