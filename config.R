## Variáveis de configuração da aplicação

# Executar versão de teste
teste_ver <- TRUE

# DSN para acesso à base da prefeitura via ODBC
dsn_name <- "PMVBigData"
dw_encoding <- "UTF-8"
#dw_encoding <- "LATIN1"

# Arquivos presentes em /dados:
arquivo_lotes <- "lotes.kml"
arquivo_imoveis <- "imoveis.kml"
arquivo_unidades <- "unidades.kml"
arquivo_unidades_saude <- "unidades_saude.kml"

# Cidade
CIDADE <- "VITÓRIA"
UF <- "ES"

# Chave para criptografia do banco de senhas
chave <- "m4EK^c370Gjq"
usuario_inicial <- "admin"
senha_inicial <- "admin"

# Funções para leitura e escrita de dados criptografados
readRDK <- function (file_name) {
  chave <- readRDS("dados/chave_db.RDS")
  file_name |>
    readRDS() |>
    aes_cbc_decrypt(key = chave) |>
    unserialize()
}

saveRDK <- function (dados, file_name) {
  chave <- readRDS("dados/chave_db.RDS")
  dados |> 
    serialize(NULL) |> 
    aes_cbc_encrypt(key = chave) |>
    saveRDS(file_name)
}

# Proxy
# proxy_url <- "proxy_server_here"
# Sys.setenv(http_proxy = proxy_url, https_proxy = proxy_url, ftp_proxy = proxy_url)