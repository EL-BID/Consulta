# Exemplo de leitura do DW via arquivo xlsx

print("Carga de dados do DataWarehouse...")

library(xlsx)
source("config.R")

campos <- read.csv2("dados/dicionario.csv", row.names = 1) |>
  as.data.frame()

# leitura dos dados
for (base in names(campos)) {
  assign(base, read.xlsx(paste0("dados/amostra/",campos["tabela", base],".xlsx"), 1))
}

# grava dados
for (base in names(campos)) {
  get(base) |> saveRDK(paste0("coleta/dados/",base,".RDK"))
}
print("Fim da carga de dados do DataWarehouse.")
