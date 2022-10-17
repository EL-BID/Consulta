# Cruza informações das pessoas com os lotes
print("Identificando pessoas por lote...")

library(sp)
library(dplyr)
library(tidyr)

source("config.R")

lotes <- readRDS("coleta/dados/lotes.RDS")
imoveis_c_geo <- readRDS("manipulacao/dados/imoveis_c_geo.RDS")
pessoas <- readRDS("manipulacao/dados/pessoas.RDS")

# numerar lotes
lotes@data$Name <- 1:length(lotes@data$Name)

# Cruzar lotes
imoveis_p_lote <- imoveis_c_geo %over% lotes
# Elimina imóveis que não estão nos lotes
imoveis_p_lote <- 
  imoveis_p_lote[imoveis_p_lote$Name |> is.na() |> not(),]

# Elimina lotes que não possuem imóveis georreferenciados
lista_lotes <- imoveis_p_lote$Name |> unique()
lotes <- lotes[lista_lotes,]


# Complementar informações
imoveis_p_lote$pcode <- imoveis_c_geo$pcode
imoveis_p_lote$complemento <- imoveis_c_geo$complemento
imoveis_p_lote$relacao <- imoveis_c_geo$relacao

imoveis_p_lote$nome <- 
  pessoas$nome[match(imoveis_p_lote$pcode,
                     pessoas$pcode)]

# Organiza (e ordena pelo complemento)
imoveis_p_lote <- 
  imoveis_p_lote[order(imoveis_p_lote$complemento),
                 c("Name","pcode","nome","complemento","relacao")]


lotes |> saveRDS("manipulacao/dados/lotes.RDS")
imoveis_p_lote |> saveRDS("manipulacao/dados/imoveis_p_lote.RDS")
