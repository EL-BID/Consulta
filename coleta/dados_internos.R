### Carrega informações fornecidas internamente

print("Carga de dados do internos...")

library(rgdal)
source("config.R")

# ## lotes ####
# print("Carga de dados do internos... lotes...")
# lotes <- readOGR(paste0("dados/",arquivo_lotes))
# saveRDS(lotes,
#         file = "coleta/dados/lotes.RDS")

## unidades ####
print("Carga de dados do internos... unidades de atendimento...")
unidades <- readOGR(paste0("dados/",arquivo_unidades))

# extrai dados presente na descrição
unidades$bairro <- 
  sub('</td>.*', "", 
      sub(".*bairro</td> <td>", "", unidades$Description))

saveRDS(unidades,
        file = "coleta/dados/unidades.RDS")

# ## unidades de saúde ####
# print("Carga de dados do internos... unidades de saúde...")
# unidades_saude <- readOGR(paste0("dados/",arquivo_unidades_saude))
# saveRDS(unidades_saude,
#         file = "coleta/dados/unidades_saude.RDS")

## georreferenciamento das unidades imobiliárias ####
print("Carga de dados do internos... unidades imobiliárias...")
imoveis_geo_interno <- readOGR(paste0("dados/",arquivo_imoveis))

# remove duplicados
imoveis_geo_interno <- 
  imoveis_geo_interno[!duplicated(imoveis_geo_interno$Name),]

# extrai dados presente na descrição
imoveis_geo_interno$inscricaoCadastral <- 
  sub('</td>.*', "", 
      sub(".*inscricaoC</td> <td>", "", imoveis_geo_interno$Description))

saveRDS(imoveis_geo_interno, file = 
          "coleta/dados/imoveis_geo_interno.RDS")

print("Fim da carga de dados do internos.")
