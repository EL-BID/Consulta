# Dados do OpenStreetMaps

print("Carga de dados do OpenStreetMap...")

library(cartography)
library(osmdata)
library(raster)
library(sf)
library(rgdal)
library(tidygeocoder)
source("config.R")

## Edificações ####
# print("Carga de dados do OpenStreetMap... edificações...")
# edificacoes <- opq("Vitória - ES") |>
#   add_osm_features(c("\"building\"=\"yes\"")) |>
#   osmdata_sf()
# 
# edificacoes <- edificacoes$osm_multipolygons |> as("Spatial")
# 
# saveRDS(edificacoes,
#         "coleta/dados/edificacoes.RDS")

# Limite da cidade
print("Carga de dados do OpenStreetMap... limite da cidade...")
limite_cidade <- opq(paste("Vitória","-",UF)) |>
  add_osm_features(c("\"admin_level\"=\"8\"")) |>
  osmdata_sf()

limite_cidade <- limite_cidade$osm_multipolygons |> as("Spatial")
limite_cidade <- limite_cidade[limite_cidade$name|>toupper()==CIDADE,]
limite_cidade <- spTransform(limite_cidade,  CRS("+ellps=WGS84 +proj=longlat +datum=WGS84 +no_defs"))

saveRDS(limite_cidade,
        "coleta/dados/limite_cidade.RDS")

## consulta as coordenadas dos imóveis para os quais não há informações internas
print("Carga de dados do OpenStreetMap... geocode dos imóveis...")
imoveis_geo_osm <- readRDS("coleta/dados/imoveis.RDS")
imoveis_geo_interno <- readRDS("coleta/dados/imoveis_geo_interno.RDS")

imoveis_geo_osm <- 
  imoveis_geo_osm[
    which(!(imoveis_geo_osm$inscricaoCadastral %in% 
              imoveis_geo_interno$inscricaoCadastral)),]

imoveis_geo_osm$endereco <-
  paste0(imoveis_geo_osm$tipoLogradouro, " ",
         imoveis_geo_osm$nomeLogradouro,", ",
         imoveis_geo_osm$numero," ",
         imoveis_geo_osm$nomeBairro,", ",
         CIDADE," - ",
         UF,", ",
         "BRASIL")

imoveis_geo_osm <- imoveis_geo_osm[,c("inscricaoCadastral", "endereco")] |>
  geocode(address = endereco,
          method = 'osm')

# Elimina imóveis sem geo
imoveis_geo_osm$Name <- NA
imoveis_geo_osm$Description <- NA
imoveis_geo_osm <- 
  imoveis_geo_osm[imoveis_geo_osm$long |> is.na() |> not(),
                  c("Name", "Description", "inscricaoCadastral", "long", "lat")]

# Converte para SpatialPointsDataFrame
coordinates(imoveis_geo_osm) <- c("long","lat")
proj4string(imoveis_geo_osm) <- "+proj=longlat +datum=WGS84 +no_defs"

# Grava
saveRDS(imoveis_geo_osm,
        file = "coleta/dados/imoveis_geo_osm.RDS")

print("Fim da carga de dados do OpenStreetMap.")
