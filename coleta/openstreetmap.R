# Dados do OpenStreetMaps

print("Carga de dados do OpenStreetMap...")

library(cartography)
library(osmdata)
library(raster)
library(sf)
library(rgdal)
library(tidygeocoder)
source("config.R", encoding = "UTF-8")

# Limite da cidade
print("Carga de dados do OpenStreetMap... limite da cidade...")
limite_cidade <- opq(paste("Vitória","-",UF)) |>
  add_osm_features(c("\"admin_level\"=\"8\"")) |>
  osmdata_sf()

limite_cidade <- limite_cidade$osm_multipolygons |> as("Spatial")
limite_cidade <- limite_cidade[limite_cidade$name |> 
                                 iconv(from = "UTF-8", to = "UTF-8") |> 
                                 toupper() == CIDADE,]
limite_cidade <- spTransform(limite_cidade,  CRS("+ellps=WGS84 +proj=longlat +datum=WGS84 +no_defs"))

saveRDK(limite_cidade,
        "coleta/dados/limite_cidade.RDK")

## consulta as coordenadas dos imóveis para os quais não há informações internas
print("Carga de dados do OpenStreetMap... geocode dos imóveis...")
imoveis_geo_osm <- readRDK("coleta/dados/imoveis.RDK")
imoveis_geo_interno <- readRDK("coleta/dados/imoveis_geo_interno.RDK")

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
imoveis_geo_osm |> saveRDK("coleta/dados/imoveis_geo_osm.RDK")

print("Fim da carga de dados do OpenStreetMap.")
