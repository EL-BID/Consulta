### Carrega informações fornecidas internamente

print("Carga de dados do internos...")

library(rgdal)
source("config.R")

## lotes ####
print("Carga de dados do internos... lotes...")
lotes <- readOGR(paste0("dados/",arquivo_lotes))
lotes |> saveRDK("coleta/dados/lotes.RDK")

## unidades ####
print("Carga de dados do internos... unidades de atendimento...")
unidades <- readOGR(paste0("dados/",arquivo_unidades))

# extrai dados presente na descrição
unidades$bairro <- 
  sub('</td>.*', "", 
      sub(".*bairro</td> <td>", "", unidades$Description))
unidades |> saveRDK("coleta/dados/unidades.RDK")

# ## unidades de saúde ####
print("Carga de dados do internos... unidades de saúde...")
unidades_saude <- readOGR(paste0("dados/",arquivo_unidades_saude))
unidades_saude$CNES <- 
  sub('</td>.*', "", 
      sub(".*CNES</td> <td>", "", unidades_saude$Description))
unidades_saude |> saveRDK("coleta/dados/unidades_saude.RDK")

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
imoveis_geo_interno |> saveRDK("coleta/dados/imoveis_geo_interno.RDK")

print("Fim da carga de dados do internos.")
