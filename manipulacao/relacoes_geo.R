### Georreferenciamento das relações do munícipe
print("Georreferencia as relações com o munícipe...")
library(dplyr)
library(raster)
library(magrittr)
library(sp)
source("manipulacao/library.R")
source("config.R")

## Carregamento de dados ####

pessoas <- readRDS("manipulacao/dados/pessoas.RDS")
pessoas_assistencia <- readRDS("manipulacao/dados/pessoas_assistencia.RDS")
pessoas_educacao <- readRDS("manipulacao/dados/pessoas_educacao.RDS")
pessoas_saude <- readRDS("manipulacao/dados/pessoas_saude.RDS")
pessoas_fisica <- readRDS("manipulacao/dados/pessoas_fisica.RDS")
vinculos <- readRDS("coleta/dados/pessoas_vinculo.RDS")
imoveis <- readRDS("coleta/dados/imoveis.RDS")
imoveis_geo_osm <- readRDS("coleta/dados/imoveis_geo_osm.RDS")
imoveis_geo_interno <- readRDS("coleta/dados/imoveis_geo_interno.RDS")
unidades <- readRDS("coleta/dados/unidades.RDS")
# unidades_saude <- readRDS("coleta/dados/unidades_saude.RDS")
limite_cidade <- readRDS("coleta/dados/limite_cidade.RDS")

## Preparação ####

## imoveis
# pcode das pessoas físicas que possuem vículo com inscrições 
# cadastrais de imóveis
vinculos <- vinculos |> subset(tipoInscricao ==2)
imoveis_c_geo <- 
  pessoas_fisica$pcode[match(vinculos$codPessoa, pessoas_fisica$codPessoa)] |>
  as.data.frame()
names(imoveis_c_geo) <- "pcode"

# Informações para mostrar em cada imóvel
imoveis_c_geo$inscricao <- vinculos$inscricaoCadastral
imoveis_c_geo$relacao <- vinculos$tipoVinculoInscricao
imoveis_c_geo$responsavel <- vinculos$responsavel
imoveis_c_geo$fiscal <- vinculos$responsabilidadeFiscal
imoveis$endereco <- paste0(imoveis$tipoLogradouro, " ",
                           imoveis$nomeLogradouro,", n ",
                           imoveis$numero," ",
                           imoveis$complemento,", ",
                           imoveis$nomeBairro,", ",
                           CIDADE," - ",
                           UF,", ",
                           "BRASIL")
imoveis_c_geo <- imoveis_c_geo |>
  left_join(imoveis[,c("endereco",
                       "vlDebitoDA",
                       "vlIPTU",
                       "temDebitoInscricaoExercicio",
                       "vlVenalImovel",
                       "ocupacao",
                       "condicaoResponsavel",
                       "tipoIsencaoImunidadeIPTU",
                       "inscricaoCadastral")],
            by = c("inscricao" = "inscricaoCadastral"))

# união dos georreferenciamentos interno e osm
imoveis_geo_interno@coords <- imoveis_geo_interno@coords[,c(1,2)]
imoveis_geo <- rbind(imoveis_geo_interno, imoveis_geo_osm)

## Assistência
# Tentativa de melhorar os nomes dos CRAS
tabela_cras <-
  unidades@data$Name[grep("CRAS",(unidades@data$Name))] |>
  as.data.frame()
names(tabela_cras) <- "CRAS"
tabela_cras$bairro <- 
  unidades@data$bairro[grep("CRAS",(unidades@data$Name))]
tabela_cras$unidadeReferencia <- 
  paste("CRAS","-", tabela_cras$bairro |> toupper())

pessoas_assistencia[
  pessoas_assistencia$unidadeReferencia %in% tabela_cras$unidadeReferencia,
  "unidadeReferencia"] <- 
  tabela_cras$CRAS[match(pessoas_assistencia[
    pessoas_assistencia$unidadeReferencia %in% tabela_cras$unidadeReferencia,
    "unidadeReferencia"], tabela_cras$unidadeReferencia)]

# Separação dos dados georreferenciados

educacao_c_geo <- 
  separa_geo(
    pessoas_educacao,
    unidades)
educacao_s_geo <- educacao_c_geo$s_geo
educacao_c_geo <- educacao_c_geo$c_geo

assistencia_c_geo <- 
  separa_geo(
    pessoas_assistencia,
    unidades)
assistencia_s_geo <- assistencia_c_geo$s_geo
assistencia_c_geo <- assistencia_c_geo$c_geo

saude_c_geo <- 
  separa_geo(
    pessoas_saude,
    unidades,
    by = c("UnidadeReferencia" = "Name"))
saude_s_geo <- saude_c_geo$s_geo
saude_c_geo <- saude_c_geo$c_geo

imoveis_c_geo <- 
  separa_geo(
    imoveis_c_geo,
    imoveis_geo,
    by = c("inscricao" = "inscricaoCadastral"),
    limite_cidade = limite_cidade)
imoveis_s_geo <- imoveis_c_geo$s_geo
imoveis_c_geo <- imoveis_c_geo$c_geo

# Grava
saveRDS(assistencia_c_geo, 
        file = "manipulacao/dados/assistencia_c_geo.RDS")
saveRDS(assistencia_s_geo, 
        file = "manipulacao/dados/assistencia_s_geo.RDS")

saveRDS(educacao_c_geo, 
        file = "manipulacao/dados/educacao_c_geo.RDS")
saveRDS(educacao_s_geo, 
        file = "manipulacao/dados/educacao_s_geo.RDS")

saveRDS(saude_c_geo, 
        file = "manipulacao/dados/saude_c_geo.RDS")
saveRDS(saude_s_geo, 
        file = "manipulacao/dados/saude_s_geo.RDS")

saveRDS(imoveis_c_geo, 
        file = "manipulacao/dados/imoveis_c_geo.RDS")
saveRDS(imoveis_s_geo, 
        file = "manipulacao/dados/imoveis_s_geo.RDS")
