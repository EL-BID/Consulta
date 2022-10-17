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
                           imoveis$nomeLogradouro,", N ",
                           imoveis$numero," ",
                           imoveis$complemento,", ",
                           imoveis$nomeBairro,", ",
                           CIDADE," - ",
                           UF,", ",
                           "BRASIL")
imoveis_c_geo <- imoveis_c_geo |>
  left_join(imoveis[,c("endereco",
                       "tipoLogradouro",
                       "nomeLogradouro",
                       "numero",
                       "complemento",
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

# Identificação das residencias

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

# Lista de endereços para sistema de navegação

lista1 <- 
  paste0(imoveis_c_geo@data$tipoLogradouro, " ",
         imoveis_c_geo@data$nomeLogradouro) |> 
  unique() |>
  as.data.frame()
names(lista1) <- "endereco"

lista2 <- 
  paste0(imoveis_c_geo@data$tipoLogradouro, " ",
         imoveis_c_geo@data$nomeLogradouro,", N ",
         imoveis_c_geo@data$numero) |> 
  unique () |> 
  as.data.frame()
names(lista2) <- "endereco"

lista_enderecos <- rbind(lista1,lista2)
lista_enderecos$indice <- 1:length(lista_enderecos$endereco)
lista_enderecos <- lista_enderecos[order(lista_enderecos$endereco),]

# Residências

endereco <- NULL

pessoas_fisica$LOGRADOURO <- pessoas_fisica$logradouro |> limpa_nome_rua()
pessoas_fisica$endereco <- paste0(pessoas_fisica$tipoLogradouro, " ",
                                  pessoas_fisica$LOGRADOURO,"#",
                                  pessoas_fisica$numero,"|",
                                  pessoas_fisica$complemento)
pessoas_fisica$endereco[pessoas_fisica$LOGRADOURO |> is.na()] <- NA
endereco <- endereco |> junta_endereco(pessoas_fisica, "dt_fisica")

pessoas_educacao$LOGRADOURO <- pessoas_educacao$logradouro |> limpa_nome_rua()
pessoas_educacao$endereco <- paste0(pessoas_educacao$LOGRADOURO,"#",
                                    pessoas_educacao$numero,"|",
                                    pessoas_educacao$complemento)
pessoas_educacao$endereco[pessoas_educacao$LOGRADOURO |> is.na()] <- NA
endereco <- endereco |> junta_endereco(pessoas_educacao, "dt_educacao")

pessoas_saude$LOGRADOURO <- pessoas_saude$Logradouro |> limpa_nome_rua()
pessoas_saude$endereco <- paste0(pessoas_saude$tipoLogradouro, " ",
                                 pessoas_saude$LOGRADOURO,"#",
                                 pessoas_saude$numero,"|",
                                 pessoas_saude$complemento)
pessoas_saude$endereco[pessoas_saude$LOGRADOURO |> is.na()] <- NA
endereco <- endereco |> junta_endereco(pessoas_saude, "dt_saude")

pessoas_assistencia$LOGRADOURO <- pessoas_assistencia$logradouro |> limpa_nome_rua()
pessoas_assistencia$endereco <- paste0(pessoas_assistencia$LOGRADOURO,"#",
                                       pessoas_assistencia$numero,"|",
                                       pessoas_assistencia$complemento)
pessoas_assistencia$endereco[pessoas_assistencia$LOGRADOURO |> is.na()] <- NA
endereco <- endereco |> junta_endereco(pessoas_assistencia, "dt_assistencia")

detalhado <- endereco[,c(1,2)] |> cSplit("valor","#")
detalhado <- detalhado[detalhado$valor_3 |> is.na(),]
detalhado <- detalhado[,c(1:3)] |> cSplit("valor_2","|")
detalhado <- detalhado[detalhado$valor_2_3 |> is.na(),c(1:4)]
names(detalhado) <- c("pcode","rua","num","complemento")



base <- imoveis_c_geo@data
base$nomeLogradouro
base$rua <- paste(base$tipoLogradouro,base$nomeLogradouro)

unico_imovel <- base |>
  group_by(pcode, rua) |>
  summarise(n = n(), .groups = "drop") |>
  filter(n == 1L)
  
teste <-  inner_join(unico_imovel,detalhado)
  
teste2 <- teste |> left_join(base, by = c("pcode", "rua"))

teste2$indice <- paste(teste2$pcode,".",teste2$inscricao)

imoveis_c_geo@data$indice <- paste(imoveis_c_geo@data$pcode,".",imoveis_c_geo@data$inscricao)

imoveis_s_geo$residente <- FALSE
imoveis_c_geo@data$residente <- FALSE
imoveis_c_geo@data$residente[
  (imoveis_c_geo@data$indice %in% teste2$indice) |> which()] <- TRUE

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

lista_enderecos |> saveRDS("manipulacao/dados/lista_enderecos.RDS")
