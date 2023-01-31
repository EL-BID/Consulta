### Georreferenciamento das relações do munícipe
print("Georreferencia as relações com o munícipe...")
library(dplyr)
library(raster)
library(magrittr)
library(stringr)
library(splitstackshape)
library(sp)
source("manipulacao/library.R")
source("config.R")

## Carregamento de dados ####

imoveis_geo_osm <- readRDK("coleta/dados/imoveis_geo_osm.RDK")
imoveis_geo_interno <- readRDK("coleta/dados/imoveis_geo_interno.RDK")
unidades <- readRDK("coleta/dados/unidades.RDK")
unidades_saude <- readRDK("coleta/dados/unidades_saude.RDK")
limite_cidade <- readRDK("coleta/dados/limite_cidade.RDK")

campos <- read.csv2("dados/dicionario.csv", row.names = 1) |>
  as.data.frame()

for (base in colnames(campos)) {
  if (campos["cpf",base]=="")
    assign(base, readRDK(paste0("coleta/dados/",base,".RDK")))
  else
    assign(paste0("pessoas_",base),
           readRDK(paste0("manipulacao/dados/",base,".RDK")))
}

pessoas <- readRDK("manipulacao/dados/pessoas.RDK")


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

### Identificação das residencias #####
# Isso aqui identifica os proprietários (e afins) que moram em seus imóveis
endereco <- NULL

pessoas_fisica$LOGRADOURO <- pessoas_fisica$logradouro |> limpa_nome_rua()
pessoas_fisica$endereco <- paste0(pessoas_fisica$tipoLogradouro, " ",
                                  pessoas_fisica$LOGRADOURO,"§",
                                  pessoas_fisica$numero,"¢",
                                  pessoas_fisica$complemento)
pessoas_fisica$endereco[pessoas_fisica$LOGRADOURO |> is.na()] <- NA
endereco <- endereco |> junta_endereco(pessoas_fisica, "dt_fisica")

pessoas_educacao$LOGRADOURO <- pessoas_educacao$logradouro |> limpa_nome_rua()
pessoas_educacao$endereco <- paste0(pessoas_educacao$LOGRADOURO,"§",
                                    pessoas_educacao$numero,"¢",
                                    pessoas_educacao$complemento)
pessoas_educacao$endereco[pessoas_educacao$LOGRADOURO |> is.na()] <- NA
endereco <- endereco |> junta_endereco(pessoas_educacao, "dt_educacao")

pessoas_saude$LOGRADOURO <- pessoas_saude$Logradouro |> limpa_nome_rua()
pessoas_saude$endereco <- paste0(pessoas_saude$tipoLogradouro, " ",
                                 pessoas_saude$LOGRADOURO,"§",
                                 pessoas_saude$numero,"¢",
                                 pessoas_saude$complemento)
pessoas_saude$endereco[pessoas_saude$LOGRADOURO |> is.na()] <- NA
endereco <- endereco |> junta_endereco(pessoas_saude, "dt_saude")

pessoas_assistencia$LOGRADOURO <- pessoas_assistencia$logradouro |> limpa_nome_rua()
pessoas_assistencia$endereco <- paste0(pessoas_assistencia$LOGRADOURO,"§",
                                       pessoas_assistencia$numero,"¢",
                                       pessoas_assistencia$complemento)
pessoas_assistencia$endereco[pessoas_assistencia$LOGRADOURO |> is.na()] <- NA
endereco <- endereco |> junta_endereco(pessoas_assistencia, "dt_assistencia")

endereco <- endereco[,c(1,2)] |> cSplit("valor","§")
endereco <- endereco[,c(1:3)] |> cSplit("valor_2","¢")
names(endereco) <- c("pcode","rua","num","complemento")
endereco$ruanum <- paste(endereco$rua, endereco$num)
endereco$ruanumcomplemento <- paste(endereco$rua, endereco$num, endereco$complemento)

base <- imoveis_c_geo
base$nomeLogradouro <- base$nomeLogradouro |> limpa_nome_rua()
base$rua <- paste(base$tipoLogradouro,base$nomeLogradouro)
base$ruanum <- paste(base$tipoLogradouro,base$nomeLogradouro,base$numero)
base$ruanumcomplemento <- paste(base$tipoLogradouro,base$nomeLogradouro,base$numero,base$complemento)

# Se só possui um imóvel na rua e essa rua é registrada como endereço, esse
# imóvei é a residência
unico_imovel <- base |>
  group_by(pcode, rua) |>
  summarise(n = n(), .groups = "drop") |>
  filter(n == 1L)

unico_imovel <-  unico_imovel |> inner_join(endereco)
unico_imovel <- unico_imovel |> left_join(base, by = c("pcode", "rua"))
unico_imovel$indice <- paste(unico_imovel$pcode,".",unico_imovel$inscricao)

imoveis_c_geo$indice <- paste(imoveis_c_geo$pcode,".",imoveis_c_geo$inscricao)
imoveis_c_geo$residente <- FALSE
imoveis_c_geo$residente[
  (imoveis_c_geo$indice %in% unico_imovel$indice)] <- TRUE

# Se só possui um imóvel na rua e número e essa rua e num é registrada como 
# endereço, esse imóvei é a residência
unico_imovel <- base |>
  group_by(pcode, ruanum) |>
  summarise(n = n(), .groups = "drop") |>
  filter(n == 1L)

unico_imovel <-  unico_imovel |> inner_join(endereco)
unico_imovel <- unico_imovel |> left_join(base, by = c("pcode", "rua"))
unico_imovel$indice <- paste(unico_imovel$pcode,".",unico_imovel$inscricao)

imoveis_c_geo$residente[
  imoveis_c_geo$indice %in% unico_imovel$indice] <- TRUE

# Se possui + de um imóvel, é preciso verificar se há semelhança no complemento
multiplo_imovel <- base |>
  group_by(pcode, ruanumcomplemento) |>
  summarise(n = n(), .groups = "drop") |>
  filter(n > 1L)

multiplo_imovel <-  multiplo_imovel |> inner_join(endereco)
multiplo_imovel <- multiplo_imovel |> left_join(base, by = c("pcode", "ruanum"))
multiplo_imovel$complemento.x[multiplo_imovel$complemento.x |> is.na()]  <- ""
multiplo_imovel$complemento.y[multiplo_imovel$complemento.y |> is.na()]  <- ""
multiplo_imovel$residente <- FALSE
multiplo_imovel$residente <-
  multiplo_imovel$complemento.x == multiplo_imovel$complemento.y

multiplo_imovel = nao_deu <- multiplo_imovel[multiplo_imovel$residente,]

nao_deu <- nao_deu |>
  group_by(pcode) |>
  summarise(n = n(), .groups = "drop") |>
  filter(n > 1L)

multiplo_imovel <- multiplo_imovel[(multiplo_imovel$pcode %in% nao_deu$pcode) |> not(),]
multiplo_imovel$indice <- paste(multiplo_imovel$pcode,".",multiplo_imovel$inscricao)

imoveis_c_geo$residente[
  imoveis_c_geo$indice %in% multiplo_imovel$indice] <- TRUE

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
    unidades_saude,
    by = c("CNES" = "CNES"))
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

# Grava
saveRDK(assistencia_c_geo, 
        file = "manipulacao/dados/assistencia_c_geo.RDK")
saveRDK(assistencia_s_geo, 
        file = "manipulacao/dados/assistencia_s_geo.RDK")

saveRDK(educacao_c_geo, 
        file = "manipulacao/dados/educacao_c_geo.RDK")
saveRDK(educacao_s_geo, 
        file = "manipulacao/dados/educacao_s_geo.RDK")

saveRDK(saude_c_geo, 
        file = "manipulacao/dados/saude_c_geo.RDK")
saveRDK(saude_s_geo, 
        file = "manipulacao/dados/saude_s_geo.RDK")

saveRDK(imoveis_c_geo, 
        file = "manipulacao/dados/imoveis_c_geo.RDK")
saveRDK(imoveis_s_geo, 
        file = "manipulacao/dados/imoveis_s_geo.RDK")

lista_enderecos |> saveRDK("manipulacao/dados/lista_enderecos.RDK")
