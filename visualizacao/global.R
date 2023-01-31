library(shiny)
library(shinythemes)
library(leaflet)
library(htmlwidgets)
library(magrittr)
library(DT)
library(shinymanager)
library(keyring)
library(dplyr)
library(tidyr)
library(xlsx)
library(cachem)
library(sp)
library(splitstackshape)
library(openssl)

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

# Carrega dados
pessoas <- readRDK("dados/pessoas.RDK")
info_pessoais <- readRDK("dados/info_pessoais.RDK")
imoveis_c_geo <- readRDK("dados/imoveis_c_geo.RDK")
educacao_c_geo <- readRDK("dados/educacao_c_geo.RDK")
saude_c_geo <- readRDK("dados/saude_c_geo.RDK")
assistencia_c_geo <- readRDK("dados/assistencia_c_geo.RDK")
imoveis_s_geo <- readRDK("dados/imoveis_s_geo.RDK")
educacao_s_geo <- readRDK("dados/educacao_s_geo.RDK")
saude_s_geo <- readRDK("dados/saude_s_geo.RDK")
assistencia_s_geo <- readRDK("dados/assistencia_s_geo.RDK")
lotes <- readRDK("dados/lotes.RDK")
lotes_sem_dados <- readRDK("dados/lotes_sem_dados.RDK")
imoveis_p_lote <- readRDK("dados/imoveis_p_lote.RDK")
lista_enderecos <- readRDK("dados/lista_enderecos.RDK")
status <- readRDS("dados/status.RDS")
mybbox <- imoveis_c_geo@bbox

chave <- readRDS("dados/chave.RDS")

shinyOptions(cache = cache_disk("dados/cache/"))

# definições de estilos:
# informações de teste
if (status$teste) {
  footer_height <- 55
  info_login <- 
    withTags(
      table(
        witdh = "100%",
        style = "margin: auto;",
        tr(
          td(
            width = "100%",
            style = "font-size: 24px; font-weight: bold;",
            "Consulta Munícipe"
          )
        ),
        tr(
          td(
            style = "font-size: 16px; font-weight: bold; color: red;",
            "(Versão de teste. Dados fictícios.)"
          )
        ),
        tr(
          td(
            strong("Usuário:"),
            "admin",
            strong("Senha:"),
            "admin"
          )
        )
      )
    )
  
} else {
  footer_height <- 25
  info_login <- "Consulta Munícipe"
}

# botão de ferramenta
btn_pressionado <- 
  "z-index:4000;
  right: 55px;
  width: 34px;
  height: 34px;
  background-color: #C3C3C3;
  border-bottom-left-radius: 4px;
  border-bottom-right-radius: 4px;
  border-top-left-radius: 4px;
  border-top-right-radius: 4px;
  border: 2px solid rgba(0,0,0,0.2);"

btn_normal <-
  "z-index:3000;
  right: 55px;
  width: 34px;
  height: 34px;
  background-color: #fff;
  border-bottom-left-radius: 4px;
  border-bottom-right-radius: 4px;
  border-top-left-radius: 4px;
  border-top-right-radius: 4px;
  border: 2px solid rgba(0,0,0,0.2);"

btn_rotulo <- 
  "z-index:5000;
  right: 55px;
  width: 34px;
  height: 34px;
  font-size: 22px;
  text-align: center;
  background-color: transparent;"

info_cabecalho <- withTags(table(
  class = 'compact',
  thead(
    border = 1,
    tr(
      th(
        rowspan = 2,
        colspan = 2,
        style = "
          vertical-align: middle;
          text-align: center;
          font-size: 18px;
          ",
        "Informações pessoais"),
      th(
        colspan = 4,
        style = "text-align: center;",
        'Data da última atualização')
    ),
    tr(
      th('Fiscal'),
      th('Educação'),
      th('Saúde'),
      th('Assistência')
    )
  )
))





plotar_relacoes <- function(relacao, cor, label, id = NULL, mapa, pcode) {
  
  dados <- relacao[relacao@data$pcode == pcode,]
  
  if (dados@data[1,1] |> is.na()) return(NULL)
  
  # Define layerId
  if (id |> is.null() |> not())
    id <- dados@data[[id]]
  
  # Define label
  if (label |> length() == 2)
    label <- 
      sprintf("%s | %s", dados@data[,label[1]], dados@data[,label[2]]) |>
      lapply(HTML)
  else
    label <- dados@data[,label] |> lapply(HTML)
  
  if (cor == "blue"){
    icones <- "function (cluster) {    
    var childCount = cluster.getChildCount(); 
    var c = ' marker-cluster-blue';  
    return new L.DivIcon({ html: '<div><span>' + childCount + '</span></div>', className: 'marker-cluster' + c, iconSize: new L.Point(40, 40) });

  }"
  } else if (cor == "red"){
    icones <- "function (cluster) {    
    var childCount = cluster.getChildCount(); 
    var c = ' marker-cluster-red';  
    return new L.DivIcon({ html: '<div><span>' + childCount + '</span></div>', className: 'marker-cluster' + c, iconSize: new L.Point(40, 40) });

  }"
  } else {
    icones <- "function (cluster) {    
    var childCount = cluster.getChildCount(); 
    var c = ' marker-cluster-small';  
    return new L.DivIcon({ html: '<div><span>' + childCount + '</span></div>', className: 'marker-cluster' + c, iconSize: new L.Point(40, 40) });

  }"
  }

  #Plota no mapa
  mapa |>
    addCircleMarkers(
      data = dados,
      layerId = id,
      color = cor,
      radius = 10,
      weight = 1,
      label = label,
      clusterOptions = markerClusterOptions(iconCreateFunction=JS(icones)),
      group = "municipe"
    )
  
  #Retorna a BBOX
  return(dados@bbox)
}

concatenar <- function(...){
  paste0(..., collapse = "|")
}


