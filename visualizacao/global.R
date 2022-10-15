library(shiny)
library(shinythemes)
library(leaflet)
library(htmlwidgets)
library(magrittr)
library(DT)
library(shinymanager)
library(keyring)
library(dplyr)


# Carrega dados
pessoas <- readRDS("dados/pessoas.RDS")
info_pessoais <- readRDS("dados/info_pessoais.RDS")
# pessoas_imobiliario <- readRDS("dados/pessoas_imobiliario.RDS")
# pessoas_educacao <- readRDS("dados/pessoas_educacao.RDS")
# pessoas_assistencia <- readRDS("dados/pessoas_assistencia.RDS")
# pessoas_saude <- readRDS("dados/pessoas_saude.RDS")
# pessoas_fisica <- readRDS("dados/pessoas_fisica.RDS")
imoveis_c_geo <- readRDS("dados/imoveis_c_geo.RDS")
educacao_c_geo <- readRDS("dados/educacao_c_geo.RDS")
saude_c_geo <- readRDS("dados/saude_c_geo.RDS")
assistencia_c_geo <- readRDS("dados/assistencia_c_geo.RDS")
imoveis_s_geo <- readRDS("dados/imoveis_s_geo.RDS")
educacao_s_geo <- readRDS("dados/educacao_s_geo.RDS")
saude_s_geo <- readRDS("dados/saude_s_geo.RDS")
assistencia_s_geo <- readRDS("dados/assistencia_s_geo.RDS")
lotes <- readRDS("dados/lotes.RDS")
mybbox <- imoveis_c_geo@bbox

# definições de estilos:
# botão de ferramenta
btn_pressionado <- 
  "z-index:4000;
  right: 10px;
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
  right: 10px;
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
  
  #Plota no mapa
  mapa |>
    addCircleMarkers(
      data = dados,
      layerId = id,
      color = cor,
      radius = 5,
      weight = 2,
      label = label,
      group = "municipe"
    )
  
  #Retorna a BBOX
  return(dados@bbox)
}
