library(shiny)
library(shinythemes)
library(leaflet)
library(htmlwidgets)
library(magrittr)
library(DT)
library(shinymanager)
library(keyring)

# Carrega dados
pessoas <- readRDS("dados/pessoas.RDS")
pessoas_imobiliario <- readRDS("dados/pessoas_imobiliario.RDS")
pessoas_educacao <- readRDS("dados/pessoas_educacao.RDS")
pessoas_assistencia <- readRDS("dados/pessoas_assistencia.RDS")
pessoas_saude <- readRDS("dados/pessoas_saude.RDS")
pessoas_fisica <- readRDS("dados/pessoas_fisica.RDS")
imoveis_c_geo <- readRDS("dados/imoveis_c_geo.RDS")
educacao_c_geo <- readRDS("dados/educacao_c_geo.RDS")
saude_c_geo <- readRDS("dados/saude_c_geo.RDS")
assistencia_c_geo <- readRDS("dados/assistencia_c_geo.RDS")
imoveis_s_geo <- readRDS("dados/imoveis_s_geo.RDS")
educacao_s_geo <- readRDS("dados/educacao_s_geo.RDS")
saude_s_geo <- readRDS("dados/saude_s_geo.RDS")
assistencia_s_geo <- readRDS("dados/assistencia_s_geo.RDS")
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
  right: 10px;
  width: 34px;
  height: 34px;
  font-size: 22px;
  text-align: center;
  background-color: transparent;"
