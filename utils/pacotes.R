print("instalando e carregando pacotes requeridos...")
packages <- c(
  "DBI",
  "odbc",
  "dplyr",
  "rgdal",
  "cartography",
  "osmdata",
  "raster",
  "sf",
  "rgdal",
  "tidygeocoder",
  "stringr",
  "shiny",
  "shinythemes",
  "shinyWidgets",
  "leaflet",
  "RColorBrewer",
  "magrittr",
  "splitstackshape",
  'xlsx',
  "tidyr",
  "sp",
  "rgeos",
  "DT",
  "shinymanager",
  "openssl",
  "keyring"
)

install.packages(setdiff(packages, rownames(installed.packages())))  

# Carregando pacontes
lapply(packages,library,character.only = T)



