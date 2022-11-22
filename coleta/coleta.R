# coleta todas as informações necessárias

dir.create("coleta/dados", showWarnings = FALSE)

# dados do Data Warehouse via ODBC
source("coleta/dw_via_odbc.R")
rm(list=ls())
gc()

# Dados fornecidos pela prefeitura
source("coleta/dados_internos.R")
rm(list=ls())
gc()

# openstreetmap (georreferenciamento restante)
source("coleta/openstreetmap.R", encoding = "UTF-8")
rm(list=ls())
gc()
