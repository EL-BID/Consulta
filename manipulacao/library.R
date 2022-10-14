
diffchar <- function(texto) {
  # verifica a quantidade de caracteres diferentes em uma string
  charToRaw(texto)|>unique()|>length()
}

cpf_validar <- function(cpf){
  # verifica se o CPF é válido, comparando o dígito verificador
  digitos <- str_sub(cpf,-2,-1)
  v1 <- 0L
  v2 <- 0L
  z <- as.integer(strsplit(as.character(str_sub(cpf,1,9)), "")[[1]])
  for(i in seq_along(z)){
    v1 <- v1 + z[i]*(11-i)
    v2 <- v2 + z[i]*(12-i)
  }
  v1 <- v1 %% 11
  if(v1 < 2){
    v1 <- 0
  } else{
    v1 <- 11 - v1
  }
  v2 <- v2 + 2*v1
  v2 <- v2 %% 11
  if(v2 < 2){
    v2 <- 0
  } else{
    v2 <- 11 - v2
  }
  digitos == paste0(v1, v2)
}

separa_geo <- function(base, 
                       unidades, 
                       by = c("unidadeReferencia" = "Name"), 
                       limite_cidade = NULL){
  ### Função para separa os dados de pessoas da base de acordo com o geocode

  # Separa apenas as pessoas
  base_c_geo <- 
    base[base$pcode |> is.na() |> not(),] |>
    as.data.frame()

  # Obtém as coordenadas
  base_c_geo[,c("long","lat")] <-
    coordinates(unidades)[
      match(base_c_geo[[names(by)]] |> toupper(),
            unidades[[by]] |> toupper()),c(1,2)]
  
  # Separa os dados sem georreferenciamento
  base_s_geo <- 
    base_c_geo[is.na(base_c_geo$long),
               !names(base_c_geo) %in% c("long","lat")]
  base_c_geo <- base_c_geo[!is.na(base_c_geo$long),]
  
  # Converte para SpatialPointsDataFrame
  coordinates(base_c_geo) <- c("long","lat")
  proj4string(base_c_geo) <- "+proj=longlat +datum=WGS84 +no_defs"
  
  # Separa os dados que não estão na cidade e junta com imoveis_s_geo
  if (limite_cidade |> is.null() |> not()) {
    na_cidade <-  base_c_geo %over% (limite_cidade |> as("SpatialPolygons"))
    base_s_geo <-  rbind(base_s_geo,
                         base_c_geo@data[na_cidade |>is.na() |> which(),])
    base_c_geo <- base_c_geo[(na_cidade==1)|>which(),]
  }
  
  # Elimina imóveis sem pessoas
  resultado <- NULL
  resultado$c_geo <- base_c_geo |> subset(pcode |> is.na() |> not())
  resultado$s_geo <- base_s_geo |> subset(pcode |> is.na() |> not())
  
  # Retorna o resultado em uma lista com as duas bases
  resultado
}
