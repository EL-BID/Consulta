diffchar <- function(texto) {
  # verifica a quantidade de caracteres diferentes em uma string
  charToRaw(texto)|>unique()|>length()
}

limpa_nomes <- function(lista) {
  lista <- gsub("[^⁠[:alpha:] ]", "", lista)
  ajuste <- lapply(lista, diffchar) |> unlist()
  lista[ajuste <3] <- NA
  ajuste <- lapply(lista, nchar) |> unlist()
  lista[ajuste <4] <- NA
  lista <- lista |> str_trim()
  lista <- lista |> str_squish()
  lista <- lista |> toupper()
  lista[lista == ""] <- NA
  return(lista)
}

limpa_nome_rua <- function(lista) {
  lista <- gsub("[^⁠[:alnum:] ]", "", lista)
  ajuste <- lapply(lista, diffchar) |> unlist()
  lista[ajuste <3] <- NA
  ajuste <- lapply(lista, nchar) |> unlist()
  lista[ajuste <4] <- NA
  lista <- lista |> str_trim()
  lista <- lista |> str_squish()
  lista <- lista |> toupper()
  lista[lista == ""] <- NA
  return(lista)
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

junta_nome <- function(nome = NULL, base, nome_data, coluna = "NOME") {
  # Função para juntar todas as datas de nascimento
  
  # separa os nascimentos válidos
  nome_temp <- 
    base[base[[coluna]] |> is.na() |> not(),
         c("pcode",coluna, "dataAtualizacao")]
  # elimina as pessoas não identificadas
  nome_temp <- nome_temp[nome_temp$pcode |> is.na() |> not(),]
  # elimina acentos
  nome_temp[[coluna]] <- iconv(nome_temp[[coluna]],
                               from="UTF-8",to="ASCII//TRANSLIT")
  # junta tudo em um único data.frame
  names(nome_temp) <- c("pcode","valor",nome_data)
  if (nome |> is.null())
    return(nome_temp)
  else
    return(nome |> full_join(nome_temp, by = c("pcode", "valor")))
}

junta_email <- function(email = NULL, base, nome_data, coluna = "email") {
  # Função para juntar todos os e-mails
  
  # separa os eimails válidos
  email_temp <- 
    base[grep("@",base[[coluna]]),
         c("pcode",coluna, "dataAtualizacao")]
  # elimina as pessoas não identificadas
  email_temp <- email_temp[email_temp$pcode |> is.na() |> not(),]
  # ajusta formatação
  email_temp[[coluna]] <- gsub(" ","",email_temp[[coluna]])
  email_temp[[coluna]] <- email_temp[[coluna]] |> tolower()
  # junta tudo em um único data.frame
  names(email_temp) <- c("pcode","valor",nome_data)
  if (email |> is.null())
    return(email_temp)
  else
    return(email |> full_join(email_temp, by = c("pcode", "valor")))
}


junta_telefone <- function(telefone = NULL, base, nome_data, 
                           coluna = "telefone") {
  # Função para juntar todos os telefones
  
  # separa os telefones válidos
  telefone_temp <- 
    base[base[[coluna]] |> is.na() |>  not() &
           base[[coluna]] != "",
         c("pcode",coluna, "dataAtualizacao")]
  # elimina as pessoas não identificadas
  telefone_temp <- telefone_temp[telefone_temp$pcode |> is.na() |> not(),]

  # Separa pessoas com telefones múltiplos
  if (!identical(grep("\\|",telefone_temp[[coluna]]),integer(0))) {
    multiplos <- telefone_temp[grep("\\|",telefone_temp[[coluna]]),]
    telefone_temp <- telefone_temp[-grep("\\|",telefone_temp[[coluna]]),]
    multiplos <- multiplos |> cSplit(coluna, sep="|", type.convert=FALSE)
    colunas <- grep(coluna, names(multiplos))
    multiplos <- multiplos |>
      pivot_longer(cols=colunas |> all_of(), 
                   names_to = NULL, 
                   values_to = coluna)
    multiplos <- multiplos[,c(1,3,2)]
    telefone_temp <- rbind(telefone_temp, multiplos)
  }

  # Separa o DDD
  ddd <- strsplit(telefone_temp[[coluna]],")")
  ddd <- 1:length(ddd) |> lapply(\(i,tst = ddd) tst[[i]][1]) |> unlist()
  telefone_temp$ddd <- NA
  telefone_temp$ddd[grep("\\(", ddd)] <- ddd[grep("\\(", ddd)]
  telefone_temp$ddd <- gsub("[^0-9]","",telefone_temp$ddd)
  telefone_temp$ajuste <- lapply(telefone_temp$ddd, nchar) |> unlist()
  telefone_temp$ddd[telefone_temp$ajuste != 2] <- NA
  
  # Elimina caracteres não numéricos
  telefone_temp[[coluna]] <- gsub("[^0-9]","",telefone_temp[[coluna]])
  
  # Para os números que ainda não foram identificados ddd, retira os dois 
  # primeiros números caso a qtd de números seja 10 ou 11 (provavelmente com ddd)
  telefone_temp$ajuste <- lapply(telefone_temp[[coluna]], nchar) |> unlist()
  telefone_temp[telefone_temp$ddd |> is.na() & 
                  telefone_temp$ajuste %in% 10:11,"ddd"] <-
    telefone_temp[telefone_temp$ddd |> is.na() & 
                    telefone_temp$ajuste %in% 10:11,coluna] |>
    substr(1, 2)
  
  # Retira o ddd dos números já identificados
  telefone_temp[telefone_temp$ddd |> is.na() |> not(),coluna] <-
    telefone_temp[telefone_temp$ddd |> is.na() |> not(),coluna] |>
    substr(3, nchar(telefone_temp[telefone_temp$ddd |> is.na() |> not(),coluna]))
  
  # Elimina o ddd 00
  telefone_temp$ddd[telefone_temp$ddd == "00"] <- NA
  
  # Elimina números com um único caracter
  telefone_temp$ajuste <- lapply(telefone_temp[[coluna]], diffchar) |> unlist()
  telefone_temp <- telefone_temp |> subset(ajuste > 1)
  
  # Elimina os números que não tenham tamanho de telefone
  telefone_temp$ajuste <- lapply(telefone_temp[[coluna]], nchar) |> unlist()
  telefone_temp <- telefone_temp |> subset(ajuste %in% 8:9)
  
  # Formata
  telefone_temp[[coluna]] <-
    paste0(
      telefone_temp[[coluna]] |> substr(1, telefone_temp$ajuste |> as.numeric() -4),
      "-",
      telefone_temp[[coluna]] |> substr(telefone_temp$ajuste |> as.numeric() -3, telefone_temp$ajuste) 
    )
  
  telefone_temp[telefone_temp$ddd |> is.na() |> not(), coluna] <- 
    paste0(
      "(",
      telefone_temp[telefone_temp$ddd |> is.na() |> not(), "ddd"],
      ")",
      telefone_temp[telefone_temp$ddd |> is.na() |> not(), coluna]
    )
  
  # junta tudo em um único data.frame
  telefone_temp <- telefone_temp[,c("pcode",coluna, "dataAtualizacao")]
  names(telefone_temp) <- c("pcode","valor",nome_data)
  if (telefone |> is.null())
    return(telefone_temp)
  else
    return(telefone |> full_join(telefone_temp, by = c("pcode", "valor")))
}

junta_mae <- function(mae = NULL, base, nome_data, coluna = "mae") {
  # Função para juntar todas as mães
  
  # separa os eimails válidos
  mae_temp <- 
    base[base[[coluna]] != "",
         c("pcode",coluna, "dataAtualizacao")]
  # elimina as pessoas não identificadas
  mae_temp <- mae_temp[mae_temp$pcode |> is.na() |> not(),]
  mae_temp[[coluna]] <- gsub("[^⁠[:alpha:] ]", "", mae_temp[[coluna]])

  # limpa nomes
  mae_temp$ajuste <- lapply(mae_temp[[coluna]], diffchar) |> unlist()
  mae_temp <- mae_temp |> subset(ajuste > 2)
  mae_temp$ajuste <- lapply(mae_temp[[coluna]], nchar) |> unlist()
  mae_temp <- mae_temp |> subset(ajuste > 3)
  mae_temp$mae <- mae_temp$mae |> str_trim()
  mae_temp$mae <- mae_temp$mae |> str_squish()
  mae_temp$mae <- mae_temp$mae |> toupper()
  # Elimina principais nomes inadequados
  mae_temp <- mae_temp[(mae_temp$mae %in% c("A COMPLETAR",
                                            "A CONFIRMAR",
                                            "Á CONFIRMAR",
                                            "À CONFIRMAR",
                                            "A DECLARAR",
                                            "A SABER",
                                            "FULANA DE TAL",
                                            "IGNORADO",
                                            "MAE",
                                            "N INFORMADO",
                                            "NÃO DECLARADA",
                                            "NAO DECLARADO",
                                            "NÃO IDENTIFICADO",
                                            "NÃO INFORMADA",
                                            "NAO INFORMADO",
                                            "NÃO INFORMADO",
                                            "NI",
                                            "SEM INFORMACAO",
                                            "SEM INFORMAÇÃO")) |> not(),]

  # junta tudo em um único data.frame
  mae_temp <- mae_temp[,c("pcode", coluna, "dataAtualizacao")]
  names(mae_temp) <- c("pcode","valor",nome_data)
  if (mae |> is.null())
    return(mae_temp)
  else
    return(mae |> full_join(mae_temp, by = c("pcode", "valor")))
}

junta_nascimento <- function(nascimento = NULL, base, nome_data, coluna = "dataNascimento") {
  # Função para juntar todas as datas de nascimento
  
  # separa os nascimentos válidos
  nascimento_temp <- 
    base[base[[coluna]] |> is.na() |> not(),
         c("pcode",coluna, "dataAtualizacao")]
  # elimina as pessoas não identificadas
  nascimento_temp <- nascimento_temp[nascimento_temp$pcode |> is.na() |> not(),]
  #formata
  nascimento_temp[[coluna]] <- nascimento_temp[[coluna]] |> as.Date("%d/%m/%Y")
  # junta tudo em um único data.frame
  names(nascimento_temp) <- c("pcode","valor",nome_data)
  if (nascimento |> is.null())
    return(nascimento_temp)
  else
    return(nascimento |> full_join(nascimento_temp, by = c("pcode", "valor")))
}

junta_identidade <- function(identidade = NULL, base, nome_data, coluna = "rg") {
  # Função para juntar todos os documentos de identidade
  
  # separa os documentos válidos
  identidade_temp <- 
    base[base[[coluna]] |> is.na() |> not(),
         c("pcode",coluna, "dataAtualizacao")]
  # elimina as pessoas não identificadas
  identidade_temp <- identidade_temp[identidade_temp$pcode |> is.na() |> not(),]
  # elimina valores inadequados
  identidade_temp[[coluna]] <- gsub("[^⁠[:alnum:] /-]","",identidade_temp[[coluna]])
  identidade_temp$ajuste <- lapply(identidade_temp[[coluna]], diffchar) |> unlist()
  identidade_temp <- identidade_temp |> subset(ajuste > 1)
  identidade_temp$ajuste <- lapply(identidade_temp[[coluna]], nchar) |> unlist()
  identidade_temp <- identidade_temp |> subset(ajuste > 2)
  
  # junta tudo em um único data.frame
  identidade_temp <- identidade_temp[,c("pcode", coluna, "dataAtualizacao")]
  names(identidade_temp) <- c("pcode","valor",nome_data)
  if (identidade |> is.null())
    return(identidade_temp)
  else
    return(identidade |> full_join(identidade_temp, by = c("pcode", "valor")))
}

junta_endereco <- function(endereco = NULL, base, nome_data, coluna = "endereco") {
  # Função para juntar todos os documentos de endereco
  
  # separa os documentos válidos
  endereco_temp <- 
    base[base[[coluna]] |> is.na() |> not(),
         c("pcode",coluna, "dataAtualizacao")]
  # elimina as pessoas não identificadas
  endereco_temp <- endereco_temp[endereco_temp$pcode |> is.na() |> not(),]
  # elimina valores inadequados
  endereco_temp[[coluna]] <- endereco_temp[[coluna]] |> toupper()

  # junta tudo em um único data.frame
  endereco_temp <- endereco_temp[,c("pcode", coluna, "dataAtualizacao")]
  names(endereco_temp) <- c("pcode","valor",nome_data)
  if (endereco |> is.null())
    return(endereco_temp)
  else
    return(endereco |> full_join(endereco_temp, by = c("pcode", "valor")))
}

