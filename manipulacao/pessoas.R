# Junta todas as pessoas em um único dataframe
print("Agrupa dados das pessoas...")

library(stringr)
library(magrittr)
library(dplyr)
library(splitstackshape)
library(tidyr)
source("manipulacao/library.R")
source("config.R")

# Carrega dicionário de dados, apenas com dados de bases de pessoas
campos <- read.csv2("dados/dicionario.csv", row.names = 1) |>
  as.data.frame()
campos <- campos[,campos["cpf",] != ""]
campos[campos == ""] <- NA

print("Agrupa dados das pessoas... Carregando informações...")
bases <- NULL
for (base in colnames(campos)) {
  print(paste("Agrupa dados das pessoas... Carregando informações...", base))
  bases[[base]] <- 
    readRDS(paste0("coleta/dados/",base,".RDS"))
}

print("Agrupa dados das pessoas... Criando arquivo de pessoas...")

pessoas <- NULL

for (base in colnames(campos)) {
  # Cria códigos próprios para cada lançamento na tabela e elimina 
  # caracteres não numéricos do CPF
  bases[[base]][[campos["code",base]]] <- 
    1:length(bases[[base]][[campos["cpf",base]]])
  bases[[base]]$ncpf <- 
    gsub("[^0-9]","",bases[[base]][[campos["cpf",base]]])

  # Padroniza nomes
  print(paste("Agrupa dados das pessoas... Criando arquivo de pessoas...",
              "Padronizando nomes...",base))
  bases[[base]]$NOME <- 
    bases[[base]][[campos["nome",base]]] |> limpa_nomes()

  # Junta todos os cpfs em uma única base
  pessoas <- 
    c(pessoas, 
      bases[[base]]$ncpf)
}

pessoas <- pessoas |> unique()|> as.data.frame()
names(pessoas) <- "cpf"

# Elimina todos os documentos que não possuem tamanho de cpf
pessoas$pcode <- lapply(pessoas$cpf, nchar)
pessoas <- pessoas |> subset(pcode == 11)

# Elimina documentos com um único caracter
pessoas$pcode <- lapply(pessoas$cpf, diffchar)
pessoas <- pessoas |> subset(pcode > 1)

# Elimina cpfs inválidos
print(paste("Agrupa dados das pessoas... Criando arquivo de pessoas...",
            "Validando CPFs..."))
pessoas$pcode <- 
  sapply(pessoas$cpf, cpf_validar)
pessoas <- pessoas |> subset(pcode)
print(paste("Agrupa dados das pessoas... Criando arquivo de pessoas...",
            "Validando CPFs... FIM"))

# Cria código único para tabela pessoas
pessoas <- pessoas[,c(2,1)]
pessoas$pcode <- 1:length(pessoas$pcode)

# Vincular bases
pessoas$nome <- NA
for (base in colnames(campos)) {
  # Preencher nomes
  pessoas$nome[pessoas$nome |> is.na()] <- 
    bases[[base]]$NOME[match(pessoas$cpf[pessoas$nome |> is.na()],
                                    bases[[base]]$ncpf)]

  # Vincula as pessoas com as bases anteriores
  pessoas[[campos["code",base]]] <- 
    bases[[base]][[campos["code",base]]][match(pessoas$cpf,
                                                      bases[[base]]$ncpf)]
  #vincula as bases com as pessoas
  bases[[base]]$pcode <- 
    pessoas$pcode[match(bases[[base]][[campos["code",base]]], 
                        pessoas[[campos["code",base]]])]
}

# Incluir todas as pessoas sem CPF
for (base in colnames(campos)) {
  # Juntar todas as outras pessoas que não possuem cpf adequado...
  # O cadastro dessas pessoas será cruzado posteriormente no 
  # módulo complementar_pessoas.R
  ptemp <- 
    bases[[base]][bases[[base]]$pcode |> is.na(),
                         c(campos["code",base], "ncpf", "NOME")]
  bases[[base]][bases[[base]]$pcode |> is.na(),"pcode"] = 
    ptemp$pcode <- 
    paste0(base,".",ptemp[[campos["code",base]]])
  
  for (code in campos["code",names(campos) != base]) {
    ptemp[[code]] <- NA
  }
  
  ptemp <- ptemp[,c("pcode", "ncpf", "NOME", campos["code",] |> as.character())]
  names(ptemp) <- names(pessoas)
  pessoas <- rbind(pessoas,ptemp)
}

# Ordena as pessoas pelos nomes
print(paste("Agrupa dados das pessoas... Criando arquivo de pessoas... Ordenando..."))
pessoas <- pessoas[order(pessoas$nome),]

print("Agrupa dados das pessoas... Salvando informações de pessoas...")

pessoas |> saveRDS("manipulacao/dados/pessoas.RDS")
for (base in names(campos)) {
  bases[[base]] |> saveRDS(paste0("manipulacao/dados/",base,".RDS"))
}

# Separa as informações pessoais
print("Agrupa dados das pessoas...")
nome <- NULL
email <- NULL
telefone <- NULL
mae <- NULL
nascimento <- NULL
identidade <- NULL
ctps <- NULL
endereco <- NULL

for (base in names(campos)) {
  # Ajusta as datas de atualização das bases que possuem data de inclusão
  if (campos["inclusao",base] |> is.na() |> not()) {
    bases[[base]][[campos["atualizacao",base]]][
      bases[[base]][[campos["atualizacao",base]]] |> is.na()] <- 
      bases[[base]][[campos["inclusao",base]]][
        bases[[base]][[campos["atualizacao",base]]] |> is.na()]
  }
  
  # Padroniza o formato das datas
  bases[[base]][[campos["atualizacao",base]]] <- 
    bases[[base]][[campos["atualizacao",base]]] |>
    as.Date("%d/%m/%Y") |> 
    format("%d/%m/%Y")  

  print(paste("Agrupa dados das pessoas... Coletando nomes...",base))
  nome <- nome |> 
    junta_nome(bases[[base]], paste0("dt_",base), "NOME")
  
  print(paste("Agrupa dados das pessoas... Coletando e-mails...",base))
  email <- email |> 
    junta_email(bases[[base]], paste0("dt_",base), campos["email",base])

  print(paste("Agrupa dados das pessoas... Coletando telefones...",base))
  telefone <- telefone |> 
    junta_telefone(bases[[base]], paste0("dt_",base), campos["telefone",base])

  print(paste("Agrupa dados das pessoas... Coletando filiação...",base))
  mae <- mae |> 
    junta_mae(bases[[base]], paste0("dt_",base), campos["mae",base])
  
  print(paste("Agrupa dados das pessoas... Coletando nascimento...",base))
  nascimento <- nascimento |> 
    junta_nascimento(bases[[base]], paste0("dt_",base), campos["nascimento",base])
  
  print(paste("Agrupa dados das pessoas... Coletando identidade...",base))
  identidade <- identidade |> 
    junta_identidade(bases[[base]], paste0("dt_",base), campos["identidade",base])
  
  print(paste("Agrupa dados das pessoas... Coletando ctps...",base))
  ctps <- ctps |> 
    junta_ctps(bases[[base]], paste0("dt_",base), campos["ctps",base])

  print(paste("Agrupa dados das pessoas... Coletando endereços...",base))
  # Formata endereço
  bases[[base]]$LOGRADOURO <- 
    bases[[base]][[campos["logradouro",base]]] |>
    limpa_nome_rua()
  if (campos["tipo_logradouro",base] |> is.na()) {
    bases[[base]]$endereco <- ""
  } else {
    bases[[base]]$endereco <- 
      paste0(bases[[base]][[campos["tipo_logradouro",base]]]," ")
  }
  bases[[base]]$endereco <- paste0(
    bases[[base]]$endereco,
    bases[[base]]$LOGRADOURO,", N ",
    bases[[base]][[campos["numero",base]]],"#",
    bases[[base]][[campos["complemento",base]]],", ",
    bases[[base]][[campos["bairro",base]]],", ",
    bases[[base]][[campos["cidade",base]]]," - ",
    bases[[base]][[campos["uf",base]]],", ",
    "BRASIL")
  bases[[base]]$endereco[bases[[base]]$LOGRADOURO |> is.na()] <- NA
  endereco <- endereco |> 
    junta_endereco(bases[[base]], paste0("dt_",base), "endereco")
}

# Só é necessario guardar nomes duplicados
pcodes_duplicados <- nome$pcode[nome$pcode |> duplicated()] |> unique()
nome <- nome[nome$pcode %in% pcodes_duplicados,]
nome$campo <- "Nome"
email$campo <- "E-mail"
telefone$campo <- "Telefone"
mae$campo <- "Mãe"
nascimento$campo <- "Nascimento"
identidade$campo <- "Identidade"
ctps$campo <- "CTPS"
endereco$campo <- "Endereço"

print("Agrupa dados das pessoas... Gravando...")

info_pessoais <- rbind(
  nome,
  telefone,
  email,
  nascimento,
  identidade,
  ctps,
  mae,
  endereco
)

info_pessoais <- info_pessoais[info_pessoais$pcode |> is.na() |> not(),]

saveRDS(info_pessoais,
        file = "manipulacao/dados/info_pessoais.RDS")

print("Agrupa dados das pessoas... fim.")