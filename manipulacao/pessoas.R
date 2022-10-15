# Junta todas as pessoas em um único dataframe
print("Agrupa dados das pessoas...")

library(stringr)
library(magrittr)
library(dplyr)
library(splitstackshape)
library(tidyr)
source("manipulacao/library.R")
source("config.R")

pessoas_imobiliario <-
        readRDS("coleta/dados/pessoas_imobiliario.RDS")
pessoas_educacao <-
        readRDS("coleta/dados/pessoas_educacao.RDS")
pessoas_assistencia <-
        readRDS("coleta/dados/pessoas_assistencia.RDS")
pessoas_saude <-
        readRDS("coleta/dados/pessoas_saude.RDS")
pessoas_fisica <-
        readRDS("coleta/dados/pessoas_fisica.RDS")
pessoas_vinculo <-
        readRDS("coleta/dados/pessoas_vinculo.RDS")

# Cria códigos próprios para cada lançamento na tabela
pessoas_assistencia$acode <- 1:length(pessoas_assistencia$cpf)
pessoas_educacao$ecode <- 1:length(pessoas_educacao$CPF)
pessoas_fisica$fcode <- 1:length(pessoas_fisica$cpf)
pessoas_imobiliario$icode <- 1:length(pessoas_imobiliario$cpf)
pessoas_saude$scode <- 1:length(pessoas_saude$CPF)

# Elimina caracteres não numéricos
pessoas_assistencia$ncpf <- gsub("[^0-9]","",pessoas_assistencia$cpf)
pessoas_educacao$ncpf <- gsub("[^0-9]","",pessoas_educacao$CPF)
pessoas_fisica$ncpf <- gsub("[^0-9]","",pessoas_fisica$cpf)
pessoas_imobiliario$ncpf <- gsub("[^0-9]","",pessoas_imobiliario$cpf)
pessoas_saude$ncpf <- gsub("[^0-9]","",pessoas_saude$CPF)

# Padroniza nomes
pessoas_assistencia$NOME <- pessoas_assistencia$nome |> limpa_nomes()
pessoas_educacao$NOME <- pessoas_educacao$nome |> limpa_nomes()
pessoas_fisica$NOME <- pessoas_fisica$nome |> limpa_nomes()
pessoas_imobiliario$NOME <- pessoas_imobiliario$nome |> limpa_nomes()
pessoas_saude$NOME <- pessoas_saude$Nome |> limpa_nomes()

# Junta todos os cpfs em uma única base
pessoas <- c(pessoas_assistencia$ncpf,
                    pessoas_educacao$ncpf,
                    pessoas_fisica$ncpf,
                    pessoas_imobiliario$ncpf,
                    pessoas_saude$ncpf) |> unique()|> as.data.frame()
names(pessoas) <- "cpf"

# Elimina todos os documentos que não possuem tamanho de cpf
pessoas$pcode <- lapply(pessoas$cpf, nchar)
pessoas <- pessoas |> subset(pcode == 11)

# Elimina documentos com um único caracter
pessoas$pcode <- lapply(pessoas$cpf, diffchar)
pessoas <- pessoas |> subset(pcode > 1)

# Elimina cpfs inválidos
pessoas$pcode <- 
  sapply(pessoas$cpf, cpf_validar)
pessoas <- pessoas |> subset(pcode)

# Cria código único para tabela pessoas
pessoas <- pessoas[,c(2,1)]
pessoas$pcode <- 1:length(pessoas$pcode)

# Preencher nome por ordem de preferência
pessoas$nome <- 
  pessoas_fisica$NOME[match(pessoas$cpf,
                            pessoas_fisica$ncpf)]
pessoas$nome[pessoas$nome |> is.na()] <- 
  pessoas_imobiliario$NOME[match(pessoas$cpf[pessoas$nome |> is.na()],
                                 pessoas_imobiliario$ncpf)]
pessoas$nome[pessoas$nome |> is.na()] <- 
  pessoas_assistencia$NOME[match(pessoas$cpf[pessoas$nome |> is.na()],
                                pessoas_assistencia$ncpf)]
pessoas$nome[pessoas$nome |> is.na()] <- 
  pessoas_saude$NOME[match(pessoas$cpf[pessoas$nome |> is.na()],
                           pessoas_saude$ncpf)]
pessoas$nome[pessoas$nome |> is.na()] <- 
  pessoas_educacao$NOME[match(pessoas$cpf[pessoas$nome |> is.na()],
                              pessoas_educacao$ncpf)]

# Vincula as pessoas com as tabelas anteriores
pessoas$fcode <- 
  pessoas_fisica$fcode[match(pessoas$cpf,
                            pessoas_fisica$ncpf)]
pessoas$icode <- 
  pessoas_imobiliario$icode[match(pessoas$cpf,
                                    pessoas_imobiliario$ncpf)]
pessoas$acode <- 
  pessoas_assistencia$acode[match(pessoas$cpf,
                                   pessoas_assistencia$ncpf)]
pessoas$scode <- 
  pessoas_saude$scode[match(pessoas$cpf,
                              pessoas_saude$ncpf)]
pessoas$ecode <- 
  pessoas_educacao$ecode[match(pessoas$cpf,
                                 pessoas_educacao$ncpf)]

#vincula as tabelas com as pessoas
pessoas_fisica$pcode <- 
  pessoas$pcode[match(pessoas_fisica$fcode, pessoas$fcode)]
pessoas_imobiliario$pcode <- 
  pessoas$pcode[match(pessoas_imobiliario$icode, pessoas$icode)]
pessoas_assistencia$pcode <- 
  pessoas$pcode[match(pessoas_assistencia$acode, pessoas$acode)]
pessoas_saude$pcode <- 
  pessoas$pcode[match(pessoas_saude$scode, pessoas$scode)]
pessoas_educacao$pcode <- 
  pessoas$pcode[match(pessoas_educacao$ecode, pessoas$ecode)]

# Ordena as pessoas pelos nomes
pessoas <- pessoas[order(pessoas$nome),]

saveRDS(pessoas,
        file = "manipulacao/dados/pessoas.RDS")
saveRDS(pessoas_saude,
        file = "manipulacao/dados/pessoas_saude.RDS")
saveRDS(pessoas_educacao,
        file = "manipulacao/dados/pessoas_educacao.RDS")
saveRDS(pessoas_fisica,
        file = "manipulacao/dados/pessoas_fisica.RDS")
saveRDS(pessoas_assistencia,
        file = "manipulacao/dados/pessoas_assistencia.RDS")
saveRDS(pessoas_imobiliario,
        file = "manipulacao/dados/pessoas_imobiliario.RDS")

# Separa as informações pessoais

# Ajusta as datas de atualização da saúde e assistência
pessoas_saude$dataAtualizacao[pessoas_saude$dataAtualizacao |> is.na()] <- 
  pessoas_saude$dataInclusao[pessoas_saude$dataAtualizacao |> is.na()]
pessoas_assistencia$dataAtualizacao[pessoas_assistencia$dataAtualizacao |> is.na()] <- 
  pessoas_assistencia$dataInclusao[pessoas_assistencia$dataAtualizacao |> is.na()]

# Padroniza o formato das datas
pessoas_fisica$dataAtualizacao <- pessoas_fisica$dataAtualizacao |> as.Date("%d/%m/%Y")
pessoas_educacao$dataAtualizacao <- pessoas_educacao$dataAtualizacao |> as.Date("%d/%m/%Y")
pessoas_saude$dataAtualizacao <- pessoas_saude$dataAtualizacao |> as.Date("%d/%m/%Y")
pessoas_assistencia$dataAtualizacao <- pessoas_assistencia$dataAtualizacao |> as.Date("%d/%m/%Y")

# nomes
nome <- NULL
nome <- nome |> junta_nome(pessoas_fisica, "dt_fisica")
nome <- nome |> junta_nome(pessoas_educacao, "dt_educacao")
nome <- nome |> junta_nome(pessoas_saude, "dt_saude")
nome <- nome |> junta_nome(pessoas_assistencia, "dt_assistencia")
nome$campo <- "Nome"
# Só é necessario guardar nomes duplicados
pcodes_duplicados <- nome$pcode[nome$pcode |> duplicated()] |> unique()
nome <- nome[nome$pcode %in% pcodes_duplicados,]

# e-mails
email <- NULL
email <- email |> junta_email(pessoas_fisica, "dt_fisica", "Email")
email <- email |> junta_email(pessoas_educacao, "dt_educacao")
email <- email |> junta_email(pessoas_saude, "dt_saude")
email <- email |> junta_email(pessoas_assistencia, "dt_assistencia")
email$campo <- "E-mail"

# telefones
telefone <- NULL
telefone <- telefone |> junta_telefone(pessoas_fisica, "dt_fisica", "Telefone")
telefone <- telefone |> junta_telefone(pessoas_educacao, "dt_educacao")
telefone <- telefone |> junta_telefone(pessoas_saude, "dt_saude")
telefone <- telefone |> junta_telefone(pessoas_assistencia, "dt_assistencia")
telefone$campo <- "Telefone"

# maes
mae <- NULL
mae <- mae |> junta_mae(pessoas_saude,"dt_saude")
mae$dt_fisica <- NA
mae$dt_educacao <- NA
mae <- mae[,c(1,2,4,5,3)]
mae <- mae |> junta_mae(pessoas_assistencia,"dt_assistencia")
mae$campo <- "Mãe"

# nascimento
nascimento <- NULL
nascimento <- nascimento |> junta_nascimento(pessoas_fisica, "dt_fisica")
nascimento$dt_educacao <- NA
nascimento <- nascimento |> junta_nascimento(pessoas_saude, "dt_saude")
nascimento <- nascimento |> junta_nascimento(pessoas_assistencia, "dt_assistencia")
nascimento$campo <- "Data de nascimento"

# identidade
identidade <- NULL
identidade <- identidade |> junta_identidade(pessoas_fisica, "dt_fisica")
identidade <- identidade |> junta_identidade(pessoas_educacao, "dt_educacao")
identidade <- identidade |> junta_identidade(pessoas_saude, "dt_saude")
identidade <- identidade |> junta_identidade(pessoas_assistencia, "dt_assistencia")
identidade$campo <- "Identidade"

# ctps
ctps <- pessoas_assistencia[pessoas_assistencia$pcode |> is.na() |> not() &
                              pessoas_assistencia$ctps |> is.na() |> not(),
                            c("pcode", "ctps", "dataAtualizacao")]
ctps <- ctps[ctps$ctps != "",]
names(ctps) <- c("pcode", "valor", "dt_assistencia")
ctps$dt_fisica <- NA
ctps$dt_educacao <- NA
ctps$dt_saude <- NA
ctps <- ctps[,c(1,2,4,5,6,3)]
ctps$campo <- "CTPS"

# Endereços
endereco <- NULL

pessoas_fisica$LOGRADOURO <- pessoas_fisica$logradouro |> limpa_nome_rua()
pessoas_fisica$endereco <- paste0(pessoas_fisica$tipoLogradouro, " ",
                                  pessoas_fisica$LOGRADOURO,", n ",
                                  pessoas_fisica$numero," ",
                                  pessoas_fisica$complemento,", ",
                                  pessoas_fisica$bairro,", ",
                                  pessoas_fisica$cidade," - ",
                                  pessoas_fisica$UF,", ",
                                  "BRASIL")
pessoas_fisica$endereco[pessoas_fisica$LOGRADOURO |> is.na()] <- NA
endereco <- endereco |> junta_endereco(pessoas_fisica, "dt_fisica")

pessoas_educacao$LOGRADOURO <- pessoas_educacao$logradouro |> limpa_nome_rua()
pessoas_educacao$endereco <- paste0(pessoas_educacao$LOGRADOURO,", n ",
                                    pessoas_educacao$numero," ",
                                    pessoas_educacao$complemento,", ",
                                    pessoas_educacao$bairro,", ",
                                    pessoas_educacao$localidade," - ",
                                    pessoas_educacao$estado,", ",
                                    "BRASIL")
pessoas_educacao$endereco[pessoas_educacao$LOGRADOURO |> is.na()] <- NA
endereco <- endereco |> junta_endereco(pessoas_educacao, "dt_educacao")

pessoas_saude$LOGRADOURO <- pessoas_saude$Logradouro |> limpa_nome_rua()
pessoas_saude$endereco <- paste0(pessoas_saude$tipoLogradouro, " ",
                                  pessoas_saude$LOGRADOURO,", n ",
                                  pessoas_saude$numero," ",
                                  pessoas_saude$complemento,", ",
                                  pessoas_saude$bairro,", ",
                                  pessoas_saude$cidade," - ",
                                  pessoas_saude$UF,", ",
                                  "BRASIL")
pessoas_saude$endereco[pessoas_saude$LOGRADOURO |> is.na()] <- NA
endereco <- endereco |> junta_endereco(pessoas_saude, "dt_saude")

pessoas_assistencia$LOGRADOURO <- pessoas_assistencia$logradouro |> limpa_nome_rua()
pessoas_assistencia$endereco <- paste0(pessoas_assistencia$LOGRADOURO,", n ",
                                 pessoas_assistencia$numero," ",
                                 pessoas_assistencia$complemento,", ",
                                 pessoas_assistencia$bairro,", ",
                                 pessoas_assistencia$cidade," - ",
                                 pessoas_assistencia$uf,", ",
                                 "BRASIL")
pessoas_assistencia$endereco[pessoas_assistencia$LOGRADOURO |> is.na()] <- NA
endereco <- endereco |> junta_endereco(pessoas_assistencia, "dt_assistencia")
endereco$campo <- "Endereço"

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

saveRDS(info_pessoais,
        file = "manipulacao/dados/info_pessoais.RDS")

# Endereços!!
# Precisa unificar entre as pessoas
# E tentar cruzar com os imoveis_c_geo!

# Cruzar nomes:
# Identificar pessoas sem CPF em alguma base (Esse tem q ser o foco)
# Separar pessoas que só estão em 1 base e sem CPF (Talvez)
# Cruzar pessoas q estão em mais de uma base e não possuem CPF (Talvez).


# # Por enquanto é só, pessoal
# # Vamos desenvolver o consulta com essas informações;
# # Depois que tiver com a base pronta, vamos ver o que dá para extrair a mais do
# # restante das informações
# # Separa os nomes únicos que sobraram nas bases
# nomes_unicos <- c(
#   pessoas_assistencia$NOME[!(pessoas_assistencia$acode %in% pessoas$acode)],
#   pessoas_fisica$NOME[!(pessoas_fisica$fcode %in% pessoas$fcode)],
#   pessoas_imobiliario$NOME[!(pessoas_imobiliario$icode %in% pessoas$icode)],
#   pessoas_saude$NOME[!(pessoas_saude$scode %in% pessoas$scode)],
#   pessoas_educacao$NOME[!(pessoas_educacao$ecode %in% pessoas$ecode)]
# )
# 
# nomes_duplicados <- nomes_unicos[duplicated(nomes_unicos)] |> unique()
# nomes_unicos <- nomes_unicos[!(nomes_unicos %in% nomes_duplicados)] 
# nomes_unicos <- nomes_unicos[!(nomes_unicos %in% pessoas$nome)]
# 
# teste <- match(pessoas$cpf, pessoas_assistencia$ncpf)
# min(teste, na.rm = TRUE)
# #pessoa com débito
# 
# p_c_debito <- imoveis |> subset(vlDebitoDA>0)
# 
# pessoas_vinculo |> head()
# p_c_debito <- p_c_debito |> left_join(pessoas_vinculo |> subset(responsavel == TRUE))
# p_c_debito$codPessoa |> unique()
