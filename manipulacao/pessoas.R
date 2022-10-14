# Junta todas as pessoas em um único dataframe
print("Agrupa dados das pessoas...")

library(stringr)

source("manipulacao/library.R")

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
pessoas_assistencia$NOME <- toupper(pessoas_assistencia$nome)
pessoas_educacao$NOME <- toupper(pessoas_educacao$nome)
pessoas_fisica$NOME <- toupper(pessoas_fisica$nome)
pessoas_imobiliario$NOME <- toupper(pessoas_imobiliario$nome)
pessoas_saude$NOME <- toupper(pessoas_saude$Nome)

pessoas_assistencia$NOME <- gsub("[^⁠[:alpha:] ]","",pessoas_assistencia$NOME)
pessoas_educacao$NOME <- gsub("[^⁠[:alpha:] ]","",pessoas_educacao$NOME)
pessoas_fisica$NOME <- gsub("[^⁠[:alpha:] ]","",pessoas_fisica$NOME)
pessoas_imobiliario$NOME <- gsub("[^⁠[:alpha:] ]","",pessoas_imobiliario$NOME)
pessoas_saude$NOME <- gsub("[^⁠[:alpha:] ]","",pessoas_saude$NOME)

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
