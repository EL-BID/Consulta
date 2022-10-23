function(input, output, session)  {
  
  ## Sistema de autenticação
  res_auth <- secure_server(
    check_credentials = check_credentials(
      "dados/database.sqlite",
      # passphrase = key_get("R-shinymanager-key", "consulta")
      passphrase = chave
    )
  )
  output$auth_output <- renderPrint({
    reactiveValuesToList(res_auth)
  })
  
  # Debug
  # output$debug <- renderText(
  #   input$map_zoom |> paste0(collapse = " | ")
  # )  
  
  # Mapa base ####
  output$map <- renderLeaflet({
    leaflet(
      options = leafletOptions(
        zoomControl = FALSE,
        boxZoom = TRUE,
        doubleClickZoom = FALSE,
        maxZoom = 25,
        preferCanvas = TRUE,
        worldCopyJump = FALSE
      )) |>
      onRender(
        "function(el, x){L.control.zoom({ position: 'topright' }).addTo(this)}"        
      ) |>
      # Mapa de fundo
      addProviderTiles(
        providers$CartoDB,
        options = providerTileOptions(
          maxZoom = 25
        )
      ) |>
      # Define zoom inicial
      fitBounds(mybbox[1,1],
                mybbox[2,1],
                mybbox[1,2],
                mybbox[2,2])
  })
  
  # SelectsizeInput pelo lado do servidor por questão de performance
  updateSelectizeInput(
    inputId =  'consulta_pcode',
    choices = pessoas[,c("pcode","cpf","nome")],
    server = TRUE,
    options = list(
      placeholder = "Buscar pessoa...",
      onInitialize = I('function() { this.setValue(""); }'),
      searchConjunction = 'and',
      searchField = c('cpf', 'nome'),
      labelField= 'nome',
      # sortField = 'nome',
      valueField = "pcode",
      render = I("{option: function(item, escape) {
            return '<div style = \"white-space: nowrap; overflow: hidden;\">' + escape(item.cpf) + ' | ' + escape(item.nome) +'</div>';
          }}")
    )
  )
  
  # Controle da pessoa selecionada
  observeEvent(input$consulta_pcode, {
    pcode(input$consulta_pcode)
  })
  
  pcode <- reactiveVal("")
  
  mostrar_consulta <- reactiveVal(-1)
  output$mostrar_consulta <- reactive(mostrar_consulta())
  outputOptions(output,"mostrar_consulta", suspendWhenHidden = FALSE)
  observeEvent(input$fechar_nome_cpf, {
    updateSelectizeInput(inputId = "consulta_pcode",
                         selected = "")
    pcode("")
    dados_imovel("")
    mostrar_consulta(-1)
  })
  
  observeEvent(pcode(),{
    req(pcode())
    if (pcode() == "")
      mostrar_consulta(-1)
    else
      mostrar_consulta(1)
    })
  
  
  output$nome <- renderText(
    if (pcode()=="")
      "Aguarde..."
    else
      pessoas$nome[pessoas$pcode == pcode()]
  )
  output$cpf <- renderText(
    if (pcode()=="")
      "Aguarde..."
    else {
      cpf <- pessoas$cpf[pessoas$pcode == pcode()]
      paste0(
        cpf |> substr(1,3),
        ".",
        cpf |> substr(4,6),
        ".",
        cpf |> substr(7,9),
        "-",
        cpf |> substr(10,11))
    }
  )
  
  # Efeitos da seleção de um munícipe
  observeEvent(pcode(),{
    pcode <- pcode()
    
    # Elimina as informações do munícipe anterior
    proxy <- leafletProxy("map")
    proxy   |>
      clearGroup("municipe")|>
      fitBounds(mybbox[1,1],
                mybbox[2,1],
                mybbox[1,2],
                mybbox[2,2])
    mostrar_tabela_relacoes(-1)
    mostrar_tabela_infos(-1)
    dados_relacoes(NULL)
    dados_infos(NULL)
    newbbox <- NULL
    
    # Encerra ações se nenhum munícipe foi selecionado
    if (pcode == "") {
      return()
    }

    ## mostra tabela de informações pessoais
    dados_infos <- 
      info_pessoais[info_pessoais$pcode == pcode,
                    c("campo","valor","dt_fisica","dt_educacao","dt_saude","dt_assistencia")]
    dados_infos(dados_infos)
    
    # Plota relações
    newbbox$imoveis <- 
      plotar_relacoes(imoveis_c_geo[!imoveis_c_geo@data$residente,],
                      "blue",
                      label = c("inscricao", "relacao"),
                      id = "inscricao",
                      mapa = proxy, pcode = pcode)
    newbbox$residencias <- 
      plotar_relacoes(imoveis_c_geo[imoveis_c_geo@data$residente,],
                      "red",
                      label = c("inscricao", "relacao"),
                      id = "inscricao",
                      mapa = proxy, pcode = pcode)
    newbbox$educacao <- 
      plotar_relacoes(educacao_c_geo,
                      "green",
                      label = "unidadeReferencia",
                      mapa = proxy, pcode = pcode)
    newbbox$saude <- 
      plotar_relacoes(saude_c_geo,
                      "orange",
                      label = "UnidadeReferencia",
                      mapa = proxy, pcode = pcode)
    newbbox$assistencia <- 
      plotar_relacoes(assistencia_c_geo,
                      "cyan",
                      label = "unidadeReferencia",
                      mapa = proxy, pcode = pcode)
    
    if (newbbox |> is.null() |> not()) {
      n <- length(newbbox)
      newbbox <- newbbox |> unlist()
      long_min <- newbbox[seq(1+0, (n*4)+0, 4)] |> min() -0.001
      lat_min <- newbbox[seq(1+1, (n*4)+1, 4)] |> min() -0.001
      long_max <- newbbox[seq(1+2, (n*4)+2, 4)] |> max() +0.001
      lat_max <- newbbox[seq(1+3, (n*4)+3, 4)] |> max() +0.001
      proxy |>
        fitBounds(long_min,
                  lat_min,
                  long_max,
                  lat_max)
    }
    
    # Seleciona informações não georreferenciadas
    imoveis <- imoveis_s_geo[
      imoveis_s_geo$pcode == pcode,]
    educacao <- educacao_s_geo[
      educacao_s_geo$pcode == pcode,]
    saude <- saude_s_geo[
      saude_s_geo$pcode == pcode,]
    assistencia <- assistencia_s_geo[
      assistencia_s_geo$pcode == pcode,]
    
    if (pcode == "" |
        (imoveis[1,1] |> is.na() &
         educacao[1,1] |> is.na() &
         saude[1,1] |> is.na() &
         assistencia[1,1] |> is.na())) {
      # Se não houver informações, elimina os dados
      mostrar_tabela_relacoes(-1)
      dados_relacoes(NULL)
    } else {
      # Se houver informações, ativa a tabela de relações
      dados_relacoes <- rbind(
        educacao$unidadeReferencia,
        saude$UnidadeReferencia,
        assistencia$unidadeReferencia
      ) |> as.data.frame()
      if (imoveis[1,1] |> is.na() |> not()) {
        lista_imoveis <- imoveis$inscricao |>
          lapply(\(i) paste0("Imóvel: ",i)) |>
          as.data.frame() |>
          t()
        dados_relacoes <-
          rbind(dados_relacoes, lista_imoveis)
      }
      names(dados_relacoes) <- "Relações sem georreferenciamento"
      dados_relacoes(dados_relacoes)
    }
    
  },  ignoreInit = TRUE)
  
  observeEvent(input$map_marker_click,{
    req(input$map_marker_click)
    ponto <- input$map_marker_click
    dados_imovel(ponto$id)
  })
  
  ### Tabela de relações não georreferenciadas ####
  # sistema para abrir e fechar tabela de dados
  mostrar_tabela_relacoes <- reactiveVal(-1)
  output$mostrar_tabela_relacoes <- reactive(mostrar_tabela_relacoes())
  outputOptions(output,"mostrar_tabela_relacoes", suspendWhenHidden = FALSE)
  observeEvent(input$fechar_tabela_relacoes, mostrar_tabela_relacoes(-1))
  observeEvent(dados_relacoes(), 
               if (dados_relacoes() |> is.null() |> not())
                 mostrar_tabela_relacoes(1))
  
  # dados
  dados_relacoes <- reactiveVal(NULL)
  
  # tabela de relações
  output$tabela_relacoes <- renderDT({
    if (dados_relacoes() |> is.null())
      "Aguarde..." |> as.data.frame()
    else
      dados_relacoes()
  },
  server = F,
  rownames = FALSE,
  selection = "none",
  options = list(
    ordering = FALSE,
    searching = FALSE,
    paging = FALSE,
    info = FALSE,
    lengthChange = FALSE)
  )
  
  # clique na tabela de relações
  observeEvent(input$tabela_relacoes_cell_clicked, {
    req(input$tabela_relacoes_cell_clicked)
    if (input$tabela_relacoes_cell_clicked |> length() == 0) return()
    info = input$tabela_relacoes_cell_clicked
    if (info$value |> is.null() |> not()) {
      if (grep("Imóvel: ",info$value) |> identical(integer(0)) |> not()) {
        # Se clicou em um imóvel, apresenta a tabela de informações do imóvel
        dados_imovel(sub("Imóvel: ","", info$value))
      }
    }
  })
  ## Fim tabela de relações
  
  ### Tabela de informações do imóvel ####
  # sistema para abrir e fechar tabela de dados
  mostrar_tabela_imovel <- reactiveVal(-1)
  output$mostrar_tabela_imovel <- reactive(mostrar_tabela_imovel())
  outputOptions(output,"mostrar_tabela_imovel", suspendWhenHidden = FALSE)
  observeEvent(input$fechar_tabela_imovel, dados_imovel(""))
  observeEvent(dados_imovel(), 
               if (dados_imovel() == "")
                 mostrar_tabela_imovel(-1)
               else
                 mostrar_tabela_imovel(1))
  
  # dados
  dados_imovel <- reactiveVal("")
  
  # tabela do imóvel
  output$tabela_imovel <- renderUI({
    if (dados_imovel() |> length() == 0) return("Erro...")
    if (dados_imovel() == "") return("Aguarde...")
    
    # Seleciona todos os dados
    inscricao <- dados_imovel()
    pcode <- pcode()
    
    imovel <-
      imoveis_c_geo@data[
        imoveis_c_geo@data$inscricao == inscricao,]
    
    if (imovel[1,1] |> is.na())
      # Se não for imóvel georreferenciado, busca sem geo
      imovel <-
      imoveis_s_geo[
        imoveis_s_geo$inscricao == inscricao,]
    
    qtd_pessoas <- imovel[,1] |> length()
    
    imovel <- 
      imovel[imovel$pcode == pcode,]
    
    if (imovel$temDebitoInscricaoExercicio |> is.na() | 
        !imovel$temDebitoInscricaoExercicio) 
      estilo_devedor <- "height: 36px; text-align: center;"
    else
      estilo_devedor <- "height: 36px; text-align: center; color: #FF0000"
    
    # A tabela, propriamente dita
    tags$table(
      width = "100%",
      style = "color: black; !important",
      tags$tr(
        tags$td(
          style="width: 55%; padding: 5px;",
          "Inscrição Cadastral: ",
          imovel$inscricao |> HTML() |> tags$b()
        ),
        tags$td(
          style="text-align: right; width: 40%;padding: 5px; font-weight: bold;",
          if (imovel$relacao |> is.na() |> not()) {
            tagList("( ",imovel$relacao," - ", imovel$condicao |> tolower(),")")
          }
        )),
      tags$tr(
        tags$td(
          style="padding: 5px; text-align: right;",
          colspan="2",
          if (imovel$residente) {
            tags$span(
              class="badge",
              "Morador")
          },
          if (imovel$responsavel) {
            tags$span(
              class="badge bg-success",
              "Responsável")  
          },
          if (imovel$fiscal) {
            tags$span(
              class="badge  bg-warning",
              "Responsabilidade fiscal")  
          }
        )
      ),
      tags$tr(
        tags$td(
          colspan="2",
          tags$hr(style = "margin-top: 0px;margin-bottom: 0px;"
          )
        )
      ),
      tags$tr(
        tags$td(
          style = "padding: 5px;",
          colspan="2",
          "Endereço: " |> tags$b(),
          imovel$endereco)
      ),
      tags$tr(
        tags$td(
          style = "padding: 5px;",
          colspan="2","Ocupação: " |> tags$b(),
          imovel$ocupacao)
      ),
      tags$tr(
        tags$td(
          colspan = "2",
          tags$table(
            style = "text-align: center; margin: 0 auto !important; margin-top: 5px !important;margin-bottom: 5px !important;",
            width = "97%",
            border = "1",
            style = "margin: 5px;",
            tags$tr(
              tags$td(
                width = "33%",
                style="text-align: center; font-weight: bold;",
                "IPTU do Exercício"),
              tags$td(
                width = "33%",
                style="text-align: center; font-weight: bold;",
                "Dívida Ativa"),
              tags$td(
                width = "34%",
                style="text-align: center; font-weight: bold;",
                "Valor Venal")
            ),
            tags$tr(
              style="height: 36px;",
              tags$td(
                style = estilo_devedor,
                "R$ ",
                imovel$vlIPTU|> format(big.mark = ".", decimal.mark = ",", nsmall = 2)),
              tags$td(
                style="height: 36px; text-align: center;",
                "R$ ",
                imovel$vlDebitoDA |> format(big.mark = ".", decimal.mark = ",", nsmall = 2)),
              tags$td(
                style="height: 36px; text-align: center;",
                "R$ ",
                imovel$vlVenalImovel|> format(big.mark = ".", decimal.mark = ",", nsmall = 2))
            ),
          )
        )
      ),
      if (imovel$tipoIsencaoImunidadeIPTU |> is.na() |> not()) {
        tags$tr(
          tags$td(
            style="text-align: center;
          font-weight: bold;
          font-size: 10px;
          vertical-align: text-top;",
            colspan="2",
            "(",imovel$tipoIsencaoImunidadeIPTU,")"
          )
        )
      },
      tags$tr(
        tags$td(
          colspan="2",
          tags$hr(style = "margin-top: 0px;margin-bottom: 0px;"
          )
        )
      ),
      if (qtd_pessoas > 1) {
        tags$tr(
          tags$td(
            colspan = "2",
            DTOutput("tabela_outros_relacionados")
          )
        )
      }
    )
  })
  
  outros_relacionados <- reactiveVal(NULL)
  
  # Tabela que elenca outros munícipes relacionados
  output$tabela_outros_relacionados <- renderDT({
    if (dados_imovel() == "") return("Aguarde..." |> as.data.frame())
    
    # Seleciona todos os dados
    inscricao <- dados_imovel()
    pcode <- pcode()
    
    # Cria dataframe
    dados <- 
      imoveis_c_geo@data[
        imoveis_c_geo@data$inscricao == inscricao, c("pcode","relacao")] |> 
      as.data.frame()
    if (dados[1,1] |> is.na())
      dados <-
      imoveis_s_geo[
        imoveis_s_geo$inscricao == inscricao, c("pcode","relacao")] |> 
      as.data.frame()
    
    dados <- dados[dados$pcode != pcode,]
    if (dados[1,1] |> is.na())
      return(NULL)
    
    dados <- dados |> left_join(pessoas[,c("pcode","cpf","nome")], by = "pcode")
    dados <- dados[order(dados$nome),]
    outros_relacionados(dados[,c("pcode","cpf","nome")])
    dados <- dados[,c("nome","relacao")]
    names(dados) <- c("Outras pessoas relacionadas:", " ")
    
    dados
  },
  server = F,
  rownames = FALSE,
  selection = "none",
  options = list(
    ordering = FALSE,
    searching = FALSE,
    paging = FALSE,
    info = FALSE,
    lengthChange = FALSE)
  )
  
  # Clique na tabela de outros relacionados seleciona outro munícipe
  observeEvent(input$tabela_outros_relacionados_cell_clicked, {
    info <- input$tabela_outros_relacionados_cell_clicked
    dados <- outros_relacionados()
    if (info$row |> is.null()) return()
    pcode(dados[info$row,1])
  })
  ## Fim tabela do imóvel

  ### Tabela de informações pessoais ####
  # sistema para abrir e fechar tabela de dados
  mostrar_tabela_infos <- reactiveVal(-1)
  output$mostrar_tabela_infos <- reactive(mostrar_tabela_infos())
  outputOptions(output,"mostrar_tabela_infos", suspendWhenHidden = FALSE)
  observeEvent(input$btn_tabela_infos,mostrar_tabela_infos(mostrar_tabela_infos()*-1))
  observeEvent(input$fechar_tabela_infos, mostrar_tabela_infos(-1))
  
  # dados
  dados_infos <- reactiveVal(NULL)
  
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
  # tabela de informações
  output$tabela_infos <- renderDT({
    if (dados_infos() |> is.null())
      "Aguarde..." |> as.data.frame()
    else
      datatable(dados_infos(),options = list(
        ordering = FALSE,
        searching = FALSE,
        paging = FALSE,
        info = FALSE,
        lengthChange = FALSE),
        container = info_cabecalho,
        rownames = FALSE,
        selection = "none"
      )
  },
  server = TRUE
  )
  ## Fim tabela de informações pessoais
  
  # Confrontantes ##############
  
  cache_ingles <- reactiveVal("")

  # Mapa base - confrontantes
  output$map_confrontantes <- renderLeaflet({
    cache_ingles <- cache_ingles()
    leaflet(
      options = leafletOptions(
        zoomControl = FALSE,
        boxZoom = TRUE,
        doubleClickZoom = FALSE,
        maxZoom = 25,
        preferCanvas = TRUE,
        worldCopyJump = FALSE
      )) |>
      onRender(
        "function(el, x){L.control.zoom({ position: 'topright' }).addTo(this)}"        
      ) |>
      # Mapa de fundo
      addProviderTiles(
        providers$CartoDB.Positron,
        options = providerTileOptions(
          maxZoom = 25,
          updateWhenZooming = FALSE,
          updateWhenIdle = TRUE           
        )
      ) |>
      addPolygons(
        data = lotes,
        layerId = lotes$Name,
        weight = 1,
        color = "#666",
        fillColor = "red",
        opacity = .5,
        group = "lotes"
      ) |>
      addPolygons(
        data = lotes_sem_dados,
        weight = 1,
        color = "#665",
        fillColor = "grey",
        opacity = .2,
        options = pathOptions(interactive = FALSE),
        group = "lotes"
      ) |>
      # Define zoom inicial
      fitBounds(mybbox[1,1],
                mybbox[2,1],
                mybbox[1,2],
                mybbox[2,2])
   })|> bindCache(cache_ingles())
   #})
  
  outputOptions(output, "map_confrontantes", suspendWhenHidden = FALSE) 

  # SISTEMA DE NAVEGAÇÃO POR ENDEREÇO
  # SelectsizeInput pelo lado do servidor por questão de performance
  updateSelectizeInput(
    inputId =  'pesquisa_endereco',
    choices = lista_enderecos,
    server = TRUE,
    options = list(
      placeholder = "Pesquise endereço...",
      onInitialize = I('function() { this.setValue(""); }'),
      searchConjunction = 'and',
      searchField = c('endereco'),
      labelField= 'endereco',
      # sortField = 'nome',
      valueField = "endereco",
      render = I("{option: function(item, escape) {
            return '<div style = \"white-space: nowrap; overflow: hidden;\">' + escape(item.endereco)  +'</div>';
          }}")
    )
  )
  
  observeEvent(input$pesquisa_endereco,{
    req(input$pesquisa_endereco)
    if (input$pesquisa_endereco |> length() >0){
      selecao <- 
        imoveis_c_geo[
          grep(input$pesquisa_endereco, imoveis_c_geo@data$endereco),]
      novabbox <- selecao@bbox
      leafletProxy("map_confrontantes") |>
        fitBounds(novabbox[1,1] -0.0001,
                  novabbox[2,1] -0.0001,
                  novabbox[1,2] +0.0001,
                  novabbox[2,2] +0.0001)
    }
  })

  # Ao clicar em um lote, mostra a tabela de relacionamentos
  observeEvent(input$map_confrontantes_shape_click,{
    req(input$map_confrontantes_shape_click)
    ponto <- input$map_confrontantes_shape_click
    if (ponto$id |> length() == 0) return()
    if (ponto$id |> is.na()) return()
    lote(ponto$id)
    mostrar_lista_relacionados(1)
  })
  
  ### Tabela de pessoas relacionadas  ####
  # sistema para abrir e fechar tabela de dados
  mostrar_lista_relacionados <- reactiveVal(-1)
  output$mostrar_lista_relacionados <- reactive(mostrar_lista_relacionados())
  outputOptions(output,"mostrar_lista_relacionados", suspendWhenHidden = FALSE)
  observeEvent(input$fechar_lista_relacionados, mostrar_lista_relacionados(-1))

  # dados
  lote <- reactiveVal(NULL)
  
  dados_lista_relacionados <- reactiveVal("")
  observeEvent(lote(), {
    req(lote())
    dados_lista_relacionados(imoveis_p_lote[imoveis_p_lote$Name == lote(),])
  })
  
  # tabela de pessoas relacionadas
  output$lista_relacionados <- renderDT({
    if (lote() |> is.null())
      "Aguarde..." |> as.data.frame()
    else {
      dados <- dados_lista_relacionados()[c("nome", "complemento", "relacao")]
      names(dados) <- c("Nome", "Complemento", "Relação")
      dados
    }
  },
  server = TRUE,
  rownames = FALSE,
  selection = "none",
  options = list(
    ordering = FALSE,
    searching = TRUE,
    language = list(
      search = "<i class='glyphicon glyphicon-search'></i>"
    ),
    paging = TRUE,
    info = FALSE,
    lengthChange = FALSE)
  )
  
  # clique na tabela de relações
  observeEvent(input$lista_relacionados_cell_clicked, {
    req(input$lista_relacionados_cell_clicked)
    req(dados_lista_relacionados())
    if (input$lista_relacionados_cell_clicked |> length() == 0) return()
    if (dados_lista_relacionados() |> is.null()) return()
    if (dados_lista_relacionados() |> length() == 0) return()
    info <- input$lista_relacionados_cell_clicked
    dados <- dados_lista_relacionados()
    pcode(dados[info$row,"pcode"])
    dados_imovel("")
    mostrar_consulta(1)
    updateNavbarPage(
      inputId = "consulta",
      selected = "Munícipe")
  }, ignoreInit = TRUE)
  ## Fim tabela de relações  
  
  
  ### DOWNLOAD ######
  
  dados_para_download <- reactiveVal(NULL)
  
  # Seleção dos dados
  observe({
    req(input$arquivo_cpf)
    
    # Resgata lista de cpfs enviada pelo usuário
    lista_pessoas <- 
      read.table(
        input$arquivo_cpf$datapath, 
        colClasses = c("character"))
    
    # Agrega pcodes
    lista_pessoas <- pessoas[pessoas$cpf %in% lista_pessoas[[1]],c("pcode", "cpf")]
    
    # Lista as informações desejadas
    inscricoes = dados = telefone = email = endereco <- NULL
    if (input$info_telefone) telefone <- "Telefone"
    if (input$info_email) email <- "E-mail"
    if (input$info_endereco) endereco <- "Endereço"
    
    lista_campos <- c(
      telefone,
      email,
      endereco
    )
    
    # Prepara dados das informações pessoais
    if (lista_campos |> is.null() |> not()) {
      dados <- 
        info_pessoais[
          info_pessoais$pcode %in% lista_pessoas$pcode &
            info_pessoais$campo %in% lista_campos, 
          c("pcode", "campo", "valor")]
      
      campos_multiplos <- dados |> 
        group_by(pcode, campo) |>
        summarise(n = n(), .groups = "drop") |>
        filter(n > 1L) 
      
      campos_multiplos <- campos_multiplos$campo |> unique()
      
      dados <- 
        pivot_wider(
          dados, 
          names_from = campo, 
          values_from = valor,
          values_fn = concatenar)
      
      dados <- dados |> cSplit(splitCols = campos_multiplos, sep="|", type.convert=FALSE)
      lista_pessoas <- lista_pessoas |> left_join(dados)
    }
    
    # Prepara os dados das informações cadastrais
    if (input$info_inscricao) {
      inscricoes <- imoveis_c_geo@data[
        imoveis_c_geo@data$pcode %in% lista_pessoas$pcode, 
        c("pcode", "inscricao", "relacao")]
      
      inscricoes <- inscricoes |>
        mutate(
          .keep = "unused",
          inscricao_cadastral = paste0(inscricao, " (", relacao,")")
        )
      inscricoes$campo <- "Inscrição"
      
      inscricoes <- 
        pivot_wider(
          inscricoes,
          names_from = campo,
          values_from = inscricao_cadastral,
          values_fn = concatenar)
      
      inscricoes <- inscricoes |> cSplit(splitCols = 2, sep="|", type.convert=FALSE)
      lista_pessoas <- lista_pessoas |> left_join(inscricoes)
    }
    
    # Atualiza os dados
    lista_pessoas <- lista_pessoas[ , -which(names(lista_pessoas) == "pcode")]
    dados_para_download(lista_pessoas)
  })
  
  # Download
  output$download <- downloadHandler(
    filename = function() {
      "consulta.xlsx"
    },
    content = function(file) {
      write.xlsx(dados_para_download(), file)
    }
  )
  
  ########### FIM ##############
  
  # Variável para exibir painel "carregando..."
  output$carregando <- renderText("")
  outputOptions(output, 'carregando', suspendWhenHidden=FALSE)

}

