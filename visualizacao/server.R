function(input, output, session)  {
  
  ## Sistema de autenticação
  res_auth <- secure_server(
    check_credentials = check_credentials(
      "dados/database.sqlite",
      passphrase = key_get("R-shinymanager-key", "consulta")
    )
  )
  output$auth_output <- renderPrint({
    reactiveValuesToList(res_auth)
  })
  
  # Mapa base ####
  output$map <- renderLeaflet({
    leaflet(
      options = leafletOptions(
        zoomControl = FALSE,
      #   zoomSnap = 0.1,
      #   zoomDelta = 0.1,
        boxZoom = TRUE,
        doubleClickZoom = FALSE,
        # minZoom = 10,
        # maxZoom = 20,
        worldCopyJump = FALSE
      #   preferCanvas = TRUE
      )) |>
      onRender(
        "function(el, x){L.control.zoom({ position: 'topright' }).addTo(this)}"        
      ) |>
      # Mapa de fundo
      addProviderTiles(
        providers$CartoDB.PositronNoLabels,
        group = "Mapa"
      ) |>
      # Rótulos
      addProviderTiles(
        providers$CartoDB.PositronOnlyLabels,
        group = "Rótulos"
      ) |>
      addLayersControl(
        overlayGroups = c(
          "Mapa",
          "Rótulos"
        ),
        options = layersControlOptions(
          collapsed = FALSE
        )
      ) |>
      # Define zoom inicial
      fitBounds(mybbox[1,1],
                mybbox[2,1],
                mybbox[1,2],
                mybbox[2,2])
  })
  
  # Efeitos da seleção de um munícipe
  observeEvent(input$consulta_pcode,{
    pcode <- input$consulta_pcode
    # mostra tabela de informações pessoais
    if(pcode != ""){
      mostrar_tabela(1)
    }
    
    # seleciona variáveis georreferenciadas
    imoveis <- imoveis_c_geo[
      (imoveis_c_geo@data$pcode == pcode) |> which(),]
    educacao <- educacao_c_geo[
      (educacao_c_geo@data$pcode == pcode) |> which(),]
    saude <- saude_c_geo[
      (saude_c_geo@data$pcode == pcode) |> which(),]
    assistencia <- assistencia_c_geo[
      (assistencia_c_geo@data$pcode == pcode) |> which(),]

    # define novo bbox
    if (pcode == "" | 
        (imoveis@data[1,1] |> is.na() &
         educacao@data[1,1] |> is.na() &
         saude@data[1,1] |> is.na() &
         assistencia@data[1,1] |> is.na())) {
      long_max <- mybbox[1,2]
      long_min <- mybbox[1,1]
      lat_max <- mybbox[2,2]
      lat_min <- mybbox[2,1]
    } else {
      long_min <- min(
        ifelse(imoveis@data[1,1] |> is.na(), NA, imoveis@bbox[1,1]),
        ifelse(educacao@data[1,1] |> is.na(), NA, educacao@bbox[1,1]),
        ifelse(saude@data[1,1] |> is.na(), NA, saude@bbox[1,1]),
        ifelse(assistencia@data[1,1] |> is.na(), NA, assistencia@bbox[1,1]),
        na.rm = TRUE
      )
      
      long_max <- max(
        ifelse(imoveis@data[1,1] |> is.na(), NA, imoveis@bbox[1,2]),
        ifelse(educacao@data[1,1] |> is.na(), NA, educacao@bbox[1,2]),
        ifelse(saude@data[1,1] |> is.na(), NA, saude@bbox[1,2]),
        ifelse(assistencia@data[1,1] |> is.na(), NA, assistencia@bbox[1,2]),
        na.rm = TRUE
      )
      
      lat_min <- min(
        ifelse(imoveis@data[1,1] |> is.na(), NA, imoveis@bbox[2,1]),
        ifelse(educacao@data[1,1] |> is.na(), NA, educacao@bbox[2,1]),
        ifelse(saude@data[1,1] |> is.na(), NA, saude@bbox[2,1]),
        ifelse(assistencia@data[1,1] |> is.na(), NA, assistencia@bbox[2,1]),
        na.rm = TRUE
      )
      
      lat_max <- max(
        ifelse(imoveis@data[1,1] |> is.na(), NA, imoveis@bbox[2,2]),
        ifelse(educacao@data[1,1] |> is.na(), NA, educacao@bbox[2,2]),
        ifelse(saude@data[1,1] |> is.na(), NA, saude@bbox[2,2]),
        ifelse(assistencia@data[1,1] |> is.na(), NA, assistencia@bbox[2,2]),
        na.rm = TRUE
      )
    }
    
    # plota relações
    proxy <- leafletProxy("map")
    proxy   |>
      clearGroup("municipe") |>
      addCircleMarkers(
        data = imoveis,
        color = "blue",
        # label = long_max |> as.character(),
        group = "municipe"
      ) |> 
      addCircleMarkers(
        data = educacao,
        color = "orange",
        # label = imoveis@data$relacao |>list(), ??? Pq não está funcionando?
        group = "municipe"
      ) |> 
      addCircleMarkers(
        data = saude,
        color = "green",
        # label = imoveis@data$relacao |>list(), ??? Pq não está funcionando?
        group = "municipe"
      ) |> 
      addCircleMarkers(
        data = assistencia,
        color = "yellow",
        # label = imoveis@data$relacao |>list(), ??? Pq não está funcionando?
        group = "municipe"
      ) |>
      fitBounds(long_min,
                lat_min,
                long_max,
                lat_max
                )
    
    # Seleciona informações não georreferenciadas
    # seleciona variáveis georreferenciadas
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
      # Se não houver informações, esconde a tabela
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
          lapply(function(i) paste0("Imóvel: ",i)) |>
          as.data.frame() |>
          t()
        dados_relacoes <-
          rbind(dados_relacoes, lista_imoveis)
      }
      names(dados_relacoes) <- "Relações sem georreferenciamento"
      dados_relacoes(dados_relacoes)
    }
      
  },  ignoreInit = FALSE)


  # SelectsizeInput pelo lado do servidor por questão de performance
  updateSelectizeInput(
    inputId =  'consulta_pcode',
    choices = pessoas[,c("pcode","cpf","nome")],
    server = TRUE
  )

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
    info = input$tabela_relacoes_cell_clicked
    if (info$value |> is.null() |> not()) {
      if (grep("Imóvel: ",info$value) != 0) {
        # Se clicou em um imóvel, apresenta a tabela de informações do imóvel
        dados_imovel(sub("Imóvel: ","", info$value))
        dados_imovel(sub("Imóvel: ","", info$value))
        mostrar_tabela_imovel(1)
      }
    }
  })
  ## Fim tabela de relações
  
  
  ### Tabela de informações do imóvel ####
  # sistema para abrir e fechar tabela de dados
  mostrar_tabela_imovel <- reactiveVal(-1)
  output$mostrar_tabela_imovel <- reactive(mostrar_tabela_imovel())
  outputOptions(output,"mostrar_tabela_imovel", suspendWhenHidden = FALSE)
  observeEvent(input$fechar_tabela_imovel, mostrar_tabela_imovel(-1))
  observeEvent(dados_imovel(), 
               if (dados_imovel() |> is.null() |> not())
                 mostrar_tabela_imovel(1))
  
  # dados
  dados_imovel <- reactiveVal(NULL)
  
  # tabela do imóvel
  output$tabela_imovel <- renderText({
    # dados_imovel()
    paste("Outro teste: ",dados_imovel())
  }
  # },
  # 
  # server = F,
  # rownames = FALSE,
  # selection = "none",
  # options = list(
  #   ordering = FALSE,
  #   searching = FALSE,
  #   paging = FALSE,
  #   info = FALSE,
  #   lengthChange = FALSE)
  )
  ## Fim tabela do imóvel
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # Tabela ####
  # sistema para abrir e fechar tabela de dados
  mostrar_tabela <- reactiveVal(-1)
  output$mostrar_tabela <- reactive(mostrar_tabela())
  outputOptions(output,"mostrar_tabela", suspendWhenHidden = FALSE)
  observeEvent(input$fechar_tabela,mostrar_tabela(-1))
  observeEvent(input$btn_tabela,mostrar_tabela(mostrar_tabela()*-1))
  
    
  # Variável para exibir painel "carregando..."
  output$carregando <- renderText("")
  outputOptions(output, 'carregando', suspendWhenHidden=FALSE)

}

