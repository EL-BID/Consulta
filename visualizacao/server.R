function(input, output, session)  {
  
  # ## Sistema de autenticação
  # res_auth <- secure_server(
  #   check_credentials = check_credentials(
  #     "dados/database.sqlite",
  #     passphrase = key_get("R-shinymanager-key", "consulta")
  #   )
  # )
  # output$auth_output <- renderPrint({
  #   reactiveValuesToList(res_auth)
  # })
  
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
        maxZoom = 20,
      preferCanvas = TRUE,
        worldCopyJump = FALSE
      )) |>
      onRender(
        "function(el, x){L.control.zoom({ position: 'topright' }).addTo(this)}"        
      ) |>
      # Mapa de fundo
      addProviderTiles(
        providers$CartoDB
      ) |>
      # addPolygons(
      #   data = lotes,
      #   weight = 1,
      #   group = "lotes"
      # ) |>
      # hideGroup("lotes") |>
      # Define zoom inicial
      fitBounds(mybbox[1,1],
                mybbox[2,1],
                mybbox[1,2],
                mybbox[2,2])
  })
  # Controle do nível zoom
  # observeEvent(
  #   input$map_zoom,
  #   if (input$map_zoom |> is.null()) return()
  #   else if (input$map_zoom |> is.na()) return()
  #   else if (input$map_zoom |> is.nan()) return()
  #   else if (input$map_zoom == "") return()
  #   else if (input$mapid_zoom>15)
  #     leafletProxy("map") |> showGroup("lotes")
  #   # else
  #   #   leafletProxy("map") |> hideGroup("lotes")
  # )
  
  # SelectsizeInput pelo lado do servidor por questão de performance
  updateSelectizeInput(
    inputId =  'consulta_pcode',
    selected = "232820",
    choices = pessoas[,c("pcode","cpf","nome")],
    server = TRUE,
    options = list(
      placeholder = "buscar...",
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
  observeEvent(input$consulta_pcode, pcode(input$consulta_pcode))
  pcode <- reactiveVal("")
  observeEvent(input$fechar_nome_cpf, {
    updateSelectizeInput(inputId = "consulta_pcode",
                         selected = "")
    dados_imovel("")
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
    dados_relacoes(NULL)
    dados_infos(NULL)
    newbbox <- NULL
    
    # Encerra ações se nenhum munícipe foi selecionado
    if (pcode == "") return()

    ## mostra tabela de informações pessoais
    dados_infos <- 
      info_pessoais[info_pessoais$pcode == pcode,
      c("campo","valor","dt_fisica","dt_educacao","dt_saude","dt_assistencia")]
    dados_infos(dados_infos)
    
    # Plota relações
    newbbox$imoveis <- 
      plotar_relacoes(imoveis_c_geo,
                      "blue",
                      label = c("inscricao", "relacao"),
                      id = "inscricao",
                      mapa = proxy, pcode = pcode)
    newbbox$educacao <- 
      plotar_relacoes(educacao_c_geo,
                      "orange",
                      label = "unidadeReferencia",
                      mapa = proxy, pcode = pcode)
    newbbox$saude <- 
      plotar_relacoes(saude_c_geo,
                      "green",
                      label = "UnidadeReferencia",
                      mapa = proxy, pcode = pcode)
    newbbox$assistencia <- 
      plotar_relacoes(assistencia_c_geo,
                      "yellow",
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
    ponto <- input$map_marker_click
    if (ponto$id |> is.null() |> not()) {
      dados_imovel(ponto$id)
    }
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
          if (FALSE) {
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
                width = "30%",
                style="text-align: center; font-weight: bold;",
                "IPTU"),
              tags$td(
                width = "30%",
                style="text-align: center; font-weight: bold;",
                "Dívida"),
              tags$td(
                width = "40%",
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
  
  # tabela de relações
  output$tabela_infos <- renderDT({
    if (dados_infos() |> is.null())
      "Aguarde..." |> as.data.frame()
    else
      dados_infos()
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
  ## Fim tabela de relações
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # Tabela ####
  # sistema para abrir e fechar tabela de dados
  mostrar_tabela <- reactiveVal(-1)
  output$mostrar_tabela <- reactive(mostrar_tabela())
  outputOptions(output,"mostrar_tabela", suspendWhenHidden = FALSE)
  observeEvent(input$fechar_tabela,mostrar_tabela(-1))
  
    
  # Variável para exibir painel "carregando..."
  output$carregando <- renderText("")
  outputOptions(output, 'carregando', suspendWhenHidden=FALSE)

}

