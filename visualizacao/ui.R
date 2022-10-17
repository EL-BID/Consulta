navbarPage(
  id = "consulta",
  theme = shinytheme("simplex"),
  collapsible = TRUE,
  windowTitle = "PMV - Consulta",
  title = "Consulta",

  header = tagList(
    # Painel "carregando.." (para ocultar conteúdo enquanto carrega)
    conditionalPanel(
      "output.carregando != ''",
      absolutePanel(
        top = 41,
        left = 0,
        right = 0,
        bottom = 0,
        style = "
          background-color: #FAFAFA;
          text-align: center;
          z-index: 100000;
        ",
        img(
          src = "spinner.gif",
          style = "
            position: fixed;
            top: calc(50vh - 5px);
            left: calc(50vw - 8px);
          "
        )
      )
    )
  ),
    
  tabPanel(
    "Munícipe",
    tags$style(type = "text/css", "#map {z-index: 1; background: #FAFAFA}"),
    tags$style(type = "text/css", ".container-fluid {padding-left:0px;padding-right:0px;}"),
    tags$style(type = "text/css", ".navbar {margin-bottom: 0px;}"),
    tags$style(type = "text/css", ".container-fluid .navbar-header .navbar-brand {margin-left: 0px;}"),
    
    # Painel de consulta
    absolutePanel(
      top = 46,
      left = 10,
      height = 65,
      width = 400,
      style = "z-index: 5000;
      font-size: 14px;
      padding: 15px 10px 0px 10px;
      background-color: rgba(0,0,0,0.1);"
    ),
    conditionalPanel(
      "output.mostrar_consulta == -1",
      absolutePanel(
        id = "inputs_panel",
        top = 46,
        left = 10,
        width = 400,
        style = "z-index: 5000;
      font-size: 14px;
      padding: 15px 10px 0px 10px;
      background-color: rgba(0,0,0,0);",
        selectizeInput(
          "consulta_pcode",
          label = NULL,
          width = 400,
          choices = NULL,
          # )
        ),
        textOutput("debug") # Debug area
      )
    ),
    # Painel de nome e cpf
    conditionalPanel(
      "output.mostrar_consulta != -1",
      absolutePanel(
        top = 46,
        left = 10,
        width = 400,
        height = 68,
        style = "z-index: 5000;
      font-size: 14px;
      color: black;
      padding: 13px 10px 0px 10px;
      background-color: rgba(0,0,0,0);",
        "Nome: ",
        textOutput("nome", inline = TRUE) |> tags$b(),
        tags$p("CPF: ",
               textOutput("cpf", inline = TRUE) |> tags$b()),
        actionLink(
          "fechar_nome_cpf",
          label = NULL,
          style = "vertical-align: text-top;font-size: 14px;color: black;",
          icon = icon("times", verify_fa = FALSE)
        ) |> absolutePanel(top = 0, right = 5)
      )
    ),
    
    # Mapa
    leafletOutput("map", width = "100%", height = "calc(100vh - 41px)"),
    
    # Tabela de informações pessoas
    conditionalPanel(
      "output.mostrar_tabela_infos != -1",
      absolutePanel(
        width = 730,
        # height = 200,
        top = 46,
        left = 420,
        draggable = TRUE,
        style = "
          z-index: 4000;
          background-color: #fff;
          border-bottom-left-radius: 4px;
          border-bottom-right-radius: 4px;
          border-top-left-radius: 4px;
          border-top-right-radius: 4px;
          border: 2px solid rgba(0,0,0,0.2);
        ",
        DTOutput("tabela_infos") |> div(style = "padding: 0px 0px !important;"),
        actionLink(
          "fechar_tabela_infos",
          label = NULL,
          style = "
                    vertical-align: text-top;
                    font-size: 14px;
                    color: black;
                  ",
          icon = icon("times", verify_fa = FALSE)
        ) |> absolutePanel(top = 0, right = 5)
      )
    ),
    
    conditionalPanel(
      "output.mostrar_consulta != -1",
      conditionalPanel(
        "output.mostrar_tabela_infos == 1",
        absolutePanel(
          top = 51,
          style = btn_pressionado
        )
      ),
      absolutePanel(
        top = 51,
        style = btn_normal
      ),
      absolutePanel(
        top = 51,
        style = btn_rotulo,
        actionLink(
          "btn_tabela_infos",
          label = NULL,
          icon("id-card-o")
        )
      )),
    
    
    # Tabela de relações sem georreferenciamento
    conditionalPanel(
      "output.mostrar_tabela_relacoes != -1",
      absolutePanel(
        width = 280,
        # height = 200,
        top = 125,
        left = 10,
        draggable = TRUE,
        style = "
          z-index: 4000;
          background-color: #fff;
          border-bottom-left-radius: 4px;
          border-bottom-right-radius: 4px;
          border-top-left-radius: 4px;
          border-top-right-radius: 4px;
          border: 2px solid rgba(0,0,0,0.2);
        ",
        DTOutput("tabela_relacoes"),
        actionLink(
          "fechar_tabela_relacoes",
          label = NULL,
          style = "
                    vertical-align: text-top;
                    font-size: 14px;
                    color: black;
                  ",
          icon = icon("times", verify_fa = FALSE)
        ) |> absolutePanel(top = 0, right = 5)
      )
    ),
    
    
    # Tabela de informações do imóvel
    conditionalPanel(
      "output.mostrar_tabela_imovel != -1",
      absolutePanel(
        width = 400,
        # height = 200,
        top = 125,
        left = 300,
        draggable = TRUE,
        style = "
          z-index: 4000;
          background-color: #fff;
          border-bottom-left-radius: 4px;
          border-bottom-right-radius: 4px;
          border-top-left-radius: 4px;
          border-top-right-radius: 4px;
          border: 2px solid rgba(0,0,0,0.2);
        ",
        tags$p(
          style = "
              font-size: 14px;
              font-weight: bold;
              text-align: center;
              padding: 5px;
              margin: 0px; !important;
              color: black;",
          "Informações do imóvel"),
        tags$hr(style = "margin-top: 0;margin-bottom: 0;"
        ),
        uiOutput("tabela_imovel"),
        actionLink(
          "fechar_tabela_imovel",
          label = NULL,
          style = "
                    vertical-align: text-top;
                    font-size: 14px;
                    color: black;
                  ",
          icon = icon("times", verify_fa = FALSE)
        ) |> absolutePanel(top = 0, right = 5)
      )
    )
  ),

  tabPanel(
    "Confrontantes",
    
    # Mapa  
    tags$style(type = "text/css", "#map_confrontantes {z-index: 1; background: #FAFAFA}"),
    leafletOutput("map_confrontantes", width = "100%", height = "calc(100vh - 41px)"),
    
    # Pesquisa endereços
    absolutePanel(
      top = 46,
      left = 10,
      height = 65,
      width = 400,
      style = "z-index: 5000;
      font-size: 14px;
      padding: 15px 10px 0px 10px;
      background-color: rgba(0,0,0,0.1);",
      selectizeInput(
        "pesquisa_endereco",
        label = NULL,
        width = 400,
        choices = NULL,
      )
    ),

    # Tabela de lista de relacionados
    conditionalPanel(
      "output.mostrar_lista_relacionados != -1",
      absolutePanel(
        width = 650,
        # height = 200,
        top = 53,
        left = 10,
        draggable = TRUE,
        style = "
          z-index: 4000;
          background-color: #fff;
          border-bottom-left-radius: 4px;
          border-bottom-right-radius: 4px;
          border-top-left-radius: 4px;
          border-top-right-radius: 4px;
          border: 2px solid rgba(0,0,0,0.2);
        ",
        DTOutput("lista_relacionados"),
        actionLink(
          "fechar_lista_relacionados",
          label = NULL,
          style = "
                    vertical-align: text-top;
                    font-size: 14px;
                    color: black;
                  ",
          icon = icon("times", verify_fa = FALSE)
        ) |> absolutePanel(top = 0, right = 5)
      )
    )
  ),  
  
  tabPanel(
    "Download",
    style = "padding: 10px;",
    # Input: Select a file
    fileInput(
      "arquivo_cpf", 
      "Escolha arquivo .txt com a lista de CPFs desejados",
      width = 400,
      buttonLabel = "Procurar...",
      placeholder = "Escolha um arquivo.",
      multiple = FALSE,
      accept = ".txt"),
    
    # tableOutput("contents"),
    
    tags$hr(),
    
    "Informações desejadas:",
    checkboxInput("info_telefone", "Telefone", FALSE),
    checkboxInput("info_email", "E-mail", FALSE),
    checkboxInput("info_endereco", "Endereço", FALSE),
    checkboxInput("info_inscricao", "Inscrição Cadastral", FALSE),
    
    downloadButton("download"),
    
  )  
  # ) |> secure_app(enable_admin = TRUE, language = "pt-BR", theme = "simplex", tags_top = "Consulta")
)