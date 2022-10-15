navbarPage(
  theme = shinytheme("simplex"),
  collapsible = TRUE,
  windowTitle = "PMV - Perfil da Dívida Ativa (IPTU)",
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
    conditionalPanel(
      "input.consulta_pcode == ''",
      absolutePanel(
        id = "inputs_panel",
        top = 46,
        left = 10,
        width = 400,
        style = "z-index: 5000;
      font-size: 14px;
      padding: 15px 10px 0px 10px;
      background-color: rgba(0,0,0,0.1);",
        selectizeInput(
          "consulta_pcode",
          label = NULL,
          width = 400,
          choices = NULL,
        )
      )
    ),
    # Painel de nome e cpf
    conditionalPanel(
      "input.consulta_pcode != ''",
      absolutePanel(
        id = "inputs_panel",
        top = 46,
        left = 10,
        width = 400,
        height = 68,
        style = "z-index: 5000;
      font-size: 14px;
      color: black;
      padding: 13px 10px 0px 10px;
      background-color: rgba(0,0,0,0.1);",
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
        DTOutput("tabela_infos"),
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
    ),
    
    
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
    ),

  ),

  tabPanel(
    "Confrontantes"
  ),
  
  tabPanel(
    "Download"
  )
# ) |> secure_app(enable_admin = TRUE, language = "pt-BR", theme = "simplex", tags_top = "Consulta")
)