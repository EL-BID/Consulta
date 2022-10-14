navbarPage(
  theme = shinytheme("simplex"),
  collapsible = TRUE,
  windowTitle = "PMV - Perfil da Dívida Ativa (IPTU)",
  title = "Consulta",

  # header = tagList(
  #   # Painel "carregando.." (para ocultar conteúdo enquanto carrega)
  #   conditionalPanel(
  #     "output.carregando != ''",
  #     absolutePanel(
  #       top = 41,
  #       left = 0,
  #       right = 0,
  #       bottom = 0,
  #       style = "
  #         background-color: #FAFAFA;
  #         text-align: center;
  #         z-index: 100000;
  #       ",
  #       img(
  #         src = "spinner.gif",
  #         style = "
  #           position: fixed;
  #           top: calc(50vh - 5px);
  #           left: calc(50vw - 8px);
  #         "
  #       )
  #     )
  #   )
  # ),
    
  tabPanel(
    "Munícipe",
    tags$style(type = "text/css", "#map {z-index: 1; background: #FAFAFA}"),
    tags$style(type = "text/css", ".container-fluid {padding-left:0px;padding-right:0px;}"),
    tags$style(type = "text/css", ".navbar {margin-bottom: 0px;}"),
    tags$style(type = "text/css", ".container-fluid .navbar-header .navbar-brand {margin-left: 0px;}"),

    # Painel de controle
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
        "consulta_cpf",
        label = NULL,
        width = 400,
        choices = NULL,
        selected = NULL,
        options = list(
          placeholder = "buscar...",
          onInitialize = I('function() { this.setValue(""); }'),
          searchConjunction = 'and',
          searchField = c('cpf', 'nome'),
          labelField= 'nome',
          sortField = 'nome',
          valueField = "pcode",
          render = I("{option: function(item, escape) {
            return '<div style = \"white-space: nowrap; overflow: hidden;\">' + escape(item.cpf) + ' | ' + escape(item.nome) +'</div>';
          }}")
        )
      )
    ),
    
    # Mapa
    leafletOutput("map", width = "100%", height = "calc(100vh - 41px)"),


    # Tabela de dados
    conditionalPanel(
      "output.mostrar_tabela != -1",
      absolutePanel(
        width = 400,
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
        tags$table(
          width = "100%",
          style = "
              color: black;
            ",
          tags$tr(
            tags$td(
              style = "
                  text-align: center;
                  color: #000;
                  font-size: 18px;
                ",
              colspan = "2",
              "Informações",
              actionLink(
                "fechar_tabela",
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
          tags$tr(tags$td(colspan = 2,
                          tags$hr(style = "
                            margin-top: 0;
                            margin-bottom: 0;"))),
        dataTableOutput("tabelaInformacoes"),
        )
      )
    ),

    conditionalPanel(
      "output.mostrar_tabela == 1",
      absolutePanel(
        top = 425,
        style = btn_pressionado
      )
    ),
    absolutePanel(
      top = 425,
      style = btn_normal
    ),
    absolutePanel(
      top = 425,
      style = btn_rotulo,
      actionLink(
        "btn_tabela",
        label = NULL,
        icon("id-card-o")
      )
    )
    
    
  #   # Tabela de relações não georreferenciadas
  #   conditionalPanel(
  #     "output.mostrar_tabela_relacoes != -1",
  #     absolutePanel(
  #       width = 400,
  #       # height = 200,
  #       top = 125,
  #       left = 10,
  #       draggable = TRUE,
  #       style = "
  #         z-index: 4000;
  #         background-color: #fff;
  #         border-bottom-left-radius: 4px;
  #         border-bottom-right-radius: 4px;
  #         border-top-left-radius: 4px;
  #         border-top-right-radius: 4px;
  #         border: 2px solid rgba(0,0,0,0.2);
  #       ",
  #       tags$table(
  #         width = "100%",
  #         style = "
  #             color: black;
  #           ",
  #         tags$tr(
  #           tags$td(
  #             style = "
  #                 text-align: center;
  #                 color: #000;
  #                 font-size: 14px;
  #               ",
  #             colspan = "2",
  #             "Relações não georreferenciadas",
  #             actionLink(
  #               "fechar_tabela_relacoes",
  #               label = NULL,
  #               style = "
  #                   vertical-align: text-top;
  #                   font-size: 14px;
  #                   color: black;
  #                 ",
  #               icon = icon("times", verify_fa = FALSE)
  #             ) |> absolutePanel(top = 0, right = 5)
  #           )
  #         ),
  #         tags$tr(tags$td(colspan = 2,
  #                         tags$hr(style = "
  #                           margin-top: 0;
  #                           margin-bottom: 0;"))),
  #         # uiOutput("tabela_relacoes"),
  #         # tags$tr(tags$td(colspan = 2,
  #                         # uiOutput("tabelaSaude")
  #         # ))
  #       )
  #     )
  #   )
  ),

  tabPanel(
    "Confrontantes"
  ),
  
  tabPanel(
    "Download"
  )
# ) |> secure_app(enable_admin = TRUE, language = "pt-BR", theme = "simplex", tags_top = "Consulta")
)