library(shiny)
library(purrr)
library(shinydashboard)
library(DT)



  ui <- dashboardPage(

    dashboardHeader(title = 'Investimentos'),

    dashboardSidebar(
      sidebarMenu(
        menuItem('Negociações', tabName = 'negociacao'),
        menuItem('Ações', tabName = 'df_acao')
      )
    ),

    dashboardBody(
      tabItems(
        tabItem(tabName = 'negociacao', 'Negociações registradas na B3',
                fluidRow(
                  box(width = 3,
                      fileInput(inputId = 'dado_bruto', label = 'Selecione os arquivos de Negociação',
                                multiple = TRUE, accept = '.xlsx')
                      ),
                  box(width = 9, title = 'Arquivos Selecionados',
                      textOutput(outputId = 'arquivos'))
                  ),

                fluidRow(
                  box(width = 12,
                    DT::DTOutput(outputId = 'df_dado_bruto')
                  )
                )
                ),
        tabItem(tabName = 'df_acao', 'Negociação de Ações')
        )
      )

  )

  server <- function(input, output, session) {


    dados <- reactive({

      req(input$dado_bruto)
      df <- map(input$dado_bruto$datapath, .f = carrega_dados) %>%
        bind_rows() %>%
        arrange(data_negocio) %>%
        mutate(data_negocio = format(as.Date(data_negocio), "%d/%m/%Y")) %>%
        mutate(prazo_vencimento = format(as.Date(prazo_vencimento), "%d/%m/%Y"))

    })

    output$df_dado_bruto <- DT::renderDT({

      dados()

    })


  }
  shinyApp(ui, server)






