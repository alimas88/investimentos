library(shiny)
library(purrr)

myapp <- function(...){

  ui <- fluidPage(title = 'Investimentos',
    fileInput('dados_brutos', 'Selecione os arquivos de negociacao', accept = '.xlsx', multiple = TRUE),
    DT::DTOutput('negociacao')
  )

  server <- function(input, output, session) {

    dados <- reactive({

      req(input$dados_brutos)
      df <- map(input$dados_brutos$datapath, .f = carrega_dados) %>%
        bind_rows() %>%
        arrange(data_negocio) %>%
        mutate(data_negocio = format(as.Date(data_negocio), "%d/%m/%Y")) %>%
        mutate(prazo_vencimento = format(as.Date(prazo_vencimento), "%d/%m/%Y"))

    })

    output$negociacao <- DT::renderDT({

      dados()

    })

  }
  shinyApp(ui, server)
}

