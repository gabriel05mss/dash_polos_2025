PidadeUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    
    fluidRow(
      box(
        title = h1('Escolaridade por Faixa Etária - População em Situação de Rua no Brasil - Dezembro/2024', align = 'center'), #trocar titulo
        width = 12,
        collapsible = TRUE,
        solidHeader = TRUE,
        fluidRow(
          width = 12,
          column(2, uiOutput(ns("estado_ui"))),
          column(2, offset = 1, uiOutput(ns("meso_ui"))),
          column(2, offset = 2, uiOutput(ns("micro_ui"))),
          column(2, offset = 1,uiOutput(ns("municipio_ui")))
        )
      )
    ),
    
    fluidRow(
      
      box(
        title = h1('Escolaridade por Faixa Etária - População Negra', align = 'center'), #trocar titulo
        width = 6,
        collapsible = TRUE,
        solidHeader = TRUE,
        withSpinner(highchartOutput(ns("plot_1")), type = 1, color = "#ffae00", size = 2)
      ),
      
      box(
        title = h1('Escolaridade por Faixa Etária - População Não Negra', align = 'center'), #trocar titulo
        width = 6,
        collapsible = TRUE,
        solidHeader = TRUE,
        withSpinner(highchartOutput(ns("plot_2")), type = 1, color = "#ffae00", size = 2)
      )
    ),
    
    fluidRow(
      
      box(
        title = h1('titulo que quiser', align = 'center'), #trocar titulo
        width = 6,
        collapsible = TRUE,
        solidHeader = TRUE,
        withSpinner(highchartOutput(ns("plot_3")), type = 1, color = "#ffae00", size = 2)
      ),
      
      box(
        title = h1('titulo que quiser', align = 'center'), #trocar titulo
        width = 6,
        collapsible = TRUE,
        solidHeader = TRUE,
        withSpinner(highchartOutput(ns("plot_4")), type = 1, color = "#ffae00", size = 2)
      )
    ),
    
    fluidRow(
      
      box(
        title = h1('titulo que quiser', align = 'center'), #trocar titulo
        width = 12,
        collapsible = TRUE,
        solidHeader = TRUE,
        withSpinner(dataTableOutput(ns("tabela")), type = 1, color = "#ffae00", size = 2)
      )
    )
  )
}

PidadeServer <- function(input, output, session, dados) {
  ns <- session$ns
  
  # REATIVOS ----
  ##filtros----
  filtros <- reactiveValues(
    estado = NULL,
    meso = NULL,
    micro = NULL,
    municipio = NULL
  )
  ##dados ----
  df <- reactiveValues(
    filtrado = NULL
  )
  
  # UI DINÂMICA ----
  ## estado ----
  output$estado_ui <- renderUI({
    req(dados)
    selectizeInput(ns("Estado"), "Estado",
                   choices = c("TODOS", sort(unique(dados$estado))),
                   selected = "TODOS", multiple = TRUE, options = list(maxOptions = 10000))
  })
  
  ##mesoregiao ----
  output$meso_ui <- renderUI({
    req(dados)
    selectizeInput(ns("Mesoregioes"), "Mesoregiões",
                   choices = c("TODAS", sort(unique(dados$mesorregioes))),
                   selected = "TODAS", multiple = TRUE, options = list(maxOptions = 10000))
  })
  
  ##microregiao ----
  output$micro_ui <- renderUI({
    req(dados)
    selectizeInput(ns("Microregioes"), "Microregiões",
                   choices = c("TODAS", sort(unique(dados$microrregioes))),
                   selected = "TODAS", multiple = TRUE, options = list(maxOptions = 10000))
  })
  
  ##municipio ----
  output$municipio_ui <- renderUI({
    req(dados)
    selectizeInput(ns("Municipio"), "Município",
                   choices = c("TODOS", sort(unique(dados$municipio))),
                   selected = "TODOS", multiple = TRUE, options = list(maxOptions = 10000))
  })
  
  # VALIDAÇÕES ----
  ## choices ----
  observeEvent(input$Estado, {
    if (!is.null(input$Estado) && "TODOS" %in% input$Estado && length(input$Estado) > 1) {
      sendSweetAlert(session, "Erro",
                     "A opção 'TODOS' não pode ser combinada com outros estados",
                     type = "warning")
      updateSelectizeInput(session, "Estado", selected = "TODOS")
    }
  })
  
  observeEvent(input$Mesoregioes, {
    if (!is.null(input$Mesoregioes) && "TODAS" %in% input$Mesoregioes && length(input$Mesoregioes) > 1) {
      sendSweetAlert(session, "Erro",
                     "A opção 'TODAS' não pode ser combinada com outras mesoregiões",
                     type = "warning")
      updateSelectizeInput(session, "Mesoregioes", selected = "TODAS")
    }
  })
  
  observeEvent(input$Microregioes, {
    if (!is.null(input$Microregioes) && "TODAS" %in% input$Microregioes && length(input$Microregioes) > 1) {
      sendSweetAlert(session, "Erro",
                     "A opção 'TODAS' não pode ser combinada com outras microregiões",
                     type = "warning")
      updateSelectizeInput(session, "Microregioes", selected = "TODAS")
    }
  })
  
  observeEvent(input$Municipio, {
    if (!is.null(input$Municipio) && "TODOS" %in% input$Municipio && length(input$Municipio) > 1) {
      sendSweetAlert(session, "Erro",
                     "A opção 'TODOS' não pode ser combinada com outros municípios",
                     type = "warning")
      updateSelectizeInput(session, "Municipio", selected = "TODOS")
    }
  })
  
  
  ##filtros ----
  observe({
    req(dados, input$Estado)
    filtros$estado <- if ("TODOS" %in% input$Estado) unique(dados$estado) else input$Estado
  })
  
  observe({
    req(dados, input$Mesoregioes)
    filtros$meso <- if ("TODAS" %in% input$Mesoregioes) unique(dados$mesorregioes) else input$Mesoregioes
  })
  
  observe({
    req(dados, input$Microregioes)
    filtros$micro <- if ("TODAS" %in% input$Microregioes) unique(dados$microrregioes) else input$Microregioes
  })
  
  observe({
    req(dados, input$Municipio)
    filtros$municipio <- if ("TODOS" %in% input$Municipio) unique(dados$municipio) else input$Municipio
  })
  
  #OUTPUT ----
  ##tabela ----
  output$tabela = renderDataTable({
    req(dados)
    dados_aux = dados %>%
      select(estado, uf, everything(), -arquivo_origem)
    
    if (!is_empty(filtros$estado)){
      dados_aux = dados_aux %>%
        filter(estado %in% filtros$estado)
    }
    
    if (!is_empty(filtros$meso)){
      dados_aux = dados_aux %>%
        filter(mesorregioes %in% filtros$meso)
    }
    
    if (!is_empty(filtros$micro)){
      dados_aux = dados_aux %>%
        filter(microrregioes %in% filtros$micro)
    }
    
    if (!is_empty(filtros$municipio)){
      dados_aux = dados_aux %>%
        filter(municipio %in% filtros$municipio)
    }
    
    df$filtrado <- dados_aux
    
    DT::datatable(
      dados_aux,
      rownames = FALSE,
      filter = "none", 
      style = "bootstrap",
      class = "stripe hover cell-border compact",
      options = list(
        dom = 'fltip', 
        pageLength = 10,
        lengthMenu = c(5, 10, 25, 50, 100),
        scrollX = TRUE,
        autoWidth = TRUE,
        searchHighlight = TRUE,
        columnDefs = list(
          list(className = 'dt-center', targets = "_all"),
          list(targets = 0, title = "Estado")
        )
      )
    ) %>%
      DT::formatStyle(
        columns = names(dados_aux),
        fontSize = '14px',
        color = 'black',
        fontWeight = 'normal',
        textAlign = 'center'
      )
  })
  
  #output$plot_1 = renderHighchart({ 
  #req(df$filtrado)
  #continuar a desenvolver graficos, usar df$filtrado
  #})
  
  #replicar codigo acima para todos os graficos
  #lembrar de trocar o id do grafico para nn dar conflito
}
