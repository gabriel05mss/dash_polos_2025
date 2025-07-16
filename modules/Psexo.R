library(shiny)
library(dplyr)
library(shinyWidgets)

PsexoUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    box(
      title = h1('Análises por sexo', align = 'center'),
      width = 12,
      collapsible = TRUE,
      solidHeader = TRUE,
      fluidRow(
        width = 12,
        column(2, uiOutput(ns("estado_ui"))),
        column(2, uiOutput(ns("meso_ui"))),
        column(2, uiOutput(ns("micro_ui"))),
        column(2, uiOutput(ns("municipio_ui")))
      )
    )
  )
}

PsexoServer <- function(input, output, session, dados) {
  ns <- session$ns
  
  # UI DINÂMICA — usa selectizeInput com server = TRUE
  output$estado_ui <- renderUI({
    req(dados)
    selectizeInput(ns("Estado"), "Estado",
                   choices = c("TODOS", sort(unique(dados$estado))),
                   selected = "TODOS", multiple = TRUE, options = list(maxOptions = 10000))
  })
  
  output$meso_ui <- renderUI({
    req(dados)
    selectizeInput(ns("Mesoregioes"), "Mesoregiões",
                   choices = c("TODAS", sort(unique(dados$mesorregioes))),
                   selected = "TODAS", multiple = TRUE, options = list(maxOptions = 10000))
  })
  
  output$micro_ui <- renderUI({
    req(dados)
    selectizeInput(ns("Microregioes"), "Microregiões",
                   choices = c("TODAS", sort(unique(dados$microrregioes))),
                   selected = "TODAS", multiple = TRUE, options = list(maxOptions = 10000))
  })
  
  output$municipio_ui <- renderUI({
    req(dados)
    selectizeInput(ns("Municipio"), "Município",
                   choices = c("TODOS", sort(unique(dados$municipio))),
                   selected = "TODOS", multiple = TRUE, options = list(maxOptions = 10000))
  })
  
  # VALIDAÇÕES
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
  
  # REATIVOS DE FILTRO
  filtros <- reactiveValues(
    estado = NULL,
    meso = NULL,
    micro = NULL,
    municipio = NULL
  )
  
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
  
  return(filtros)
}
