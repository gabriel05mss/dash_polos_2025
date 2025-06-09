page1UI <- function(id) {
  ns <- NS(id)
  fluidPage(
    bs4Card(
      title = "Página 1",
      status = "primary",
      solidHeader = TRUE,
      width = 12,
      withSpinner(textOutput(ns("msg")), type = 4, color = "#28a745")
    )
  )
}

page1Server <- function(input, output, session) {
  output$msg <- renderText("Conteúdo interativo da Página 1.")
}
