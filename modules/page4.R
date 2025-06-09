page4UI <- function(id) {
  ns <- NS(id)
  fluidPage(
    h2("Página 4"),
    textOutput(ns("msg"))
  )
}

page4Server <- function(input, output, session) {
  output$msg <- renderText("Conteúdo da Página 4")
}
