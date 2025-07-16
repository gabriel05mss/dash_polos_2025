page3UI <- function(id) {
  ns <- NS(id)
  fluidPage(
    h2("Página 3"),
    textOutput(ns("msg"))
  )
}

page3Server <- function(input, output, session, dados) {
  output$msg <- renderText("Conteúdo da Página 3")
}
