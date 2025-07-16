page2UI <- function(id) {
  ns <- NS(id)
  fluidPage(
    bs4Card(
      title = "Página 2",
      status = "info",
      solidHeader = TRUE,
      width = 12,
      withSpinner(DT::dataTableOutput(ns("tbl")), type = 6)
    )
  )
}

page2Server <- function(input, output, session, dados) {
  output$tbl <- DT::renderDataTable({
    mtcars %>% head(10)
  })
}
