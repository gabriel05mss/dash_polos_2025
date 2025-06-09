library(shiny)
library(bs4Dash)
library(shinyjs)
library(shinyWidgets)
library(shinyalert)
library(shinycssloaders)

# Carregar módulos
source("modules/page1.R")
source("modules/page2.R")
source("modules/page3.R")
source("modules/page4.R")

ui <- fluidPage(
  useShinyjs(),
  useShinyalert(force = TRUE),
  uiOutput("main_ui")  # Tudo será carregado aqui dinamicamente
)

server <- function(input, output, session) {
  user_logged <- reactiveVal(FALSE)
  
  # UI principal só aparece após login
  output$main_ui <- renderUI({
    if (!user_logged()) {
      fluidPage(
        tags$head(tags$style(HTML("
          body {
            background-color: #f8f9fa;
            text-align: center;
            padding-top: 100px;
          }
        "))),
        tags$img(src = "logo.png", height = "200px"),
        br(), br(),
        actionBttn("login_btn", "Entrar", style = "gradient", color = "success", size = "lg")
      )
    } else {
      bs4DashPage(
        fullscreen = TRUE,
        controlbar = NULL,
        
        header = bs4DashNavbar(title = "Dashboard Modular"),
        
        sidebar = bs4DashSidebar(
          collapsed = FALSE,
          skin = "light",
          status = "primary",
          title = "Menu",
          brandColor = "primary",
          bs4SidebarMenu(
            bs4SidebarMenuItem("Página 1", tabName = "page1", icon = icon("chart-line")),
            bs4SidebarMenuItem("Página 2", tabName = "page2", icon = icon("table")),
            bs4SidebarMenuItem("Página 3", tabName = "page3", icon = icon("cogs")),
            bs4SidebarMenuItem("Página 4", tabName = "page4", icon = icon("file-alt"))
          )
        ),
        
        body = bs4DashBody(
          bs4TabItems(
            bs4TabItem(tabName = "page1", page1UI("page1")),
            bs4TabItem(tabName = "page2", page2UI("page2")),
            bs4TabItem(tabName = "page3", page3UI("page3")),
            bs4TabItem(tabName = "page4", page4UI("page4"))
          )
        ),
        
        footer = bs4DashFooter(
          left = "Criado com ❤️ em Shiny",
          right = Sys.Date()
        )
      )
    }
  })
  
  observeEvent(input$login_btn, {
    shinyalert("Login", "Você acessou o sistema!", type = "success")
    user_logged(TRUE)
  })
  
  callModule(page1Server, "page1")
  callModule(page2Server, "page2")
  callModule(page3Server, "page3")
  callModule(page4Server, "page4")
}

shinyApp(ui, server)
