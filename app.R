library(shiny)
library(bs4Dash)
library(shinyjs)
library(shinyWidgets)
library(shinyalert)
library(shinycssloaders)
library(dplyr)
library(readxl)
library(janitor)
library(stringr)


# Carregar módulos
source("modules/Psexo.R")
source("modules/page2.R")
source("modules/page3.R")
source("modules/page4.R")

ui <- fluidPage(
  useShinyjs(),
  useShinyalert(force = TRUE),
  uiOutput("main_ui")
)

server <- function(input, output, session) {
  user_logged <- reactiveVal(FALSE)
  

  #carregar dados 
dados <- read_excel("dados/dados_edu.xlsx", col_types = "text") %>% 
    clean_names()
  
dados <- dados %>%
  mutate(across(
    .cols = where(~ all(grepl("^[0-9\\.,%]*$", .), na.rm = TRUE)), 
    .fns  = ~ {
      x <- .x
      tem_pct <- str_detect(x, fixed("%"))
      num <- as.numeric(str_replace_all(x, c("%" = "", "," = ".")))
      ifelse(tem_pct, num/100, num)
    }
  ))
  
   
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
        tags$img(src = "img/logo.png", height = "200px"),
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
            bs4SidebarMenuItem("Página 1", tabName = "Psexo", icon = icon("person")),
            bs4SidebarMenuItem("Página 2", tabName = "page2", icon = icon("table")),
            bs4SidebarMenuItem("Página 3", tabName = "page3", icon = icon("cogs")),
            bs4SidebarMenuItem("Página 4", tabName = "page4", icon = icon("file-alt"))
          )
        ),
        
        body = bs4DashBody(
          bs4TabItems(
            bs4TabItem(tabName = "Psexo", PsexoUI("Psexo")),
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
  
  callModule(PsexoServer, "Psexo", dados = dados)
  callModule(page2Server, "page2", dados = dados)
  callModule(page3Server, "page3", dados = dados)
  callModule(page4Server, "page4", dados = dados)
}

shinyApp(ui, server)
