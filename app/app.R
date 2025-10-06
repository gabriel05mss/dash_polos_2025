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
library(highcharter)
library(purrr)
library(DT)
library(tidyverse)
library(scales)
library(tidyr)

# Carregar módulos
source("modules/Psexo.R")
source("modules/Pidade.R")
source("modules/deficiente.R")
source("modules/equipe.R")

# ui <- fluidPage(
#   useShinyjs(),
#   useShinyalert(force = TRUE),
#   tags$style(HTML("
#   /* Sidebar */
#   .main-sidebar, 
#   .main-sidebar .nav-link, 
#   .main-sidebar .nav-link .nav-icon, 
#   .main-sidebar .nav-link p {
#     font-size: 15px !important;
#   }
# 
#   /* Título da sidebar */
#   [class*=sidebar-light] .brand-link {
#     font-size: 22px !important;
#     font-weight: bold;
#     text-align: center;
#     justify-content: center !important;  /* flexbox centraliza horizontalmente */
#     display: flex !important;
#     align-items: center;
#   }
# ")),
#   uiOutput("main_ui")
# )
# 
# server <- function(input, output, session) {
#   user_logged <- reactiveVal(FALSE)
#   
# 
#   #carregar dados 
# dados <- read_excel("dados/dados_edu.xlsx", col_types = "text") %>% 
#     clean_names()
#   
# dados <- dados %>%
#   mutate(across(
#     .cols = where(~ all(grepl("^[0-9\\.,%]*$", .), na.rm = TRUE)), 
#     .fns  = ~ {
#       x <- .x
#       tem_pct <- str_detect(x, fixed("%"))
#       num <- as.numeric(str_replace_all(x, c("%" = "", "," = ".")))
#       ifelse(tem_pct, num/100, num)
#     }
#   ))
#   
#    
#   output$main_ui <- renderUI({
#     if (!user_logged()) {
#       fluidPage(
#         tags$head(tags$style(HTML("
#           body {
#             background-color: #f8f9fa;
#             text-align: center;
#             padding-top: 100px;
#           }
#         "))),
#         tags$img(src = "img/logo.png", height = "200px"),
#         br(), br(),
#         actionBttn("login_btn", "Entrar", style = "gradient", color = "success", size = "lg")
#       )
#     } else {
#       bs4DashPage(
#         fullscreen = TRUE,
#         controlbar = NULL,
#         
#         header = bs4DashNavbar(title = "Categorias"), #trocar depois
#         
#         sidebar = bs4DashSidebar(
#           collapsed = FALSE,
#           skin = "light",
#           status = "primary",
#           title = "Menu",
#           brandColor = "primary",
#           bs4SidebarMenu(
#             bs4SidebarMenuItem("Sexo", tabName = "Psexo", icon = icon("person")), #trocar depois
#             bs4SidebarMenuItem("Faixa etária", tabName = "Pidade", icon = icon("calendar")), #trocar depois
#             bs4SidebarMenuItem("Pessoa possui Deficiência", tabName = "deficiente", icon = icon("wheelchair")), #trocar depois
#             bs4SidebarMenuItem("Equipe", tabName = "equipe", icon = icon("address-card")) #trocas depois
#           )
#         ),
#         
#         body = bs4DashBody(
#           bs4TabItems(
#             bs4TabItem(tabName = "Psexo", PsexoUI("Psexo")),
#             bs4TabItem(tabName = "Pidade", PidadeUI("Pidade")),
#             bs4TabItem(tabName = "deficiente", deficienteUI("deficiente")),
#             bs4TabItem(tabName = "equipe", equipeUI("equipe"))
#           )
#         ),
#         
#         footer = bs4DashFooter(
#           left = "Fonte: Levantamento realizado pelo Observatório Brasileiro de Políticas Públicas com a População em Situação de
# Rua/POLOS-UFMG a partir da consulta ao CECAD (Mês de Referência: Dezembro/2024) ", #trocar depois
#           right = Sys.Date()
#         )
#       )
#     }
#   })
#   
#   observeEvent(input$login_btn, {
#     shinyalert("Login", "Você acessou o sistema!", type = "success")
#     user_logged(TRUE)
#   })
#   
#   callModule(PsexoServer, "Psexo", dados = dados)
#   callModule(PidadeServer, "Pidade", dados = dados)
#   callModule(deficienteServer, "deficiente", dados = dados)
#   callModule(equipeServer, "equipe", dados = dados)
# }
# 
# shinyApp(ui, server)
ui <- fluidPage(
  useShinyjs(),
  useShinyalert(force = TRUE),
  tags$head(
    tags$style(HTML("
      /* ... (seu código CSS para sidebar) ... */
      
      /* CSS para tela de login - AJUSTADO */
      .login-page {
        background-image: url('capa.jpeg');
        background-size: contain; /* Mudei de 'cover' para 'contain' */
        background-repeat: no-repeat;
        background-attachment: fixed;
        background-position: center center;
        background-color: #f8f9fa; /* Cor de fundo caso a imagem não preencha */
        height: 100vh;
        margin: 0;
        padding: 0;
        display: flex;
        justify-content: flex-end;
        align-items: flex-end;
      }
      
      #login_container {
        position: absolute;
        bottom: 30px; /* Reduzi a distância */
        right: 30px;  /* Reduzi a distância */
        padding: 15px;
        background-color: rgba(255, 255, 255, 0.1);
        border-radius: 10px;
      }
      
      #login_img_btn {
        cursor: pointer;
        transition: transform 0.2s;
        height: 100px !important; /* Diminuí o tamanho do botão */
      }
    "))
  ),
  uiOutput("main_ui")
)

server <- function(input, output, session) {
  user_logged <- reactiveVal(FALSE)
  
  # Carregar dados 
  dados <- read_excel("dados/dados_edu.xlsx", col_types = "text") %>% 
    clean_names()
  # No server, após carregar os dados
  observe({
    cat("Arquivos na pasta www:\n")
    print(list.files("www"))
  })
  
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
  # No server, antes do output$main_ui
  addResourcePath("www", "www")
  
  # E então use nos src:
  src = "www/capa.jpeg"
  # e
  src = "www/Acesse-Aqui.jpeg"
  output$main_ui <- renderUI({
    if (!user_logged()) {
      # Tela de Login
      div(
        class = "login-page",
        div(
          id = "login_container",
          actionLink(
            inputId = "login_btn",
            label = tags$img(
              src = "Acesse-Aqui.jpeg",
              alt = "Entrar no Sistema",
              id = "login_img_btn",
              height = "80px"
            )
          )
        )
      )
    } else {
      # Interface principal após login
      bs4DashPage(
        fullscreen = TRUE,
        controlbar = NULL,
        
        header = bs4DashNavbar(title = "Categorias"),
        
        sidebar = bs4DashSidebar(
          collapsed = FALSE,
          skin = "light",
          status = "primary",
          title = "Menu",
          brandColor = "primary",
          bs4SidebarMenu(
            bs4SidebarMenuItem("Sexo", tabName = "Psexo", icon = icon("person")),
            bs4SidebarMenuItem("Faixa etária", tabName = "Pidade", icon = icon("calendar")),
            bs4SidebarMenuItem("Pessoa possui Deficiência", tabName = "deficiente", icon = icon("wheelchair")),
            bs4SidebarMenuItem("Equipe", tabName = "equipe", icon = icon("address-card"))
          )
        ),
        
        body = bs4DashBody(
          bs4TabItems(
            bs4TabItem(tabName = "Psexo", PsexoUI("Psexo")),
            bs4TabItem(tabName = "Pidade", PidadeUI("Pidade")),
            bs4TabItem(tabName = "deficiente", deficienteUI("deficiente")),
            bs4TabItem(tabName = "equipe", equipeUI("equipe"))
          )
        ),
        
        footer = bs4DashFooter(
          left = "Fonte: Levantamento realizado pelo Observatório Brasileiro de Políticas Públicas com a População em Situação de Rua/POLOS-UFMG a partir da consulta ao CECAD (Mês de Referência: Dezembro/2024)",
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
  callModule(PidadeServer, "Pidade", dados = dados)
  callModule(deficienteServer, "deficiente", dados = dados)
  callModule(equipeServer, "equipe", dados = dados)
}

shinyApp(ui, server)