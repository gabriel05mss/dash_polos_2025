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

# Carregar módulos
source("modules/Psexo.R")
source("modules/Pidade.R")
source("modules/deficiente.R")
source("modules/equipe.R")

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
        
        header = bs4DashNavbar(title = "Categorias"), #trocar depois
        
        sidebar = bs4DashSidebar(
          collapsed = FALSE,
          skin = "light",
          status = "primary",
          title = "Menu",
          brandColor = "primary",
          bs4SidebarMenu(
            bs4SidebarMenuItem("Sexo", tabName = "Psexo", icon = icon("person")), #trocar depois
            bs4SidebarMenuItem("Faixa etária", tabName = "Pidade", icon = icon("calendar")), #trocar depois
            bs4SidebarMenuItem("Pessoa possui Deficiência", tabName = "deficiente", icon = icon("wheelchair")), #trocar depois
            bs4SidebarMenuItem("Equipe", tabName = "equipe", icon = icon("address-card")) #trocas depois
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
          left = "Fonte: Levantamento realizado pelo Observatório Brasileiro de Políticas Públicas com a População em Situação de
Rua/POLOS-UFMG a partir da consulta ao CECAD (Mês de Referência: Dezembro/2024) ", #trocar depois
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
