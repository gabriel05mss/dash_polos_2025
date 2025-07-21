deficienteUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    
    fluidRow(
      box(
        title = h1('Escolaridade por Pessoa Possui Deficiência - População em Situação de Rua no Brasil - Dezembro/2024', align = 'center'), #trocar titulo
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
        title = h1('Escolaridade por Pessoa Possui Deficiência - População Negra', align = 'center'), #trocar tituloo
        width = 6,
        collapsible = TRUE,
        solidHeader = TRUE,
        withSpinner(highchartOutput(ns("plot_1")), type = 1, color = "#ffae00", size = 2)
      ),
      
      box(
        title = h1('Escolaridade por Pessoa Possui Deficiência - População Não Negra', align = 'center'), #trocar titulo
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

deficienteServer <- function(input, output, session, dados) {
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

# DADOS PREPARADOS (reativo único com estrutura tidy)
dados_plot_raca <- reactive({
  req(df$filtrado) #usar dados filtrados
  dadosdf1 <- df$filtrado %>%
    select(
      municipio, mesorregioes, microrregioes, estado, uf,
      total_pop_em_situacao_de_rua,
      masculino, feminino,
      sem_instrucao, fundamental_incompleto, fundamental_completo,
      medio_incompleto, medio_completo, superior_incompleto_ou_mais,
      sem_resposta_18,
      branca, preta, amarela, parda, indigena, sem_resposta_30,
      sim_162, nao_164
    ) %>%
    mutate(across(
      c(masculino, feminino, total_pop_em_situacao_de_rua,
        sem_instrucao, fundamental_incompleto, fundamental_completo,
        medio_incompleto, medio_completo, superior_incompleto_ou_mais,
        sem_resposta_18, branca, preta, amarela, parda, indigena,
        sem_resposta_30, sim_162, nao_164),
      ~ as.numeric(.)
    )) %>%
    mutate(
      prop_df_sim = ifelse(total_pop_em_situacao_de_rua > 0, sim_162 / total_pop_em_situacao_de_rua, 0),
      prop_df_nao = ifelse(total_pop_em_situacao_de_rua > 0, nao_164 / total_pop_em_situacao_de_rua, 0),
      pop_negra = preta + parda,
      pop_nao_negra = branca + amarela + indigena + sem_resposta_30,
      prop_negra = ifelse(total_pop_em_situacao_de_rua > 0, pop_negra / total_pop_em_situacao_de_rua, 0),
      prop_nao_negra = ifelse(total_pop_em_situacao_de_rua > 0, pop_nao_negra / total_pop_em_situacao_de_rua, 0)
    ) %>%
    pivot_longer(
      cols = c(
        sem_instrucao, fundamental_incompleto, fundamental_completo,
        medio_incompleto, medio_completo, superior_incompleto_ou_mais,
        sem_resposta_18
      ),
      names_to = "GrauInstrucao",
      values_to = "PopulacaoGrau"
    ) %>%
    mutate(
      PopulacaoGrau = as.numeric(PopulacaoGrau),
      pop_df_sim_negra      = PopulacaoGrau * prop_df_sim * prop_negra,
      pop_df_nao_negra      = PopulacaoGrau * prop_df_nao * prop_negra,
      pop_df_sim_nao_negra  = PopulacaoGrau * prop_df_sim * prop_nao_negra,
      pop_df_nao_nao_negra  = PopulacaoGrau * prop_df_nao * prop_nao_negra
    )
  
  # População Negra
  dados_df_negra <- dadosdf1 %>%
    select(municipio, mesorregioes, microrregioes, estado, uf, GrauInstrucao,
           pop_df_sim_negra, pop_df_nao_negra) %>%
    pivot_longer(
      cols = c(pop_df_sim_negra, pop_df_nao_negra),
      names_to = "Pessoa_possui_deficiencia", values_to = "Populacao"
    ) %>%
    mutate(
      Possui_deficiencia = ifelse(Pessoa_possui_deficiencia == "pop_df_sim_negra", "Sim", "Não"),
      GrupoRacial = "Negra"
    )
  
  # População Não Negra
  dados_df_nao_negra <- dadosdf1 %>%
    select(municipio, mesorregioes, microrregioes, estado, uf, GrauInstrucao,
           pop_df_sim_nao_negra, pop_df_nao_nao_negra) %>%
    pivot_longer(
      cols = c(pop_df_sim_nao_negra, pop_df_nao_nao_negra),
      names_to = "Pessoa_possui_deficiencia", values_to = "Populacao"
    ) %>%
    mutate(
      Possui_deficiencia = ifelse(Pessoa_possui_deficiencia == "pop_df_sim_nao_negra", "Sim", "Não"),
      GrupoRacial = "Não negra"
    )
  
  grau_nomes <- c(
    "sem_instrucao" = "Sem instrução",
    "fundamental_incompleto" = "Fund. incompleto",
    "fundamental_completo" = "Fund. completo",
    "medio_incompleto" = "Médio incompleto",
    "medio_completo" = "Médio completo",
    "superior_incompleto_ou_mais" = "Superior ou +",
    "sem_resposta_18" = "Sem resposta"
  )
  
  bind_rows(dados_df_negra, dados_df_nao_negra) %>%
    mutate(GrauInstrucao = recode(GrauInstrucao, !!!grau_nomes))
})



plot_grafico_deficiencia <- function(dados, grupo_racial, titulo) {
  
  ordem_graus <- c(
    "Sem instrução",
    "Fund. incompleto",
    "Fund. completo",
    "Médio incompleto",
    "Médio completo",
    "Superior ou +",
    "Sem resposta"
  )
  
  dados_grafico <- dados() %>%
    filter(GrupoRacial == grupo_racial) %>%
    group_by(GrauInstrucao, Possui_deficiencia) %>%
    summarise(Populacao = sum(Populacao, na.rm = TRUE), .groups = "drop") %>%
    mutate(GrauInstrucao = factor(GrauInstrucao, levels = ordem_graus)) %>%
    arrange(GrauInstrucao)
  
  categorias <- levels(dados_grafico$GrauInstrucao)
  
  serie_sim <- dados_grafico %>%
    filter(Possui_deficiencia == "Sim") %>%
    pull(Populacao)
  
  serie_nao <- dados_grafico %>%
    filter(Possui_deficiencia == "Não") %>%
    pull(Populacao)
  
  highchart() %>%
    hc_chart(type = "column") %>%
    hc_title(text = titulo) %>%
    hc_xAxis(categories = categorias, title = list(text = "Grau de Instrução")) %>%
    hc_yAxis(title = list(text = "População estimada"), labels = list(format = "{value:,.0f}")) %>%
    hc_plotOptions(column = list(grouping = TRUE)) %>%
    hc_add_series(name = "Sim", data = serie_sim, color = "#1f77b4") %>%
    hc_add_series(name = "Não", data = serie_nao, color = "#ff7f0e") %>%
    hc_legend(enabled = TRUE)
}

output$plot_1 <- renderHighchart({

  plot_grafico_deficiencia(
    dados = dados_plot_raca,
    grupo_racial = "Negra",
    titulo = "População Negra - Pessoa com Deficiência x Grau de Instrução"
  )
})

output$plot_2 <- renderHighchart({

  plot_grafico_deficiencia(
    dados = dados_plot_raca,
    grupo_racial = "Não negra",
    titulo = "População Não Negra - Pessoa com Deficiência x Grau de Instrução"
  )
})
}



