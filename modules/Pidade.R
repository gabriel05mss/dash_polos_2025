PidadeUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    
    fluidRow(
      box(
        title = h1('Escolaridade por Faixa Etária - População em Situação de Rua no Brasil - Dezembro/2024', align = 'center'), #trocar titulo
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
        title = h1('Escolaridade por Faixa Etária - População Negra', align = 'center'), #trocar tituloo
        width = 6,
        collapsible = TRUE,
        solidHeader = TRUE,
        withSpinner(highchartOutput(ns("plot_1")), type = 1, color = "#ffae00", size = 2)
      ),
      
      box(
        title = h1('Escolaridade por Faixa Etária - População Não Negra', align = 'center'), #trocar titulo
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

PidadeServer <- function(input, output, session, dados) {
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
  # Reativo com dados organizados por faixa etária
  dados_plot_faixa_etaria <- reactive({
    req(df$filtrado)
    colunas_faixa <- c(
      "entre_0_e_4", "entre_5_a_6", "entre_7_a_15", "entre_16_a_17", 
      "entre_18_a_24", "entre_25_a_34", "entre_35_a_39", "entre_40_a_44",
      "entre_45_a_49", "entre_50_a_54", "entre_55_a_59", "entre_60_a_64",
      "maior_que_65", "sem_resposta_64", "sem_resposta_65"
    )
    
    dados_base <- df$filtrado %>%
      select(
        municipio, mesorregioes, microrregioes, estado, uf,
        total_pop_em_situacao_de_rua,
        branca, preta, amarela, parda, indigena, sem_resposta_30,
        sim_162, nao_164,
        all_of(colunas_faixa)
      ) %>%
      mutate(across(
        c(total_pop_em_situacao_de_rua, sim_162, nao_164,
          branca, preta, amarela, parda, indigena, sem_resposta_30,
          all_of(colunas_faixa)),
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
        cols = all_of(colunas_faixa),
        names_to = "FaixaEtaria",
        values_to = "PopulacaoFaixa"
      ) %>%
      mutate(
        PopulacaoFaixa = as.numeric(PopulacaoFaixa),
        pop_df_sim_negra      = PopulacaoFaixa * prop_df_sim * prop_negra,
        pop_df_nao_negra      = PopulacaoFaixa * prop_df_nao * prop_negra,
        pop_df_sim_nao_negra  = PopulacaoFaixa * prop_df_sim * prop_nao_negra,
        pop_df_nao_nao_negra  = PopulacaoFaixa * prop_df_nao * prop_nao_negra
      )
    
    # Base negra
    dados_negra <- dados_base %>%
      select(municipio, mesorregioes, microrregioes, estado, uf, FaixaEtaria,
             pop_df_sim_negra, pop_df_nao_negra) %>%
      pivot_longer(
        cols = c(pop_df_sim_negra, pop_df_nao_negra),
        names_to = "Pessoa_possui_deficiencia", values_to = "Populacao"
      ) %>%
      mutate(
        Possui_deficiencia = ifelse(Pessoa_possui_deficiencia == "pop_df_sim_negra", "Sim", "Não"),
        GrupoRacial = "Negra"
      )
    
    # Base não negra
    dados_nao_negra <- dados_base %>%
      select(municipio, mesorregioes, microrregioes, estado, uf, FaixaEtaria,
             pop_df_sim_nao_negra, pop_df_nao_nao_negra) %>%
      pivot_longer(
        cols = c(pop_df_sim_nao_negra, pop_df_nao_nao_negra),
        names_to = "Pessoa_possui_deficiencia", values_to = "Populacao"
      ) %>%
      mutate(
        Possui_deficiencia = ifelse(Pessoa_possui_deficiencia == "pop_df_sim_nao_negra", "Sim", "Não"),
        GrupoRacial = "Não negra"
      )
    
    bind_rows(dados_negra, dados_nao_negra)
  })
  plot_grafico_faixa <- function(dados, grupo_racial, titulo) {
    
    # Ordem das faixas etárias
    ordem_faixas <- c(
      "entre_0_e_4", "entre_5_a_6", "entre_7_a_15", "entre_16_a_17", 
      "entre_18_a_24", "entre_25_a_34", "entre_35_a_39", "entre_40_a_44",
      "entre_45_a_49", "entre_50_a_54", "entre_55_a_59", "entre_60_a_64",
      "maior_que_65", "sem_resposta_64", "sem_resposta_65"
    )
    
    nomes_faixa <- c(
      "entre_0_e_4" = "0 a 4", "entre_5_a_6" = "5 a 6", "entre_7_a_15" = "7 a 15",
      "entre_16_a_17" = "16 a 17", "entre_18_a_24" = "18 a 24",
      "entre_25_a_34" = "25 a 34", "entre_35_a_39" = "35 a 39",
      "entre_40_a_44" = "40 a 44", "entre_45_a_49" = "45 a 49",
      "entre_50_a_54" = "50 a 54", "entre_55_a_59" = "55 a 59",
      "entre_60_a_64" = "60 a 64", "maior_que_65" = "65 ou +",
      "sem_resposta_64" = "SR até 64", "sem_resposta_65" = "SR 65+"
    )
    
    dados_grafico <- dados() %>%
      filter(GrupoRacial == grupo_racial) %>%
      group_by(FaixaEtaria, Possui_deficiencia) %>%
      summarise(Populacao = sum(Populacao, na.rm = TRUE), .groups = "drop") %>%
      mutate(
        FaixaEtaria = factor(FaixaEtaria, levels = ordem_faixas),
        FaixaEtaria = recode(FaixaEtaria, !!!nomes_faixa)
      ) %>%
      arrange(FaixaEtaria)
    
    categorias <- levels(dados_grafico$FaixaEtaria)
    
    serie_sim <- dados_grafico %>%
      filter(Possui_deficiencia == "Sim") %>%
      pull(Populacao)
    
    serie_nao <- dados_grafico %>%
      filter(Possui_deficiencia == "Não") %>%
      pull(Populacao)
    
    highchart() %>%
      hc_chart(type = "column") %>%
      hc_title(text = titulo) %>%
      hc_xAxis(categories = categorias, title = list(text = "Faixa Etária")) %>%
      hc_yAxis(title = list(text = "População estimada"), labels = list(format = "{value:,.0f}")) %>%
      hc_plotOptions(column = list(grouping = TRUE)) %>%
      hc_add_series(name = "Sim", data = serie_sim, color = "#1f77b4") %>%
      hc_add_series(name = "Não", data = serie_nao, color = "#ff7f0e") %>%
      hc_legend(enabled = TRUE)
  }
  output$plot_1 <- renderHighchart({
    plot_grafico_faixa(
      dados = dados_plot_faixa_etaria,
      grupo_racial = "Negra",
      titulo = "População Negra - Pessoa com Deficiência x Faixa Etária"
    )
  })
  
  output$plot_2 <- renderHighchart({
    plot_grafico_faixa(
      dados = dados_plot_faixa_etaria,
      grupo_racial = "Não negra",
      titulo = "População Não Negra - Pessoa com Deficiência x Faixa Etária"
    )
  })
}
