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
        title = h1('Ranking Municípios', align = 'center'), #trocar titulo
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
  output$tabela <- renderDataTable({
    req(dados)
    
    # Seleciona apenas as colunas desejadas, com UF e estado primeiro
    dados_aux <- dados %>%
      select(
        estado, uf, municipio, mesorregioes, microrregioes,
        total_pop_em_situacao_de_rua,
        entre_0_e_4, entre_5_a_6, entre_7_a_15, entre_16_a_17, 
        entre_18_a_24, entre_25_a_34, entre_35_a_39, entre_40_a_44,
        entre_45_a_49, entre_50_a_54, entre_55_a_59, entre_60_a_64,
        maior_que_65,
        sem_instrucao, fundamental_incompleto, fundamental_completo,
        medio_incompleto, medio_completo, superior_incompleto_ou_mais,
        sem_resposta_18,
        branca, preta, amarela, parda, indigena, sem_resposta_30
      )
    
    # Filtros
    if (!is_empty(filtros$estado)) {
      dados_aux <- dados_aux %>% filter(estado %in% filtros$estado)
    }
    if (!is_empty(filtros$meso)) {
      dados_aux <- dados_aux %>% filter(mesorregioes %in% filtros$meso)
    }
    if (!is_empty(filtros$micro)) {
      dados_aux <- dados_aux %>% filter(microrregioes %in% filtros$micro)
    }
    if (!is_empty(filtros$municipio)) {
      dados_aux <- dados_aux %>% filter(municipio %in% filtros$municipio)
    }
    
    # Salva no reativo global
    df$filtrado <- dados_aux
    
    # Ordena por população em situação de rua (decrescente)
    dados_aux <- dados_aux %>%
      arrange(desc(total_pop_em_situacao_de_rua))
    
    # Renomeia as colunas para nomes mais amigáveis
    colnames(dados_aux) <- c(
      "Estado", "UF", "Município", "Mesorregião", "Microrregião",
      "População em Situação de Rua",
      "0 a 4 anos", "5 a 6 anos", "7 a 15 anos", "16 a 17 anos", 
      "18 a 24 anos", "25 a 34 anos", "35 a 39 anos", "40 a 44 anos",
      "45 a 49 anos", "50 a 54 anos", "55 a 59 anos", "60 a 64 anos",
      "65 anos ou mais",
      "Sem instrução", "Fund. incompleto", "Fund. completo",
      "Médio incompleto", "Médio completo", "Superior ou +", "Sem resposta (Escolaridade)",
      "Branca", "Preta", "Amarela", "Parda", "Indígena", "Sem resposta (Raça)"
    )
    
    # Cria a tabela
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
          list(className = 'dt-center', targets = "_all")
        )
      )
    ) %>%
      DT::formatStyle(
        columns = names(dados_aux),
        fontSize = '12px',
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
    dadosfaixaetaria <- df$filtrado %>%
      select(
        municipio,
        mesorregioes,
        microrregioes,
        total_pop_em_situacao_de_rua,
        entre_0_e_4,entre_5_a_6,entre_7_a_15,entre_16_a_17, 
        entre_18_a_24, entre_25_a_34,entre_35_a_39, entre_40_a_44,
        entre_45_a_49,entre_50_a_54, entre_55_a_59, entre_60_a_64,
        maior_que_65,
        sem_instrucao,
        fundamental_incompleto, 
        fundamental_completo,
        medio_incompleto, 
        medio_completo, 
        superior_incompleto_ou_mais,
        sem_resposta_18,
        branca,                                                                             
        preta,                                                                             
        amarela,                                                                         
        parda,                                                                              
        indigena,                                                                         
        sem_resposta_30,
        estado,
        uf
      )
    #---------------------
    # 1. Garantir que todas as colunas necessárias sejam numéricas
    dados_faixa_instrucao <- dadosfaixaetaria %>%
      mutate(across(
        c(
          total_pop_em_situacao_de_rua,
          entre_0_e_4, entre_5_a_6, entre_7_a_15, entre_16_a_17,
          entre_18_a_24, entre_25_a_34, entre_35_a_39,
          entre_40_a_44, entre_45_a_49, entre_50_a_54, entre_55_a_59,
          entre_60_a_64, maior_que_65,
          sem_instrucao, fundamental_incompleto, fundamental_completo,
          medio_incompleto, medio_completo, superior_incompleto_ou_mais,
          sem_resposta_18, branca, preta, amarela, parda, indigena, sem_resposta_30
        ),
        ~as.numeric(gsub(",", ".", .))
      ))
    
    
    # 2. Criar colunas de faixa etária
    dados_faixa_instrucao <- dados_faixa_instrucao %>%
      mutate(
        faixa_0_17 = rowSums(across(c(entre_0_e_4, entre_5_a_6, entre_7_a_15, entre_16_a_17)), na.rm = TRUE),
        faixa_18_39 = rowSums(across(c(entre_18_a_24, entre_25_a_34, entre_35_a_39)), na.rm = TRUE),
        faixa_40_59 = rowSums(across(c(entre_40_a_44, entre_45_a_49, entre_50_a_54, entre_55_a_59)), na.rm = TRUE),
        faixa_60_mais = rowSums(across(c(entre_60_a_64, maior_que_65)), na.rm = TRUE)
      )
    
    # 3. Calcular proporções raciais
    dados_faixa_instrucao <- dados_faixa_instrucao %>%
      mutate(
        pop_negra = preta + parda,
        pop_nao_negra = branca + amarela + indigena + sem_resposta_30,
        prop_negra = ifelse(total_pop_em_situacao_de_rua > 0,
                            pop_negra / total_pop_em_situacao_de_rua, 0),
        prop_nao_negra = ifelse(total_pop_em_situacao_de_rua > 0,
                                pop_nao_negra / total_pop_em_situacao_de_rua, 0)
      )
    
    # 4. Transformar faixas etárias em long
    dados_long_faixa <- dados_faixa_instrucao %>%
      select(municipio, mesorregioes, microrregioes, estado, uf,
             total_pop_em_situacao_de_rua, prop_negra, prop_nao_negra,
             faixa_0_17, faixa_18_39, faixa_40_59, faixa_60_mais) %>%
      pivot_longer(
        cols = starts_with("faixa_"),
        names_to = "FaixaEtariaRaw",
        values_to = "PopulacaoFaixa"
      ) %>%
      mutate(
        FaixaEtaria = recode(FaixaEtariaRaw,
                             "faixa_0_17" = "0 a 17 anos",
                             "faixa_18_39" = "18 a 39 anos",
                             "faixa_40_59" = "40 a 59 anos",
                             "faixa_60_mais" = "60 anos ou mais")
      )
    
    # 5. Transformar graus de instrução em long
    dados_long_instrucao <- dados_faixa_instrucao %>%
      select(municipio, fundamental_incompleto, fundamental_completo,
             medio_incompleto, medio_completo, superior_incompleto_ou_mais,
             sem_instrucao, sem_resposta_18) %>%
      pivot_longer(
        cols = c(sem_instrucao, fundamental_incompleto, fundamental_completo,
                 medio_incompleto, medio_completo, superior_incompleto_ou_mais, sem_resposta_18),
        names_to = "GrauInstrucao",
        values_to = "PopulacaoInstrucao"
      ) %>%
      mutate(GrauInstrucao = recode(GrauInstrucao,
                                    "sem_instrucao" = "Sem instrução",
                                    "fundamental_incompleto" = "Fund. incompleto",
                                    "fundamental_completo" = "Fund. completo",
                                    "medio_incompleto" = "Médio incompleto",
                                    "medio_completo" = "Médio completo",
                                    "superior_incompleto_ou_mais" = "Superior ou +",
                                    "sem_resposta_18" = "Sem resposta"
      ))
    
    # 6. Cruzar FaixaEtaria x GrauInstrucao usando produto cartesiano
    dados_cruzado <- dados_long_faixa %>%
      left_join(dados_long_instrucao, by = "municipio") %>%
      mutate(
        estimativa_total = ifelse(
          is.finite(PopulacaoFaixa) & is.finite(PopulacaoInstrucao) & total_pop_em_situacao_de_rua > 0,
          PopulacaoFaixa * (PopulacaoInstrucao / total_pop_em_situacao_de_rua),
          NA_real_
        ),
        estimativa_negra = ifelse(
          is.finite(estimativa_total) & is.finite(prop_negra),
          estimativa_total * prop_negra,
          NA_real_
        ),
        estimativa_nao_negra = ifelse(
          is.finite(estimativa_total) & is.finite(prop_nao_negra),
          estimativa_total * prop_nao_negra,
          NA_real_
        )
      )
    
    
    # 7. Formatar dados finais tidy
    # População Negra
    dados_negra <- dados_cruzado %>%
      select(municipio, mesorregioes, microrregioes, estado, uf,
             FaixaEtaria, GrauInstrucao, estimativa_negra) %>%
      rename(Populacao = estimativa_negra) %>%
      mutate(GrupoRacial = "Negra")
    
    # População Não Negra
    dados_nao_negra <- dados_cruzado %>%
      select(municipio, mesorregioes, microrregioes, estado, uf,
             FaixaEtaria, GrauInstrucao, estimativa_nao_negra) %>%
      rename(Populacao = estimativa_nao_negra) %>%
      mutate(GrupoRacial = "Não negra")
    
    # 8. Resultado final
    dados_faixa_instrucao_tidy <- bind_rows(dados_negra, dados_nao_negra)
    
  })
  
  
  # plot_grafico_escolaridade <- function(dados, grupo_racial, titulo = NULL) {
  #   
  #   ordem_faixas <- c("0 a 17 anos", "18 a 39 anos", "40 a 59 anos", "60 anos ou mais")
  #   ordem_instrucao <- c(
  #     "Sem instrução",
  #     "Fund. incompleto",
  #     "Fund. completo",
  #     "Médio incompleto",
  #     "Médio completo",
  #     "Superior ou +",
  #     "Sem resposta"
  #   )
  #   
  #   
  #   # Filtro pelo grupo racial
  #   dados_grafico <- dados %>%
  #     filter(GrupoRacial == grupo_racial) %>%
  #     group_by(GrauInstrucao, FaixaEtaria) %>%
  #     summarise(Populacao = sum(Populacao, na.rm = TRUE), .groups = "drop") %>%
  #     mutate(
  #       FaixaEtaria = factor(FaixaEtaria, levels = ordem_faixas),
  #       GrauInstrucao = factor(GrauInstrucao, levels = ordem_instrucao)
  #     )
  #   
  #   categorias_x <- levels(dados_grafico$GrauInstrucao)
  #   faixas <- levels(dados_grafico$FaixaEtaria)
  #   
  #   # Cores para as faixas etárias
  #   cores <- RColorBrewer::brewer.pal(n = max(3, min(12, length(faixas))), name = "Set2")
  #   if (length(faixas) > length(cores)) {
  #     cores <- rep(cores, length.out = length(faixas))
  #   }
  #   
  #   # Título padrão se não for fornecido
  #   if (is.null(titulo)) {
  #     titulo <- paste("Distribuição por Escolaridade e Faixa Etária -", grupo_racial)
  #   }
  #   
  #   # Cria o gráfico
  #   hc <- highchart() %>%
  #     hc_chart(type = "column") %>%
  #     hc_title(text = titulo) %>%
  #     hc_xAxis(categories = categorias_x, title = list(text = "Grau de Instrução")) %>%
  #     hc_yAxis(title = list(text = "População Estimada"), labels = list(format = "{value:,.0f}")) %>%
  #     hc_plotOptions(column = list(grouping = TRUE)) %>%
  #     hc_legend(title = list(text = "Faixa Etária"), enabled = TRUE)
  #   
  #   # Adiciona as séries por faixa etária
  #   for (i in seq_along(faixas)) {
  #     faixa_i <- faixas[i]
  #     dados_i <- dados_grafico %>%
  #       filter(FaixaEtaria == faixa_i) %>%
  #       arrange(match(GrauInstrucao, categorias_x)) %>%
  #       pull(Populacao)
  #     
  #     hc <- hc %>%
  #       hc_add_series(name = faixa_i, data = dados_i, color = cores[i])
  #   }
  #   
  #   hc
  # }
  plot_grafico_escolaridade <- function(dados, grupo_racial, titulo = NULL) {
    
    ordem_faixas <- c("0 a 17 anos", "18 a 39 anos", "40 a 59 anos", "60 anos ou mais")
    ordem_instrucao <- c(
      "Sem instrução",
      "Fund. incompleto",
      "Fund. completo",
      "Médio incompleto",
      "Médio completo",
      "Superior ou +",
      "Sem resposta"
    )
    
    dados_grafico <- dados %>%
      filter(GrupoRacial == grupo_racial) %>%
      group_by(GrauInstrucao, FaixaEtaria) %>%
      summarise(Populacao = sum(Populacao, na.rm = TRUE), .groups = "drop") %>%
      mutate(
        FaixaEtaria = factor(FaixaEtaria, levels = ordem_faixas),
        GrauInstrucao = factor(GrauInstrucao, levels = ordem_instrucao)
      )
    
    categorias_x <- levels(dados_grafico$GrauInstrucao)
    faixas <- levels(dados_grafico$FaixaEtaria)
    
    cores <- RColorBrewer::brewer.pal(n = max(3, min(12, length(faixas))), name = "Set2")
    if (length(faixas) > length(cores)) {
      cores <- rep(cores, length.out = length(faixas))
    }
    
    if (is.null(titulo)) {
      titulo <- paste("Distribuição por Escolaridade e Faixa Etária -", grupo_racial)
    }
    
    hc <- highchart() %>%
      hc_chart(type = "column") %>%
      hc_title(text = titulo) %>%
      hc_xAxis(categories = categorias_x, title = list(text = "Grau de Instrução")) %>%
      hc_yAxis(title = list(text = "População Estimada"),
               labels = list(format = "{value:,.0f}")) %>%
      hc_plotOptions(column = list(grouping = TRUE)) %>%
      hc_legend(title = list(text = "Faixa Etária"), enabled = TRUE)
    
    for (i in seq_along(faixas)) {
      faixa_i <- faixas[i]
      dados_i <- dados_grafico %>%
        filter(FaixaEtaria == faixa_i) %>%
        arrange(match(GrauInstrucao, categorias_x)) %>%
        pull(Populacao) %>%
        round()
      
      hc <- hc %>%
        hc_add_series(name = faixa_i, data = dados_i, color = cores[i])
    }
    
    hc
  }
  
  
  output$plot_1 <- renderHighchart({
    plot_grafico_escolaridade(
      dados = dados_plot_faixa_etaria(),
      grupo_racial = "Negra",
      titulo = "População Negra - Grau de Instrução x Faixa Etária"
    )
  })
  
  output$plot_2 <- renderHighchart({
    plot_grafico_escolaridade(
      dados = dados_plot_faixa_etaria(),
      grupo_racial = "Não negra",
      titulo = "População Não Negra - Grau de Instrução x Faixa Etária"
    )
  })
}
