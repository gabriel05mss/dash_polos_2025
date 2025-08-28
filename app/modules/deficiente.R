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
        title = h1('Ranking Municípios', align = 'center'), #trocar titulo
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
  # output$tabela = renderDataTable({
  #   req(dados)
  #   dados_aux = dados %>%
  #     select(estado, uf, everything(), -arquivo_origem)
  #   
  #   if (!is_empty(filtros$estado)){
  #     dados_aux = dados_aux %>%
  #       filter(estado %in% filtros$estado)
  #   }
  #   
  #   if (!is_empty(filtros$meso)){
  #     dados_aux = dados_aux %>%
  #       filter(mesorregioes %in% filtros$meso)
  #   }
  #   
  #   if (!is_empty(filtros$micro)){
  #     dados_aux = dados_aux %>%
  #       filter(microrregioes %in% filtros$micro)
  #   }
  #   
  #   if (!is_empty(filtros$municipio)){
  #     dados_aux = dados_aux %>%
  #       filter(municipio %in% filtros$municipio)
  #   }
  #   
  #   df$filtrado <- dados_aux
  #   
  #   DT::datatable(
  #     dados_aux,
  #     rownames = FALSE,
  #     filter = "none", 
  #     style = "bootstrap",
  #     class = "stripe hover cell-border compact",
  #     options = list(
  #       dom = 'fltip', 
  #       pageLength = 10,
  #       lengthMenu = c(5, 10, 25, 50, 100),
  #       scrollX = TRUE,
  #       autoWidth = TRUE,
  #       searchHighlight = TRUE,
  #       columnDefs = list(
  #         list(className = 'dt-center', targets = "_all"),
  #         list(targets = 0, title = "Estado")
  #       )
  #     )
  #   ) %>%
  #     DT::formatStyle(
  #       columns = names(dados_aux),
  #       fontSize = '14px',
  #       color = 'black',
  #       fontWeight = 'normal',
  #       textAlign = 'center'
  #     )
  # })
  output$tabela = renderDataTable({
    req(dados)
    
    # Aplicar filtros
    dados_aux = dados
    if (!is_empty(filtros$estado)) {
      dados_aux = dados_aux %>% filter(estado %in% filtros$estado)
    }
    if (!is_empty(filtros$meso)) {
      dados_aux = dados_aux %>% filter(mesorregioes %in% filtros$meso)
    }
    if (!is_empty(filtros$micro)) {
      dados_aux = dados_aux %>% filter(microrregioes %in% filtros$micro)
    }
    if (!is_empty(filtros$municipio)) {
      dados_aux = dados_aux %>% filter(municipio %in% filtros$municipio)
    }
    
    # Salvar no reativo
    df$filtrado <- dados_aux
    
    # Selecionar colunas
    dados_aux <- dados_aux %>%
      select(
        estado,
        uf,
        municipio,
        mesorregioes,
        microrregioes,
        total_pop_em_situacao_de_rua,
        sim_162,
        nao_164,
        sem_instrucao, fundamental_incompleto, fundamental_completo,
        medio_incompleto, medio_completo, superior_incompleto_ou_mais,
        sem_resposta_18,
        branca, preta, amarela, parda, indigena, sem_resposta_30
      )
    
    # Aplicar ordenação condicional
    if (
      is_empty(filtros$estado) &&
      is_empty(filtros$meso) &&
      is_empty(filtros$micro) &&
      is_empty(filtros$municipio)
    ) {
      dados_aux <- dados_aux %>% arrange(municipio)  # Ordem alfabética
    } else {
      dados_aux <- dados_aux %>% arrange(desc(total_pop_em_situacao_de_rua))  # Ordem decrescente
    }
    
    # Renomear colunas
    colnames(dados_aux) <- c(
      "Estado", "UF", "Município", "Mesorregião", "Microrregião",
      "População em Situação de Rua",
      "Possui Deficiência = Sim",
      "Possui Deficiência = Não",
      "Sem Instrução", "Fundamental Incompleto", "Fundamental Completo",
      "Médio Incompleto", "Médio Completo", "Superior Incompleto ou +",
      "Sem Resposta (Escolaridade)",
      "Branca", "Preta", "Amarela", "Parda", "Indígena", "Sem Resposta (Raça)"
    )
    
    # Exibir tabela
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
        columnDefs = list(list(className = 'dt-center', targets = "_all"))
      )
    ) %>%
      DT::formatStyle(
        columns = names(dados_aux),
        fontSize = '11px',
        color = 'black',
        fontWeight = 'normal',
        textAlign = 'center'
      )
  })
  
  observe({
    req(df$filtrado)
    
    # 1. Selecionar apenas colunas necessárias
    dadosdef <- df$filtrado %>%
      select(
        municipio,
        mesorregioes,
        microrregioes,
        total_pop_em_situacao_de_rua,
        sim_162,
        nao_164,
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
    
    # 2. Garantir que colunas sejam numéricas
    dadosdef <- dadosdef %>%
      mutate(across(
        c(sim_162, nao_164, total_pop_em_situacao_de_rua,
          sem_instrucao, fundamental_incompleto, fundamental_completo,
          medio_incompleto, medio_completo, superior_incompleto_ou_mais, sem_resposta_18,
          branca, preta, amarela, parda, indigena, sem_resposta_30),
        ~ as.numeric(.)
      ))
    
    # 3. Proporção de deficiência
    dados_proporcional <- dadosdef %>%
      mutate(
        prop_def = ifelse(total_pop_em_situacao_de_rua > 0,
                          sim_162 / total_pop_em_situacao_de_rua, 0),
        prop_sem_def  = ifelse(total_pop_em_situacao_de_rua > 0,
                               nao_164  / total_pop_em_situacao_de_rua, 0)
      )
    
    # 4. Calcular total de População Negra e Não Negra
    dados_raca <- dados_proporcional %>%
      mutate(
        pop_negra = preta + parda,
        pop_nao_negra = branca + amarela + indigena + sem_resposta_30,
        prop_negra = ifelse(total_pop_em_situacao_de_rua > 0,
                            pop_negra / total_pop_em_situacao_de_rua, 0),
        prop_nao_negra = ifelse(total_pop_em_situacao_de_rua > 0,
                                pop_nao_negra / total_pop_em_situacao_de_rua, 0)
      )
    
    # 5. Transformar graus de instrução em formato long
    dados_long <- dados_raca %>%
      pivot_longer(
        cols = c(
          sem_instrucao, fundamental_incompleto, fundamental_completo,
          medio_incompleto, medio_completo, superior_incompleto_ou_mais, sem_resposta_18
        ),
        names_to = "GrauInstrucao",
        values_to = "PopulacaoGrau"
      ) %>%
      mutate(
        PopulacaoGrau = as.numeric(PopulacaoGrau),
        pop_def_negra        = PopulacaoGrau * prop_def     * prop_negra,
        pop_sem_def_negra    = PopulacaoGrau * prop_sem_def * prop_negra,
        pop_def_nao_negra    = PopulacaoGrau * prop_def     * prop_nao_negra,
        pop_sem_def_nao_negra= PopulacaoGrau * prop_sem_def * prop_nao_negra
      )
    
    # 6. População Negra
    dados_negra <- dados_long %>%
      select(
        municipio, mesorregioes, microrregioes, estado, uf,
        GrauInstrucao, pop_def_negra, pop_sem_def_negra
      ) %>%
      pivot_longer(
        cols = c(pop_def_negra, pop_sem_def_negra),
        names_to = "Deficiencia", values_to = "Populacao"
      ) %>%
      mutate(
        Deficiencia = ifelse(Deficiencia == "pop_def_negra", "Com deficiência", "Sem deficiência"),
        GrupoRacial = "Negra"
      )
    
    # 7. População Não Negra
    dados_nao_negra <- dados_long %>%
      select(
        municipio, mesorregioes, microrregioes, estado, uf,
        GrauInstrucao, pop_def_nao_negra, pop_sem_def_nao_negra
      ) %>%
      pivot_longer(
        cols = c(pop_def_nao_negra, pop_sem_def_nao_negra),
        names_to = "Deficiencia", values_to = "Populacao"
      ) %>%
      mutate(
        Deficiencia = ifelse(Deficiencia == "pop_def_nao_negra", "Com deficiência", "Sem deficiência"),
        GrupoRacial = "Não negra"
      )
    
    # 8. Juntar tudo
    dados_raca_tidy <- bind_rows(dados_negra, dados_nao_negra)
    
    grau_nomes <- c(
      "sem_instrucao" = "Sem instrução",
      "fundamental_incompleto" = "Fund. incompleto",
      "fundamental_completo" = "Fund. completo",
      "medio_incompleto" = "Médio incompleto",
      "medio_completo" = "Médio completo",
      "superior_incompleto_ou_mais" = "Superior ou +",
      "sem_resposta_18" = "Sem resposta"
    )
    
    ordem_graus <- c(
      "Sem instrução",
      "Fund. incompleto",
      "Fund. completo",
      "Médio incompleto",
      "Médio completo",
      "Superior ou +",
      "Sem resposta"
    )
    
    dados_plot <- dados_raca_tidy %>%
      mutate(GrauInstrucao = recode(GrauInstrucao, !!!grau_nomes))
    
    dados_def <- dados_plot %>%
      filter(GrupoRacial == "Negra") %>%
      group_by(GrauInstrucao, Deficiencia) %>%
      summarise(Populacao = sum(Populacao, na.rm = TRUE), .groups = "drop") %>%
      mutate(GrauInstrucao = factor(GrauInstrucao, levels = ordem_graus)) %>%
      arrange(GrauInstrucao)
    
    dados_n_def <- dados_plot %>%
      filter(GrupoRacial == "Não negra") %>%
      group_by(GrauInstrucao, Deficiencia) %>%
      summarise(Populacao = sum(Populacao, na.rm = TRUE), .groups = "drop") %>%
      mutate(GrauInstrucao = factor(GrauInstrucao, levels = ordem_graus)) %>%
      arrange(GrauInstrucao)
    
    df$plot_def <- dados_def
    df$plot_n_def <- dados_n_def
  })
  
  # --- GRÁFICO POPULAÇÃO NEGRA ---
  
  output$plot_1 <- renderHighchart({
    req(df$plot_def)
    
    categorias <- levels(df$plot_def$GrauInstrucao)
    
    serie_com_def <- round(df$plot_def %>%
                             filter(Deficiencia == "Com deficiência") %>%
                             arrange(GrauInstrucao) %>%
                             pull(Populacao))
    
    serie_sem_def <- round(df$plot_def %>%
                             filter(Deficiencia == "Sem deficiência") %>%
                             arrange(GrauInstrucao) %>%
                             pull(Populacao))
    
    highchart() %>%
      hc_chart(type = "column") %>%
      hc_title(text = "População Negra por Deficiência e Grau de Instrução") %>%
      hc_xAxis(categories = categorias,
               title = list(text = "Grau de Instrução")) %>%
      hc_yAxis(title = list(text = "População estimada"),
               labels = list(format = "{value:,.0f}")) %>%
      hc_plotOptions(column = list(grouping = TRUE)) %>%
      hc_add_series(name = "Com deficiência", data = serie_com_def, color = "#1f77b4") %>%
      hc_add_series(name = "Sem deficiência",  data = serie_sem_def,  color = "#ff7f0e") %>%
      hc_legend(enabled = TRUE) %>%
      hc_tooltip(pointFormat = "<b>{series.name}</b>: {point.y:,.0f}<br/>")
  })
  
  # --- GRÁFICO POPULAÇÃO NÃO NEGRA ---
  
  output$plot_2 <- renderHighchart({
    req(df$plot_n_def)
    
    categorias <- levels(df$plot_n_def$GrauInstrucao)
    
    serie_com_def <- round(df$plot_n_def %>%
                             filter(Deficiencia == "Com deficiência") %>%
                             arrange(GrauInstrucao) %>%
                             pull(Populacao))
    
    serie_sem_def <- round(df$plot_n_def %>%
                             filter(Deficiencia == "Sem deficiência") %>%
                             arrange(GrauInstrucao) %>%
                             pull(Populacao))
    
    highchart() %>%
      hc_chart(type = "column") %>%
      hc_title(text = "População Não Negra por Deficiência e Grau de Instrução") %>%
      hc_xAxis(categories = categorias,
               title = list(text = "Grau de Instrução")) %>%
      hc_yAxis(title = list(text = "População estimada"),
               labels = list(format = "{value:,.0f}")) %>%
      hc_plotOptions(column = list(grouping = TRUE)) %>%
      hc_add_series(name = "Com deficiência", data = serie_com_def, color = "#1f77b4") %>%
      hc_add_series(name = "Sem deficiência",  data = serie_sem_def,  color = "#ff7f0e") %>%
      hc_legend(enabled = TRUE) %>%
      hc_tooltip(pointFormat = "<b>{series.name}</b>: {point.y:,.0f}<br/>")
  })
}