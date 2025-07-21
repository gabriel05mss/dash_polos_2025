PsexoUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    
    fluidRow(
      box(
        title = h1('Escolaridade por Sexo - População em Situação de Rua no Brasil - Dezembro/2024 ', align = 'center'), #trocar titulo
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
        title = h1('Escolaridade por Sexo - População Negra', align = 'center'), #trocar titulo
        width = 6,
        collapsible = TRUE,
        solidHeader = TRUE,
        withSpinner(highchartOutput(ns("plot_1")), type = 1, color = "#ffae00", size = 2)
      ),
      
      box(
        title = h1('Escolaridade por Sexo - População Não Negra', align = 'center'), #trocar titulo
        width = 6,
        collapsible = TRUE,
        solidHeader = TRUE,
        withSpinner(highchartOutput(ns("plot_2")), type = 1, color = "#ffae00", size = 2)
      )
    ),
    
    fluidRow(
      
      box(
        title = h1('titulo', align = 'center'), #trocar titulo
        width = 6,
        collapsible = TRUE,
        solidHeader = TRUE,
        withSpinner(highchartOutput(ns("plot_3")), type = 1, color = "#ffae00", size = 2)
      ),
      
      box(
        title = h1('titulo ', align = 'center'), #trocar titulo
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

PsexoServer <- function(input, output, session, dados) {
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
    filtrado = NULL,
    plot_n_negros = NULL,
    plot_negros = NULL
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
  
# banco de dados tratado para sexo
  
  observe({
    req(df$filtrado)
    # 1. Selecionar apenas colunas necessárias
    dadossexo <- df$filtrado %>%
      select(
        municipio,
        mesorregioes,
        microrregioes,
        total_pop_em_situacao_de_rua,
        masculino, 
        feminino,
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
    #----------------------
    
    
    # 1. Garantir que colunas sejam numéricas
    dadossexo <- dadossexo %>%
      mutate(across(
        c(masculino, feminino, total_pop_em_situacao_de_rua,
          sem_instrucao, fundamental_incompleto, fundamental_completo,
          medio_incompleto, medio_completo, superior_incompleto_ou_mais,sem_resposta_18,
          branca, preta, amarela, parda, indigena, sem_resposta_30),
        ~ as.numeric(.)
      ))
    
    dados_proporcional <- dadossexo %>%
      mutate(
        prop_masc = ifelse(total_pop_em_situacao_de_rua > 0,
                           masculino / total_pop_em_situacao_de_rua, 0),
        prop_fem  = ifelse(total_pop_em_situacao_de_rua > 0,
                           feminino  / total_pop_em_situacao_de_rua, 0)
      )
    
    # 3. Calcular total de População Negra e Não Negra
    dados_raca <- dados_proporcional %>%
      mutate(
        pop_negra = preta + parda,
        pop_nao_negra = branca + amarela + indigena + sem_resposta_30,
        prop_negra = ifelse(total_pop_em_situacao_de_rua > 0,
                            pop_negra / total_pop_em_situacao_de_rua, 0),
        prop_nao_negra = ifelse(total_pop_em_situacao_de_rua > 0,
                                pop_nao_negra / total_pop_em_situacao_de_rua, 0)
      )
    
    # 4. Transformar graus de instrução em formato long
    dados_long <- dados_raca %>%
      pivot_longer(
        cols = c(
          sem_instrucao, fundamental_incompleto, fundamental_completo,
          medio_incompleto, medio_completo, superior_incompleto_ou_mais,sem_resposta_18
        ),
        names_to = "GrauInstrucao",
        values_to = "PopulacaoGrau"
      ) %>%
      mutate(
        PopulacaoGrau = as.numeric(PopulacaoGrau),
        # estimar por grupo racial e sexo
        pop_masc_negra     = PopulacaoGrau * prop_masc * prop_negra,
        pop_fem_negra      = PopulacaoGrau * prop_fem  * prop_negra,
        pop_masc_nao_negra = PopulacaoGrau * prop_masc * prop_nao_negra,
        pop_fem_nao_negra  = PopulacaoGrau * prop_fem  * prop_nao_negra
      )
    
    # 5. Selecionar, juntar e formatar os dois bancos
    
    # População Negra
    dados_negra <- dados_long %>%
      select(
        municipio, mesorregioes, microrregioes, estado, uf,
        GrauInstrucao, pop_masc_negra, pop_fem_negra
      ) %>%
      pivot_longer(
        cols = c(pop_masc_negra, pop_fem_negra),
        names_to = "Sexo", values_to = "Populacao"
      ) %>%
      mutate(
        Sexo = ifelse(Sexo == "pop_masc_negra", "Masculino", "Feminino"),
        GrupoRacial = "Negra"
      )
    
    # População Não Negra
    dados_nao_negra <- dados_long %>%
      select(
        municipio, mesorregioes, microrregioes, estado, uf,
        GrauInstrucao, pop_masc_nao_negra, pop_fem_nao_negra
      ) %>%
      pivot_longer(
        cols = c(pop_masc_nao_negra, pop_fem_nao_negra),
        names_to = "Sexo", values_to = "Populacao"
      ) %>%
      mutate(
        Sexo = ifelse(Sexo == "pop_masc_nao_negra", "Masculino", "Feminino"),
        GrupoRacial = "Não negra"
      )
    
    # 6. Juntar em um único banco final
    dados_raca_tidy <- bind_rows(dados_negra, dados_nao_negra)
    
    # Reorganizar nomes dos graus de instrução para ficarem mais legíveis
    grau_nomes <- c(
      "sem_instrucao" = "Sem instrução",
      "fundamental_incompleto" = "Fund. incompleto",
      "fundamental_completo" = "Fund. completo",
      "medio_incompleto" = "Médio incompleto",
      "medio_completo" = "Médio completo",
      "superior_incompleto_ou_mais" = "Superior ou +",
      "sem_resposta_18" = "Sem resposta"
      
    )
    # Definir a ordem desejada como fator ordenado
    
    # Aplicar nomes bonitos
    dados_plot <- dados_raca_tidy %>%
      mutate(GrauInstrucao = recode(GrauInstrucao, !!!grau_nomes))
      
    # Vetor com a ordem desejada
    ordem_graus <- c(
      "Sem instrução",
      "Fund. incompleto",
      "Fund. completo",
      "Médio incompleto",
      "Médio completo",
      "Superior ou +",
      "Sem resposta"
    )
    
    # Preparar os dados
    dados_negros <- dados_plot %>%
      filter(GrupoRacial == "Negra") %>%
      group_by(GrauInstrucao, Sexo) %>%
      summarise(Populacao = sum(Populacao, na.rm = TRUE), .groups = "drop") %>%
      mutate(GrauInstrucao = factor(GrauInstrucao, levels = ordem_graus)) %>%
      arrange(GrauInstrucao)
    
    dados_nao_negros <- dados_plot %>%
      filter(GrupoRacial == "Não negra") %>%
      group_by(GrauInstrucao, Sexo) %>%
      summarise(Populacao = sum(Populacao, na.rm = TRUE), .groups = "drop") %>%
      mutate(GrauInstrucao = factor(GrauInstrucao, levels = ordem_graus)) %>%
      arrange(GrauInstrucao)
    
    df$plot_negros <- dados_negros
    df$plot_n_negros <- dados_nao_negros
  }) 
    

output$plot_1 <- renderHighchart({
  req(df$plot_negros)

  # Extrair categorias ordenadas
  categorias <- levels(df$plot_negros$GrauInstrucao)
  
  # Criar séries para cada sexo
  serie_masculino <- df$plot_negros %>%
    filter(Sexo == "Masculino") %>%
    arrange(GrauInstrucao) %>%
    pull(Populacao)
  
  serie_feminino <- df$plot_negros %>%
    filter(Sexo == "Feminino") %>%
    arrange(GrauInstrucao) %>%
    pull(Populacao)
  
  highchart() %>%
    hc_chart(type = "column") %>%
    hc_title(text = "População Negra por Sexo e Grau de Instrução") %>%
    hc_xAxis(categories = categorias,
             title = list(text = "Grau de Instrução")) %>%
    hc_yAxis(title = list(text = "População estimada"),
             labels = list(format = "{value:,.0f}")) %>%
    hc_plotOptions(column = list(grouping = TRUE)) %>%
    hc_add_series(name = "Masculino", data = serie_masculino, color = "#1f77b4") %>%
    hc_add_series(name = "Feminino",  data = serie_feminino,  color = "#ff7f0e") %>%
    hc_legend(enabled = TRUE)
})
  



# Nao negro
# ----------------------
# Gráfico 2 – População Não Negra
# ----------------------
output$plot_2 <- renderHighchart({
  req(df$plot_n_negros)
  
  categorias <- levels(df$plot_n_negros$GrauInstrucao)
  
  serie_masculino <- df$plot_n_negros %>%
    filter(Sexo == "Masculino") %>%
    arrange(GrauInstrucao) %>%
    pull(Populacao)
  
  serie_feminino <- df$plot_n_negros %>%
    filter(Sexo == "Feminino") %>%
    arrange(GrauInstrucao) %>%
    pull(Populacao)
  
  highchart() %>%
    hc_chart(type = "column") %>%
    hc_title(text = "População Não Negra por Sexo e Grau de Instrução") %>%
    hc_xAxis(categories = categorias) %>%
    hc_yAxis(title = list(text = "População estimada"),
             labels = list(format = "{value:,.0f}")) %>%
    hc_add_series(name = "Masculino",
                  data = serie_masculino,
                  color = "#1f77b4") %>%
    hc_add_series(name = "Feminino",
                  data = serie_feminino,
                  color = "#ff7f0e")
})
}