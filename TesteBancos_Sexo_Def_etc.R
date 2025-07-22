dadosteste <- read_excel("dados/dados_edu.xlsx", col_types = "text") %>% 
  clean_names()
library(tidyverse)
# 1. Selecionar apenas colunas necessárias


# banco faixa etária
dadosfaixaetaria <- dadosteste %>%
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


# Ordenar faixa etária (caso ainda não esteja)
ordem_faixas <- c("0 a 17 anos", "18 a 39 anos", "40 a 59 anos", "60 anos ou mais")
ordem_instrucao <- c(
  "Sem instrução", "Fundamental incompleto", "Fundamental completo",
  "Médio incompleto", "Médio completo", "Superior ou mais", "Sem resposta"
)

#--------------------grafico ggplot
grafico_negra <- dados_faixa_instrucao_tidy %>%
  filter(GrupoRacial == "Negra", !is.na(Populacao), !is.nan(Populacao), Populacao > 0) %>%
  mutate(GrauInstrucao = factor(GrauInstrucao, levels = c(
    "Sem instrução", "Fund. incompleto", "Fund. completo",
    "Médio incompleto", "Médio completo", "Superior ou +", "Sem resposta"
  ))) %>%
  ggplot(aes(x = GrauInstrucao, y = Populacao, fill = FaixaEtaria)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  scale_y_continuous(labels = label_comma()) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "População Negra por Grau de Instrução e Faixa Etária",
    x = "Grau de Instrução",
    y = "População Estimada",
    fill = "Faixa Etária"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




grafico_negra


#----
grafico_nao_negra <- dados_faixa_instrucao_tidy %>%
  filter(GrupoRacial == "Não negra", !is.na(Populacao), !is.nan(Populacao), Populacao > 0) %>%
  mutate(GrauInstrucao = factor(GrauInstrucao, levels = c(
    "Sem instrução", "Fund. incompleto", "Fund. completo",
    "Médio incompleto", "Médio completo", "Superior ou +", "Sem resposta"
  ))) %>%
  ggplot(aes(x = GrauInstrucao, y = Populacao, fill = FaixaEtaria)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  scale_y_continuous(labels = label_comma()) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "População Não Negra por Grau de Instrução e Faixa Etária",
    x = "Grau de Instrução",
    y = "População Estimada",
    fill = "Faixa Etária"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

grafico_nao_negra
#------
