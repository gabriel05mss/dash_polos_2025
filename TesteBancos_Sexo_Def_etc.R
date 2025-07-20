dadosteste <- read_excel("dados/dados_edu.xlsx", col_types = "text") %>% 
  clean_names()
library(tidyverse)
# 1. Selecionar apenas colunas necessárias
dadossexo1 <- dados %>%
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


library(dplyr)
library(tidyr)

# 1. Garantir que colunas sejam numéricas
dadossexo1 <- dadossexo1 %>%
  mutate(across(
    c(masculino, feminino, total_pop_em_situacao_de_rua,
      sem_instrucao, fundamental_incompleto, fundamental_completo,
      medio_incompleto, medio_completo, superior_incompleto_ou_mais,sem_resposta_18,
      branca, preta, amarela, parda, indigena, sem_resposta_30),
    ~ as.numeric(.)
  ))

# 2. Criar variáveis de proporção para sexo
dados_proporcional <- dadossexo1 %>%
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
#--------------------------------------------------------------------------------
install.packages("patchwork")
library(ggplot2)
library(dplyr)
library(scales)

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


# Gráfico 1: População Negra
grafico_negra <- dados_plot %>%
  filter(GrupoRacial == "Negra") %>%
  group_by(GrauInstrucao, Sexo) %>%
  summarise(Populacao = sum(Populacao, na.rm = TRUE), .groups = "drop") %>%
  mutate(GrauInstrucao = factor(GrauInstrucao, levels = ordem_graus)) %>%
  ggplot(aes(x = GrauInstrucao, y = Populacao, fill = Sexo)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  labs(
    title = "População Negra por Sexo e Grau de Instrução",
    x = "Grau de Instrução",
    y = "População estimada"
  ) +
  scale_y_continuous(labels = scales::label_comma()) +
  scale_fill_manual(values = c("Masculino" = "#1f77b4", "Feminino" = "#ff7f0e")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Gráfico 2: População Não Negra
grafico_nao_negra <- dados_plot %>%
  filter(GrupoRacial == "Não negra") %>%
  group_by(GrauInstrucao, Sexo) %>%
  summarise(Populacao = sum(Populacao, na.rm = TRUE), .groups = "drop") %>%
  mutate(GrauInstrucao = factor(GrauInstrucao, levels = ordem_graus)) %>%
  ggplot(aes(x = GrauInstrucao, y = Populacao, fill = Sexo)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  labs(
    title = "População Não Negra por Sexo e Grau de Instrução",
    x = "Grau de Instrução",
    y = "População estimada"
  ) +
  scale_y_continuous(labels = scales::label_comma()) +
  scale_fill_manual(values = c("Masculino" = "#1f77b4", "Feminino" = "#ff7f0e")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

library(patchwork)
grafico_negra
grafico_nao_negra
