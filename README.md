#### ANÁLISES BOLETIM DE BALANÇO - QUESTIONÁRIO ESTADUAL CNCA ####

# importação dos dados 

library(readxl)
library(dplyr)
library(janitor)
library(ggplot2)
library(forcats)
library(scales)
library(stringr)

dados <- read_excel("C:/Users/joao.correa/OneDrive - INEP/Área de Trabalho/ICA/questionario_cnca.xlsx") %>%
  clean_names()
View(dados)


# PERFIL DOS RESPONDENTES 

## perfil sociodemográfico

### sexo 
perfil_sexo <- dados %>%
  filter(!is.na(sexo)) %>%
  count(sexo) %>%
  mutate(percentual = n/sum(n))

perfil_sexo


ggplot(perfil_sexo,
       aes(x = fct_reorder(sexo, percentual),
           y = percentual)) +
  geom_col() +
  geom_text(aes(label = percent(percentual, accuracy = 1)),
            hjust = -0.1) +
  scale_y_continuous(labels = percent_format()) +
  coord_flip() +
  labs(title = "Distribuição por Sexo",
       y = "Percentual",
       x = "") +
  theme_minimal()

### cor/raça
perfil_raca_cor <- dados %>%
  filter(!is.na(raca_cor)) %>%
  count(raca_cor) %>%
  mutate(percentual = n/sum(n))

perfil_raca_cor


ggplot(perfil_raca_cor,
       aes(x = fct_reorder(raca_cor, percentual),
           y = percentual)) +
  geom_col() +
  geom_text(aes(label = percent(percentual, accuracy = 1)),
            hjust = -0.1) +
  scale_y_continuous(labels = percent_format()) +
  coord_flip() +
  labs(title = "Distribuição por Raça/Cor",
       y = "Percentual",
       x = "") +
  theme_minimal()


## perfil profissional

### escolaridade

perfil_escolaridade <- dados %>%
  filter(!is.na(escolaridade_nivel_mais_alto_concluido)) %>%
  count(resposta = escolaridade_nivel_mais_alto_concluido) %>%
  mutate(percentual = n/sum(n))

perfil_escolaridade # ótimo nível de qualificação acadêmica entre os articuladores estaduais


ggplot(perfil_escolaridade,
       aes(x = fct_reorder(resposta, percentual),
           y = percentual)) +
  geom_col() +
  geom_text(aes(label = percent(percentual, accuracy = 1)),
            hjust = -0.1) +
  scale_y_continuous(labels = percent_format()) +
  coord_flip() +
  labs(title = "Escolaridade dos Respondentes",
       x = "",
       y = "Percentual") +
  theme_minimal()

### cargo

perfil_cargo <- dados %>%
  filter(!is.na(cargo_atual)) %>%
  count(cargo = cargo_atual) %>%
  mutate(percentual = n / sum(n),
         rotulo = percent(percentual, accuracy = 1))

perfil_cargo


library(forcats)

perfil_cargo <- perfil_cargo %>%
  mutate(cargo = fct_reorder(cargo, percentual))

ggplot(perfil_cargo,
       aes(x = "", 
           y = percentual, 
           fill = cargo)) +
  geom_col(color = "white", width = 1) +
  coord_polar(theta = "y") +
  geom_text(aes(label = rotulo),
            position = position_stack(vjust = 0.5),
            size = 4) +
  labs(title = "Distribuição da Escolaridade dos Respondentes",
       fill = "Escolaridade") +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

### formação inicial 

perfil_formacao <- dados %>%
  filter(!is.na(formacao_inicial)) %>%
  count(resposta = formacao_inicial) %>%
  mutate(percentual = n/sum(n))

perfil_formacao

ggplot(perfil_formacao,
       aes(x = fct_reorder(resposta, percentual),
           y = percentual)) +
  geom_col() +
  geom_text(aes(label = percent(percentual, accuracy = 1)),
            hjust = -0.1) +
  scale_y_continuous(labels = percent_format()) +
  coord_flip() +
  labs(title = "Formação Inicial dos Respondentes",
       x = "",
       y = "Percentual") +
  theme_minimal()


### tempo de experiência 

perfil_tempo_exp <- dados %>%
  filter(!is.na(considerando_apenas_o_periodo_anterior_ao_inicio_da_atuacao_como_articulador_qual_e_o_seu_tempo_de_experiencia_na_articulacao_e_lideranca_de_processos_e_politicas_da_educacao_basica_como_formacao_de_professores_e_gestores_gestao_educacional_avaliacao)) %>%
  count(tempo_exp = considerando_apenas_o_periodo_anterior_ao_inicio_da_atuacao_como_articulador_qual_e_o_seu_tempo_de_experiencia_na_articulacao_e_lideranca_de_processos_e_politicas_da_educacao_basica_como_formacao_de_professores_e_gestores_gestao_educacional_avaliacao) %>%
  mutate(percentual = n / sum(n),
         rotulo = percent(percentual, accuracy = 1),
         tempo_exp = fct_reorder(tempo_exp, percentual))

perfil_tempo_exp


ggplot(perfil_tempo_exp,
       aes(x = tempo_exp,
           y = percentual)) +
  geom_col() +
  geom_text(aes(label = rotulo),
            vjust = -0.5,
            size = 4) +
  scale_y_continuous(labels = percent_format(),
                     expand = expansion(mult = c(0, 0.1))) +
  labs(title = "Escolaridade dos Respondentes",
       x = "Escolaridade",
       y = "Percentual") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )




# ESTRUTURA ANALÍTICA POR EIXO 

## eixo 1 - governança e articulação (CEEC / RENALFA)

### tabela consolidada 

library(dplyr)
library(janitor)
library(ggplot2)
library(scales)
library(forcats)
library(tidyr)


tabela_governanca <- dados %>%
  select(indique_como_voce_avalia_a_atuacao_do_ceec_em_relacao_as_atribuicoes_instituidas_de_acordo_com_os_aspectos_abaixo,
         como_voce_avalia_a_interlocucao_entre_os_articuladores_estaduais_secretaria_de_educacao_e_undime_no_fortalecimento_da_gestao_da_politica_de_alfabetizacao_do_territorio,
         avalie_os_aspectos_relacionados_a_coordenacao_das_acoes_de_alfabetizacao_no_seu_territorio) %>%
  pivot_longer(cols = everything(),
               names_to = "pergunta",
               values_to = "resposta") %>%
  filter(!is.na(resposta)) %>%
  count(pergunta, resposta) %>%
  group_by(pergunta) %>%
  mutate(percentual = n/sum(n))

tabela_governanca <- tabela_governanca %>%
  mutate(pergunta = case_when(
    str_detect(pergunta, "indique_como_voce_avalia_a_atuacao_do_ceec_em_relacao_as_atribuicoes_instituidas_de_acordo_com_os_aspectos_abaixo") ~ "atuacao_do_ceec",
    str_detect(pergunta, "como_voce_avalia_a_interlocucao_entre_os_articuladores_estaduais_secretaria_de_educacao_e_undime_no_fortalecimento_da_gestao_da_politica_de_alfabetizacao_do_territorio") ~ "interlocucao_sec_undime",
    str_detect(pergunta, "avalie_os_aspectos_relacionados_a_coordenacao_das_acoes_de_alfabetizacao_no_seu_territorio") ~ "coordenacao_das_acoes",
    TRUE ~ pergunta
  ))

tabela_governanca


indicador_governanca <- tabela_governanca %>%
  filter(resposta %in% c("Muito suficiente", "Suficiente")) %>%
  group_by(pergunta) %>%
  summarise(percentual_positivo = sum(percentual))

indicador_governanca


ggplot(indicador_governanca,
       aes(x = fct_reorder(pergunta, percentual_positivo),
           y = percentual_positivo)) +
  geom_col() +
  geom_text(aes(label = percent(percentual_positivo, accuracy = 1)),
            hjust = -0.1) +
  scale_y_continuous(labels = percent_format()) +
  coord_flip() +
  labs(title = "Indicador de Avaliação Positiva – Governança",
       x = "",
       y = "Percentual positivo") +
  theme_minimal()



## eixo 2 - formação (LEEI + PATE/PAR)

tabela_formacao <- dados %>%
  select(como_voce_avalia_a_qualidade_das_formacoes_coordenadas_pelo_mec_no_ambito_da_renalfa,
         como_voce_avalia_a_relevancia_das_formacoes_ofertadas_pelo_mec_no_ambito_da_renalfa_para_sua_atuacao_como_articulador_estadual,
         considerando_os_seguintes_contextos_as_formacoes_realizadas_ate_o_momento_sao_suficientes_para_que_voce_desenvolva_suas_atividades) %>%
  pivot_longer(cols = everything(),
               names_to = "pergunta",
               values_to = "resposta") %>%
  filter(!is.na(resposta)) %>%
  count(pergunta, resposta) %>%
  group_by(pergunta) %>%
  mutate(percentual = n/sum(n))

tabela_formacao <- tabela_formacao %>%
  mutate(pergunta = case_when(
    str_detect(pergunta, "como_voce_avalia_a_qualidade_das_formacoes_coordenadas_pelo_mec_no_ambito_da_renalfa") ~ "qualidade_formacao_renalfa",
    str_detect(pergunta, "como_voce_avalia_a_relevancia_das_formacoes_ofertadas_pelo_mec_no_ambito_da_renalfa_para_sua_atuacao_como_articulador_estadual") ~ "relevancia_formacao_renalfa_articulador",
    str_detect(pergunta, "considerando_os_seguintes_contextos_as_formacoes_realizadas_ate_o_momento_sao_suficientes_para_que_voce_desenvolva_suas_atividades") ~ "suficiencia_atuacao_atividades",
    TRUE ~ pergunta
  ))


tabela_formacao

ggplot(tabela_formacao,
       aes(x = resposta,
           y = pergunta,
           fill = percentual)) +
  geom_tile() +
  geom_text(aes(label = percent(percentual, accuracy = 1))) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(title = "Distribuição das Avaliações – Formação",
       x = "",
       y = "") +
  theme_minimal()


## eixo 3 - materiais complementares



tabela_materiais <- dados %>%
  select(avalie_a_producao_distribuicao_dos_materiais_complementares_de_1o_e_2o_ano_do_ensino_fundamental_previstos_no_1o_ciclo_pate_par_2023_2025_e_distribuidos_para_a_rede_estadual_e_redes_municipais,
         avalie_a_producao_distribuicao_dos_materiais_complementares_de_3o_ao_5o_ano_do_ensino_fundamental_previstos_no_1o_ciclo_pate_par_2023_2025_e_distribuidos_para_a_rede_estadual_e_redes_municipais) %>%
  pivot_longer(cols = everything(),
               names_to = "etapa",
               values_to = "resposta") %>%
  filter(!is.na(resposta)) %>%
  count(etapa, resposta) %>%
  group_by(etapa) %>%
  mutate(percentual = n/sum(n))


tabela_materiais <- tabela_materiais %>%
  mutate(etapa = case_when(
    str_detect(etapa, "avalie_a_producao_distribuicao_dos_materiais_complementares_de_1o_e_2o_ano_do_ensino_fundamental_previstos_no_1o_ciclo_pate_par_2023_2025_e_distribuidos_para_a_rede_estadual_e_redes_municipais") ~ "producao_distribuicao_1_2ano",
    str_detect(etapa, "avalie_a_producao_distribuicao_dos_materiais_complementares_de_3o_ao_5o_ano_do_ensino_fundamental_previstos_no_1o_ciclo_pate_par_2023_2025_e_distribuidos_para_a_rede_estadual_e_redes_municipais") ~ "producao_distribuicao_3_5ano",
    TRUE ~ etapa
  ))


tabela_materiais


ggplot(tabela_materiais,
       aes(x = resposta,
           y = percentual,
           fill = etapa)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = percent_format()) +
  labs(title = "Comparação – Materiais por Etapa",
       x = "",
       y = "Percentual") +
  theme_minimal()


## eixo 4 - avaliações formativas 

### plataforma cnca
tabela_plataforma_cnca <- dados %>%
  select(as_escolas_da_rede_estadual_distrital_utilizaram_a_plataforma_compromisso_nacional_crianca_alfabetizada_mec_para_realizar_avaliacoes_formativas_em_2025,
         as_escolas_das_redes_municipais_utilizaram_a_plataforma_compromisso_nacional_crianca_alfabetizada_mec_para_realizar_avaliacoes_formativas_em_2025) %>%
  pivot_longer(cols = everything(),
               names_to = "rede",
               values_to = "resposta") %>%
  filter(!is.na(resposta)) %>%
  count(rede, resposta) %>%
  group_by(rede) %>%
  mutate(percentual = n/sum(n))


tabela_plataforma_cnca <- tabela_plataforma_cnca %>%
  mutate(rede = case_when(
    str_detect(rede, "as_escolas_da_rede_estadual_distrital_utilizaram_a_plataforma_compromisso_nacional_crianca_alfabetizada_mec_para_realizar_avaliacoes_formativas_em_2025") ~ "cnca_estadual",
    str_detect(rede, "as_escolas_das_redes_municipais_utilizaram_a_plataforma_compromisso_nacional_crianca_alfabetizada_mec_para_realizar_avaliacoes_formativas_em_2025") ~ "cnca_municipal",
    TRUE ~ rede
  ))

tabela_plataforma_cnca


ggplot(tabela_plataforma_cnca,
       aes(x = rede,
           y = percentual,
           fill = resposta)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = percent(percentual, accuracy = 1)),
            position = position_dodge(width = 0.9),
            vjust = -0.5) +
  scale_y_continuous(labels = percent_format(),
                     expand = expansion(mult = c(0, 0.1))) +
  labs(title = "Uso da Plataforma CNCA por Rede",
       x = "",
       y = "Percentual",
       fill = "Resposta") +
  theme_minimal()



### plataforma - outro instrumentos
tabela_plataforma_outros <- dados %>%
  select(as_escolas_da_rede_estadual_distrital_utilizaram_outros_instrumentos_de_avaliacao_formativa_distintos_da_plataforma_compromisso_nacional_crianca_alfabetizada_mec_em_2025,
         as_escolas_das_redes_municipais_utilizaram_outros_instrumentos_de_avaliacao_formativa_distintos_da_plataforma_compromisso_nacional_crianca_alfabetizada_mec_em_2025) %>%
  pivot_longer(cols = everything(),
               names_to = "rede",
               values_to = "resposta") %>%
  filter(!is.na(resposta)) %>%
  count(rede, resposta) %>%
  group_by(rede) %>%
  mutate(percentual = n/sum(n))


tabela_plataforma_outros <- tabela_plataforma_outros %>%
  mutate(rede = case_when(
    str_detect(rede, "as_escolas_da_rede_estadual_distrital_utilizaram_outros_instrumentos_de_avaliacao_formativa_distintos_da_plataforma_compromisso_nacional_crianca_alfabetizada_mec_em_2025") ~ "outros_instrumentos_estadual",
    str_detect(rede, "as_escolas_das_redes_municipais_utilizaram_outros_instrumentos_de_avaliacao_formativa_distintos_da_plataforma_compromisso_nacional_crianca_alfabetizada_mec_em_2025") ~ "outros_instrumentos_municipal",
    TRUE ~ rede
  ))

tabela_plataforma_outros


ggplot(tabela_plataforma_outros,
       aes(x = rede,
           y = percentual,
           fill = resposta)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = percent(percentual, accuracy = 1)),
            position = position_dodge(width = 0.9),
            vjust = -0.5) +
  scale_y_continuous(labels = percent_format(),
                     expand = expansion(mult = c(0, 0.1))) +
  labs(title = "Uso da Outros Instrumentos por Rede",
       x = "",
       y = "Percentual",
       fill = "Resposta") +
  theme_minimal()


## eixo 5 - selo nacional 

tabela_selo <- dados %>%
  select(ter_recebido_o_selo_2024_ouro_prata_ou_bronze_contribuiu_para_engajar_equipes_e_impulsionar_acoes_ou_programas,
         de_maneira_geral_como_voce_avalia_a_relevancia_do_selo_2024_ouro_prata_ou_bronze_como_ferramenta_de_disseminacao_valorizacao_e_mobilizacao_da_politica_de_alfabetizacao) %>%
  pivot_longer(cols = everything(),
               names_to = "pergunta",
               values_to = "resposta") %>%
  filter(!is.na(resposta)) %>%
  count(pergunta, resposta) %>%
  group_by(pergunta) %>%
  mutate(percentual = n/sum(n))

tabela_selo <- tabela_selo %>%
  mutate(pergunta = case_when(
    str_detect(pergunta, "ter_recebido_o_selo_2024_ouro_prata_ou_bronze_contribuiu_para_engajar_equipes_e_impulsionar_acoes_ou_programas") ~ "engajamento",
    str_detect(pergunta, "de_maneira_geral_como_voce_avalia_a_relevancia_do_selo_2024_ouro_prata_ou_bronze_como_ferramenta_de_disseminacao_valorizacao_e_mobilizacao_da_politica_de_alfabetizacao") ~ "relevancia_selo",
    TRUE ~ pergunta
  ))

tabela_selo

ggplot(tabela_selo,
       aes(x = pergunta,
           y = percentual,
           fill = resposta)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = percent(percentual, accuracy = 1)),
            position = position_dodge(width = 0.9),
            vjust = -0.5) +
  scale_y_continuous(labels = percent_format(),
                     expand = expansion(mult = c(0, 0.1))) +
  labs(title = "Uso da Outros Instrumentos por Rede",
       x = "",
       y = "Percentual",
       fill = "Resposta") +
  theme_minimal()


tabela_eixo5 <- dados %>%
  select(ter_recebido_o_selo_2024_ouro_prata_ou_bronze_contribuiu_para_engajar_equipes_e_impulsionar_acoes_ou_programas, de_maneira_geral_como_voce_avalia_a_relevancia_do_selo_2024_ouro_prata_ou_bronze_como_ferramenta_de_disseminacao_valorizacao_e_mobilizacao_da_politica_de_alfabetizacao) %>%
  filter(!is.na(ter_recebido_o_selo_2024_ouro_prata_ou_bronze_contribuiu_para_engajar_equipes_e_impulsionar_acoes_ou_programas),
         !is.na(de_maneira_geral_como_voce_avalia_a_relevancia_do_selo_2024_ouro_prata_ou_bronze_como_ferramenta_de_disseminacao_valorizacao_e_mobilizacao_da_politica_de_alfabetizacao)) %>%
  count(ter_recebido_o_selo_2024_ouro_prata_ou_bronze_contribuiu_para_engajar_equipes_e_impulsionar_acoes_ou_programas, de_maneira_geral_como_voce_avalia_a_relevancia_do_selo_2024_ouro_prata_ou_bronze_como_ferramenta_de_disseminacao_valorizacao_e_mobilizacao_da_politica_de_alfabetizacao) %>%
  group_by(ter_recebido_o_selo_2024_ouro_prata_ou_bronze_contribuiu_para_engajar_equipes_e_impulsionar_acoes_ou_programas) %>%
  mutate(percentual = n/sum(n))




tabela_eixo5 <- tabela_eixo5 %>%
  rename(
    engajamento = ter_recebido_o_selo_2024_ouro_prata_ou_bronze_contribuiu_para_engajar_equipes_e_impulsionar_acoes_ou_programas,
    relevancia_selo = de_maneira_geral_como_voce_avalia_a_relevancia_do_selo_2024_ouro_prata_ou_bronze_como_ferramenta_de_disseminacao_valorizacao_e_mobilizacao_da_politica_de_alfabetizacao
  )


ggplot(tabela_eixo5,
       aes(x = engajamento,
           y = percentual,
           color = relevancia_selo,
           group = relevancia_selo)) +
  geom_point(size = 3) +
  geom_line() +
  scale_y_continuous(labels = percent_format()) +
  labs(title = "Tendência de Relevância do Selo segundo Engajamento",
       x = "Engajamento",
       y = "Percentual",
       color = "Relevância do Selo") +
  theme_minimal()





