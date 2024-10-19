## Dias da Semana



dados_dia_semana_br_total <- dados_br_total %>%
  mutate(diaobito_semana = wday(DTOBITO, label = TRUE, abbr = FALSE)) %>% 
  group_by(ANOOBITO, diaobito_semana) %>% 
  summarise(N.obitos = n(), .groups = 'drop') %>% 
  group_by(ANOOBITO) %>% 
  mutate(proporcao = N.obitos / sum(N.obitos))

dados_dia_semana_br_psic <- dados_br_psic %>%
  mutate(diaobito_semana = wday(DTOBITO, label = TRUE, abbr = FALSE)) %>%
  mutate(diaobito_semana = addNA(diaobito_semana)) %>% 
  group_by(ANOOBITO, diaobito_semana) %>% 
  summarise(N.obitos = n(), .groups = 'drop') %>% 
  group_by(ANOOBITO) %>% 
  mutate(proporcao = N.obitos / sum(N.obitos))

dados_dia_semana_es_total <- dados_es_total %>%
  mutate(diaobito_semana = wday(DTOBITO, label = TRUE, abbr = FALSE)) %>% 
  group_by(ANOOBITO, diaobito_semana) %>% 
  summarise(N.obitos = n(), .groups = 'drop') %>% 
  group_by(ANOOBITO) %>% 
  mutate(proporcao = N.obitos / sum(N.obitos))

dados_dia_semana_es_psic <- dados_es_psic %>%
  mutate(diaobito_semana = wday(DTOBITO, label = TRUE, abbr = FALSE)) %>% 
  group_by(ANOOBITO, diaobito_semana) %>% 
  summarise(N.obitos = n(), .groups = 'drop') %>% 
  group_by(ANOOBITO) %>% 
  mutate(proporcao = N.obitos / sum(N.obitos))





# Gráficos




# BR TOTAL

grafico_dia_br_total <- ggplot(
  dados_dia_semana_br_total, aes(x = factor(ANOOBITO), 
                                 y = proporcao, 
                                 fill = diaobito_semana,
                                 text = paste("Ano: ", factor(ANOOBITO),
                                              "<br>Dia da Semana: ", diaobito_semana,
                                              "<br> Proporção: ", proporcao))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values=paleta_hist(7)) +  # Paleta de cores para os dias da semana
  labs(title = "Distribuição de Óbitos Totais no Brasil pelos Dias da Semana",
       x = "Ano",
       y = "Número de Óbitos",
       fill = "Dia da Semana") +
  theme_classic()


ggplotly(grafico_dia_br_total, tooltip = "text")

#save(grafico_dia_br_total, file="graficos/grafico_dia_br_total.RData")






# BR PSIC
grafico_dia_br_psic <- ggplot(
  dados_dia_semana_br_psic, aes(x = factor(ANOOBITO), 
                                y = proporcao, 
                                fill = diaobito_semana,
                                text = paste("Ano: ", factor(ANOOBITO),
                                             "<br>Dia da Semana: ", diaobito_semana,
                                             "<br> Proporção: ", proporcao))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values=paleta_hist(7)) +  # Paleta de cores para os dias da semana
  labs(title = "Distribuição de Óbitos por Psicoativos no Brasil pelos Dias da Semana",
       x = "Ano",
       y = "Número de Óbitos",
       fill = "Dia da Semana") +
  theme_classic()


ggplotly(grafico_dia_br_psic, tooltip = "text")

#save(grafico_dia_br_psic, file="graficos/grafico_dia_br_psic.RData")








# ES TOTAL

grafico_dia_es_total <- ggplot(
  dados_dia_semana_es_total, aes(x = factor(ANOOBITO), 
                                y = proporcao, 
                                fill = diaobito_semana,
                                text = paste("Ano: ", factor(ANOOBITO),
                                             "<br>Dia da Semana: ", diaobito_semana,
                                             "<br> Proporção: ", proporcao))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values=paleta_hist(7)) +  # Paleta de cores para os dias da semana
  labs(title = "Distribuição de Óbitos Totais no ES pelos Dias da Semana",
       x = "Ano",
       y = "Número de Óbitos",
       fill = "Dia da Semana") +
  theme_classic()


ggplotly(grafico_dia_es_total, tooltip = "text")

#save(grafico_dia_es_total, file="graficos/grafico_dia_es_total.RData")






# ES PSIC
grafico_dia_es_psic <- ggplot(
  dados_dia_semana_es_psic, aes(x = factor(ANOOBITO), 
                                y = proporcao, 
                                fill = diaobito_semana,
                                text = paste("Ano: ", factor(ANOOBITO),
                                               "<br>Dia da Semana: ", diaobito_semana,
                                               "<br> Proporção: ", proporcao))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values=paleta_hist(7)) +  # Paleta de cores para os dias da semana
  labs(title = "Distribuição de Óbitos por Psicoativos no ES pelos Dias da Semana",
       x = "Ano",
       y = "Número de Óbitos",
       fill = "Dia da Semana") +
  theme_classic()


ggplotly(grafico_dia_es_psic, tooltip = "text")

#save(grafico_dia_es_psic, file="graficos/grafico_dia_es_psic.RData")


