## HORAOBITO


# BR TOTAL

dados_br_total$HORAOBITO <- as.numeric(dados_br_total$HORAOBITO)


dados_hora_br_total <- dados_br_total %>% 
  select(ANOOBITO, HORAOBITO) %>% 
  mutate(
  ANOOBITO = as.factor(ANOOBITO),
  horas = HORAOBITO %/% 100, 
  faixa_hora = cut(
    horas,
    breaks = c(-Inf, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, Inf),
    labels = c("00:00-01:59", "02:00-03:59", "04:00-05:59", "06:00-07:59", "08:00-09:59", 
               "10:00-11:59", "12:00-13:59", "14:00-15:59", "16:00-17:59", "18:00-19:59", 
               "20:00-21:59", "22:00-23:59"),
    right = FALSE)) %>% 
  group_by(ANOOBITO, faixa_hora) %>% 
  summarise(N.obitos = n(), .groups = 'drop') %>% 
  group_by(ANOOBITO) %>% 
  mutate(proporcao = N.obitos / sum(N.obitos))

  
  

# Plotando o Mapa de Calor
mapa_calor_hora_br_total <- ggplot(dados_hora_br_total, 
                                   aes(x = ANOOBITO, 
                                      y = fct_rev(fct_reorder(faixa_hora, as.numeric(substr(faixa_hora, 1, 2)))),
                                      fill = proporcao,
                                      text = paste("Ano: ", ANOOBITO,
                                                   "<br>Proporção: ", proporcao,
                                                   "<br> Faixa de Horário: ", fct_rev(fct_reorder(faixa_hora, as.numeric(substr(faixa_hora, 1, 2))))))) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "lightblue", high = "#010440",
                      name = "Proporção", limits = c(0,0.14)) +
  labs(x = "Ano", y = "Faixas de Horários (2 em 2 horas)", title = "Mapa de Calor de Óbitos por Ano e Horário no Brasil") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  )


ggplotly(mapa_calor_hora_br_total, tooltip = "text")

#save(mapa_calor_hora_br_total, file = "graficos/mapa_calor_hora_br_total.RData")







# BR PSIC

dados_br_psic$HORAOBITO <- as.numeric(dados_br_psic$HORAOBITO)


dados_hora_br_psic <- dados_br_psic %>% 
  select(ANOOBITO, HORAOBITO) %>% 
  mutate(
    ANOOBITO = as.factor(ANOOBITO),
    horas = HORAOBITO %/% 100, 
    faixa_hora = cut(
      horas,
      breaks = c(-Inf, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, Inf),
      labels = c("00:00-01:59", "02:00-03:59", "04:00-05:59", "06:00-07:59", "08:00-09:59", 
                 "10:00-11:59", "12:00-13:59", "14:00-15:59", "16:00-17:59", "18:00-19:59", 
                 "20:00-21:59", "22:00-23:59"),
      right = FALSE)) %>% 
  group_by(ANOOBITO, faixa_hora) %>% 
  summarise(N.obitos = n(), .groups = 'drop') %>% 
  group_by(ANOOBITO) %>% 
  mutate(proporcao = N.obitos / sum(N.obitos))




# Plotando o Mapa de Calor
mapa_calor_hora_br_psic <- ggplot(dados_hora_br_psic, 
                                   aes(x = ANOOBITO, 
                                       y = fct_rev(fct_reorder(faixa_hora, as.numeric(substr(faixa_hora, 1, 2)))),
                                       fill = proporcao,
                                       text = paste("Ano: ", ANOOBITO,
                                                    "<br>Proporção: ", proporcao,
                                                    "<br> Faixa de Horário: ", fct_rev(fct_reorder(faixa_hora, as.numeric(substr(faixa_hora, 1, 2))))))) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "lightblue", high = "#010440",
                      name = "Proporção", limits = c(0,0.14)) + 
  labs(x = "Ano", y = "Faixas de Horários (2 em 2 horas)", title = "Mapa de Calor de Óbitos por Psicoativos por Ano e Horário no Brasil") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  )


ggplotly(mapa_calor_hora_br_psic, tooltip = "text")


#save(mapa_calor_hora_br_psic, file = "graficos/mapa_calor_hora_br_psic.RData")


# ES TOTAL

dados_es_total$HORAOBITO <- as.numeric(dados_es_total$HORAOBITO)


dados_hora_es_total <- dados_es_total %>% 
  select(ANOOBITO, HORAOBITO) %>% 
  mutate(
    ANOOBITO = as.factor(ANOOBITO),
    horas = HORAOBITO %/% 100, 
    faixa_hora = cut(
      horas,
      breaks = c(-Inf, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, Inf),
      labels = c("00:00-01:59", "02:00-03:59", "04:00-05:59", "06:00-07:59", "08:00-09:59", 
                 "10:00-11:59", "12:00-13:59", "14:00-15:59", "16:00-17:59", "18:00-19:59", 
                 "20:00-21:59", "22:00-23:59"),
      right = FALSE)) %>% 
  group_by(ANOOBITO, faixa_hora) %>% 
  summarise(N.obitos = n(), .groups = 'drop') %>% 
  group_by(ANOOBITO) %>% 
  mutate(proporcao = N.obitos / sum(N.obitos))




# Plotando o Mapa de Calor
mapa_calor_hora_es_total <- ggplot(dados_hora_es_total, 
                                   aes(x = ANOOBITO, 
                                       y = fct_rev(fct_reorder(faixa_hora, as.numeric(substr(faixa_hora, 1, 2)))),
                                       fill = proporcao,
                                       text = paste("Ano: ", ANOOBITO,
                                                    "<br>Proporção: ", proporcao,
                                                    "<br> Faixa de Horário: ", fct_rev(fct_reorder(faixa_hora, as.numeric(substr(faixa_hora, 1, 2))))))) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "lightblue", high = "#010440",
                      name = "Proporção", limits = c(0,0.14)) +
  labs(x = "Ano", y = "Faixas de Horários (2 em 2 horas)", title = "Mapa de Calor de Óbitos por Ano e Horário no ES") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank(),
    axis.text.y = element_blank()
  )


ggplotly(mapa_calor_hora_es_total, tooltip = "text")

#save(mapa_calor_hora_es_total, file = "graficos/mapa_calor_hora_es_total.RData")









# ES PSIC

dados_es_psic$HORAOBITO <- as.numeric(dados_es_psic$HORAOBITO)


dados_hora_es_psic <- dados_es_psic %>% 
  select(ANOOBITO, HORAOBITO) %>% 
  mutate(
    ANOOBITO = as.factor(ANOOBITO),
    horas = HORAOBITO %/% 100, 
    faixa_hora = cut(
      horas,
      breaks = c(-Inf, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, Inf),
      labels = c("00:00-01:59", "02:00-03:59", "04:00-05:59", "06:00-07:59", "08:00-09:59", 
                 "10:00-11:59", "12:00-13:59", "14:00-15:59", "16:00-17:59", "18:00-19:59", 
                 "20:00-21:59", "22:00-23:59"),
      right = FALSE)) %>% 
  group_by(ANOOBITO, faixa_hora) %>% 
  summarise(N.obitos = n(), .groups = 'drop') %>% 
  group_by(ANOOBITO) %>% 
  mutate(proporcao = N.obitos / sum(N.obitos))




# Plotando o Mapa de Calor
mapa_calor_hora_es_psic <- ggplot(dados_hora_es_psic, 
                                  aes(x = ANOOBITO, 
                                      y = fct_rev(fct_reorder(faixa_hora, as.numeric(substr(faixa_hora, 1, 2)))),
                                      fill = proporcao,
                                      text = paste("Ano: ", ANOOBITO,
                                                   "<br>Proporção: ", proporcao,
                                                   "<br> Faixa de Horário: ", fct_rev(fct_reorder(faixa_hora, as.numeric(substr(faixa_hora, 1, 2))))))) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "lightblue", high = "#010440",
                      name = "Proporção", limits = c(0,0.14)) +
  labs(x = "Ano", y = "Faixas de Horários (2 em 2 horas)", title = "Mapa de Calor de Óbitos por Psicoativos por Ano e Horário no ES") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank(),
    axis.text.y = element_blank()
  )


ggplotly(mapa_calor_hora_es_psic, tooltip = "text")

#save(mapa_calor_hora_es_psic, file = "graficos/mapa_calor_hora_es_psic.RData")
