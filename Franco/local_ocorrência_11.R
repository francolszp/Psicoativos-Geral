
## LOCOCOR

dados_local_es_series <- data.frame(
  dados_es_total %>% group_by(ANOOBITO, LOCOCOR) %>%
    summarise(N.obitos = n())
)

dados_local_br_series <- data.frame(
  dados_br_total %>% group_by(ANOOBITO, LOCOCOR) %>%
    summarise(N.obitos = n()) %>% 
    mutate(LOCOCOR = case_when(
      LOCOCOR == 6 ~ "Aldeia Indígena",
      TRUE ~ LOCOCOR
    ))
)

dados_local_es_psic_series <- data.frame(
  dados_es_psic %>% group_by(ANOOBITO, LOCOCOR) %>%
    summarise(N.obitos = n())
)

dados_local_br_psic_series <- data.frame(
  dados_br_psic %>% group_by(ANOOBITO, LOCOCOR) %>%
    summarise(N.obitos = n())%>% 
    mutate(LOCOCOR = case_when(
      LOCOCOR == 6 ~ "Aldeia Indígena",
      TRUE ~ LOCOCOR
    ))
)



## Gráficos 


# BR TOTAL
series_local_br_total<- ggplot(data = dados_local_br_series, 
                                       aes(x = ANOOBITO, y = N.obitos/1000,
                                                    colour = LOCOCOR)) +
  geom_line(linewidth = 0.5, linetype = "solid", show.legend = F) +
  geom_point(shape = 15, aes(colour = LOCOCOR,
                             text  = paste("Ano: ", ANOOBITO,
                                           "<br>Quantidade: ", N.obitos/1000,
                                           "<br>Local de Ocorrência: ", LOCOCOR))) +
  scale_colour_manual(values =  paleta_lococor)+
  labs(title = "Número de Óbitos Totais no Brasil por Local de Ocorrência",
       x="Anos", y="Óbitos Totais por 1000 habitantes", colour = "") +
  scale_x_continuous(
    breaks = dados_local_br_series$ANOOBITO,
    labels = dados_local_br_series$ANOOBITO)+
  theme_classic()+
  theme(plot.title = element_text(size = 15),
        legend.title = element_text(size = size_titulo_legenda),
        legend.text = element_text(size = size_texto_legenda))



ggplotly(series_local_br_total,  tooltip = "text")

#save(series_local_br_total, file="graficos/series_local_br_total.RData")


# ES TOTAL
series_local_es_total<- ggplot(data = dados_local_es_series, 
                               aes(x = ANOOBITO, y = N.obitos,
                                   colour = LOCOCOR)) +
  geom_line(linewidth = 0.5, linetype = "solid") +
  geom_point(shape = 15, aes(colour = LOCOCOR,
                             text  = paste("Ano: ", ANOOBITO,
                                           "<br>Quantidade: ", N.obitos,
                                           "<br>Local de Ocorrência: ", LOCOCOR))) +
  scale_colour_manual(values =  paleta_lococor)+
  labs(title = "Número de Óbitos Totais no ES por Local de Ocorrência",
       x="Anos", y="Óbitos Totais", colour = "") +
  scale_x_continuous(
    breaks = dados_local_es_series$ANOOBITO,
    labels = dados_local_es_series$ANOOBITO)+
  theme_classic()+
  theme(plot.title = element_text(size = 15),
        legend.title = element_text(size = size_titulo_legenda),
        legend.text = element_text(size = size_texto_legenda))


ggplotly(series_local_es_total,  tooltip = "text")

#save(series_local_es_total, file="graficos/series_local_es_total.RData")


# BR PSIC
series_local_br_psic<- ggplot(data = dados_local_br_psic_series, 
                               aes(x = ANOOBITO, y = N.obitos,
                                   colour = LOCOCOR)) +
  geom_line(linewidth = 0.5, linetype = "solid") +
  geom_point(shape = 15, aes(colour = LOCOCOR,
                             text  = paste("Ano: ", ANOOBITO,
                                           "<br>Quantidade: ", N.obitos,
                                           "<br>Local de Ocorrência: ", LOCOCOR))) +
  scale_colour_manual(values =  paleta_lococor)+
  labs(title = "Número de Óbitos por Psicoativos no Brasil por Local de Ocorrência",
       x="Anos", y="Óbitos Totais", colour = "") +
  scale_x_continuous(
    breaks = dados_local_br_psic_series$ANOOBITO,
    labels = dados_local_br_psic_series$ANOOBITO)+
  theme_classic()+
  theme(plot.title = element_text(size = 15),
        legend.title = element_text(size = size_titulo_legenda),
        legend.text = element_text(size = size_texto_legenda))


ggplotly(series_local_br_psic,  tooltip = "text")

#save(series_local_br_psic, file="graficos/series_local_br_psic.RData")


# ES PSIC
series_local_es_psic<- ggplot(data = dados_local_es_psic_series, 
                              aes(x = ANOOBITO, y = N.obitos,
                                  colour = LOCOCOR)) +
  geom_line(linewidth = 0.5, linetype = "solid") +
  geom_point(shape = 15, aes(colour = LOCOCOR,
                             text  = paste("Ano: ", ANOOBITO,
                                           "<br>Quantidade: ", N.obitos,
                                           "<br>Local de Ocorrência: ", LOCOCOR))) +
  scale_colour_manual(values =  paleta_lococor)+
  labs(title = "Número de Óbitos por Psicoativos no ES por Local de Ocorrência",
       x="Anos", y="Óbitos Totais", colour = "") +
  scale_x_continuous(
    breaks = dados_local_es_psic_series$ANOOBITO,
    labels = dados_local_es_psic_series$ANOOBITO)+
  theme_classic()+
  theme(plot.title = element_text(size = 15),
        legend.title = element_text(size = size_titulo_legenda),
        legend.text = element_text(size = size_texto_legenda))


ggplotly(series_local_es_psic,  tooltip = "text")

#save(series_local_es_psic, file="graficos/series_local_es_psic.RData")


