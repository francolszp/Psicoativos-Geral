


# MONTANDO OS DADOS INDICADOR 1

dados_indic1 <- inner_join(
  data.frame(
    dados_es_psic %>% group_by(ANOOBITO) %>%
      summarise(N.obitos = n())
  ),
  data.frame(
    dados_es_total %>%
      filter(IDADE2>17) %>%
      group_by(ANOOBITO) %>%
      summarise(N.obitos = n())
  ),

  by="ANOOBITO"

)

colnames(dados_indic1) <- c("ANOOBITO", "N_obitos_es_psic", "N_obitos_es_total")

dados_indic1$indic1 <- (N_obitos_es_psic/N_obitos_es_total)*1000


# montando o grafico
serie_indic1 <- ggplot(data = dados_indic1, 
                       aes(x = ANOOBITO, 
                           y = indic1
                           )) +
  geom_line(linetype = "solid", color = paleta_series(1),
            linewidth = 0.5)+
  geom_point(shape = 15, color = paleta_series(1), 
             aes(text = paste("Ano: ", ANOOBITO, 
                 "<br>Indicador 1: ", 
                  round(indic1, 2)))) +
  labs(title = "Série do Indicador 1 de 2013 a 2022",
       x="Anos", y="Obitos/1000") +
  scale_x_continuous(
    breaks = dados_indic1$ANOOBITO,
    labels = dados_indic1$ANOOBITO)+
  scale_y_continuous(limits = c(0,50),
      breaks = seq(0,50, by=10))+
  theme_classic()+
  theme(plot.title = element_text(size = 13),
        legend.title = element_text(size = size_titulo_legenda),               
        legend.text = element_text(size = size_texto_legenda))

ggplotly(serie_indic1, tooltip = "text")



##save(serie_indic1, file="GRAFICOS_RDA/serie_indic1.RData")


# MONTANDO OS DADOS INDICADOR 2

dados_indic2 <- inner_join(
  data.frame(
    dados_es_psic %>% group_by(ANOOBITO) %>%
      summarise(N.obitos = n())
  ),
  data.frame(
    dados_br_psic %>% group_by(ANOOBITO) %>%
      summarise(N.obitos = n())
  ),

  by="ANOOBITO"

)

colnames(dados_indic2) <- c("ANOOBITO", "N_obitos_es_psic", "N_obitos_br_psic")

attach(dados_indic2)

dados_indic2$indic2 <- (N_obitos_es_psic/N_obitos_br_psic)*100


##save(serie_indic1, file="GRAFICOS_RDA/serie_indic2.RData")



# MONTANDO OS DADOS INDICADOR 3

dados_indic3 <- inner_join(
    data.frame(
    dados_es_total %>%
      group_by(ANOOBITO) %>%
      summarise(N.obitos = n())
  ),
  data.frame(
    dados_br_total %>%
      group_by(ANOOBITO) %>%
      summarise(N.obitos = n())
  ),

  by="ANOOBITO"

)

colnames(dados_indic3) <- c("ANOOBITO", "N_obitos_es_total", "N_obitos_br_total")

attach(dados_indic3)

dados_indic3$indic3 <- (N_obitos_es_total/N_obitos_br_total)*100





#DADOS INDIC 2 e 3

dados_indic2_3 <- data.frame(
      Ano = rep(dados_indic2$ANOOBITO, 2),
      valor = c(dados_indic2$indic2,
                dados_indic3$indic3),
      Indicador = c(rep("Indicador 2", 10),
                    rep("Indicador 3", 10))
)


# montando o grafico
serie_indic2_3 <- ggplot(data = dados_indic2_3, aes(x = Ano, y = valor,
                                                    colour = Indicador)) +
  geom_line(linetype = "solid",
            linewidth = 0.5) +
  geom_point(shape = 15, 
             aes(text = paste("Ano: ", Ano, 
                              "<br> Percentual: ", 
                              round(valor, 2), "%", 
                              "<br>", Indicador))) +
  labs(title = "Indicador 2 e Indicador 3 de 2013 a 2022",
       x="Anos", y="Obitos/100") +
  scale_colour_manual(values = paleta_series(2), name = "Indicadores",
            labels = c("Indicador 2", "Indicador 3")) +
  ylim(1.5, 3.5) +
  scale_x_continuous(
    breaks = dados_indic2_3$Ano,
    labels = dados_indic2_3$Ano)+
  scale_y_continuous(limits = c(0,3.5),
                     breaks = seq(0,3.5, by=0.5))+
  theme_classic()+
  theme(legend.title = element_text(size = size_titulo_legenda),               
        legend.text = element_text(size = size_texto_legenda))

ggplotly(serie_indic2_3, tooltip = "text")

#save(serie_indic2_3, file="GRAFICOS_RDA/serie_indic2_3.RData")



### Indicador 4 obitos psic ES/pop ES
pop_es_2022 <- data.frame(codigoUF = "32",
                          ano = "2022",
                          valor  = c(3833712))

pop13a21 <- readRDS("Bases/pop13a21.rds")

dados_indic4 <- pop13a21 %>%
  dplyr::filter(codigoUF == 32) %>%
  select(codigoUF, ano, valor) %>%
  bind_rows(pop_es_2022)

dados_indic4$ano <- as.numeric(dados_indic4$ano)

dados_indic4 <- dados_indic4 %>%  inner_join(
  data.frame(
    dados_es_psic %>% group_by(ANOOBITO) %>%
      summarise(N.obitos = n())
  ),

  by = c("ano" = "ANOOBITO")
)


### Indicador 5 obitos psic BR/pop BR
pop_br_2022 <- data.frame(codigoUF = "1",
                          ano = "2022",
                          valor  = c(
                            203080756))

dados_indic5 <- pop13a21 %>%
  dplyr::filter(codigoUF == 1) %>%
  select(ano, valor, codigoUF) %>%
  rbind(pop_br_2022)

dados_indic5$ano <- as.numeric(dados_indic5$ano)

dados_indic5 <- dados_indic5 %>%  inner_join(
  data.frame(
    dados_br_psic %>% group_by(ANOOBITO) %>%
      summarise(N.obitos = n())
  ),

  by = c("ano" = "ANOOBITO")
)



### MONTANDO DADOS
dados_indic4_5 <- data.frame(
  Ano = rep(dados_indic4$ano, 2),
  valor = c(dados_indic4$valor,
            dados_indic5$valor),
  N.obitos = c(dados_indic4$N.obitos,
               dados_indic5$N.obitos),
  Indicador = c(rep("Indicador 4", 10),
                rep("Indicador 5", 10))
)


serie_indic4_5 <- ggplot(data = dados_indic4_5, aes(x = Ano, y = (N.obitos/valor)*100000,
                                                    colour = Indicador)) +
  geom_line(linetype = "solid",
            linewidth = 0.5) +
  geom_point(shape = 15, aes(text = paste("Ano: ", Ano, 
                                    "<br> Valor: ", 
                                    round((N.obitos/valor)*100000, 2), 
                                    "<br>", Indicador))) +
  labs(title = "Indicador 4 e Indicador 5 de 2013 a 2022",
       x="Anos", y="Óbitos/100000") +
  scale_colour_manual(values = paleta_series(2), name = "Indicadores",
                      labels = c("Indicador 4", "Indicador 5")) +
  #ylim(1.5, 3.5) +
  scale_x_continuous(
    breaks = dados_indic4_5$Ano,
    labels = dados_indic4_5$Ano)+
  scale_y_continuous(limits = c(0,20),
                     breaks = seq(0,20, by=2))+
  theme_classic()+
  theme(legend.title = element_text(size = size_titulo_legenda),               
        legend.text = element_text(size = size_texto_legenda))

ggplotly(serie_indic4_5, tooltip = "text")

#save(serie_indic4_5, file="GRAFICOS_RDA/serie_indic4_5.RData")

