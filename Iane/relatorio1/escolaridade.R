#library(remotes)
#library(microdatasus)
#library(dplyr)
#library(stringr)
#library(ggplot2)
#library(lubridate)



# -------------- ANALISE ESCOLARIDADE  -------------

#HISTOGRAMA

#3.1 quantidade de mortes por escolaridade no brasil
freq_escolaridade_br_total <- dados_br_total %>%
  group_by(ESC) %>%
  summarise(Quantidade = n(), .groups = "drop") %>%
  mutate(Porcentagem = Quantidade / sum(Quantidade) * 100)

hist_escolaridade_br_total <- ggplot(freq_escolaridade_br_total, aes(x = ESC, y = Quantidade, fill = ESC)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values=paleta_hist_ordinal(5))+
  theme(plot.title = element_text(size = 15),
        legend.title = element_text(size = size_titulo_legenda),
        legend.text = element_text(size = size_texto_legenda)) +
  labs(title = "Quantidades de mortes totais no Brasil por Escolaridade",
       x = "Escolaridade",
       y = "Número de Pessoas")


print(hist_escolaridade_br_total)

#3.2 quantidade de mortes por escolaridade no espirito santo

freq_escolaridade_br_psic <- dados_br_psic %>%
  group_by(ESC) %>%
  summarise(Quantidade = n(), .groups = "drop") %>%
  mutate(Porcentagem = Quantidade / sum(Quantidade) * 100)

hist_escolaridade_br_psic <- ggplot(freq_escolaridade_br_psic, aes(x = ESC, y = Quantidade, fill = ESC)) +
  geom_bar(stat = "identity") +
  theme(plot.title = element_text(size = 20),
        legend.title = element_text(size = size_titulo_legenda),
              legend.text = element_text(size = size_texto_legenda)
  ) +
  scale_fill_manual(values=paleta_hist_ordinal(5))+
  labs(title = "Quantidades de mortes por psicoativo no Brasil por Escolaridade",
       x = "Escolaridade",
       y = "Número de Pessoas")

print(hist_escolaridade_br_psic)

#3.3 quantidade de mortes por psicoatvos por escolaridade no brasil

freq_escolaridade_es_total <- dados_es_total %>%
  group_by(ESC) %>%
  summarise(Quantidade = n(), .groups = "drop") %>%
  mutate(Porcentagem = Quantidade / sum(Quantidade) * 100)

hist_escolaridade_es_total <- ggplot(freq_escolaridade_es_total, aes(x = ESC, y = Quantidade, fill = ESC)) +
  geom_bar(stat = "identity") +
  theme(plot.title = element_text(size = 15)) +
  scale_fill_manual(values=paleta_hist_ordinal(5))+
  labs(title = "Quantidades de mortes totais no Espirito Santo por Escolaridade",
       x = "Escolaridade",
       y = "Número de Pessoas")

print(hist_escolaridade_es_total)

#3.4 quantidade de mortes por psicoativos por escolaridade no espirito santo

freq_escolaridade_es_psic <- dados_es_psic %>%
  group_by(ESC) %>%
  summarise(Quantidade = n(), .groups = "drop") %>%
  mutate(Porcentagem = Quantidade / sum(Quantidade) * 100)

hist_escolaridade_es_psic <- ggplot(freq_escolaridade_es_psic, aes(x = ESC, y = Quantidade, fill = ESC)) +
  geom_bar(stat = "identity") +
  theme(plot.title = element_text(size = 15),
        legend.title = element_text(size = size_titulo_legenda),
        legend.text = element_text(size = size_texto_legenda)) +
  scale_fill_manual(values=paleta_hist_ordinal(5))+
  labs(title = "Quantidades de mortes por psicoativo no ES por Escolaridade",
       x = "Escolaridade",
       y = "Número de Pessoas")

print(hist_escolaridade_es_psic)







#SERIE

#2.2 quantidade de mortes TOTAIS por escolaridade no Brasil

# montando os dados
dados.escolaridade.br.series <- data.frame(
  dados_br_total %>% group_by(ANOOBITO, ESC) %>%
    summarise(N.obitos = n())
)

# grafico
series_escolaridade_br_total <- ggplot(data = dados.escolaridade.br.series, aes(x = ANOOBITO, y = N.obitos,
                                                                                colour = ESC)) +
  geom_line(linewidth = 0.5, linetype = "solid") +
  geom_point(shape = 15, aes(colour = ESC,
                             text  = paste("Ano: ", ANOOBITO,
                                           "<br>Quantidade: ", N.obitos,
                                           "<br>Escolaridade: ", ESC))) +
  scale_colour_manual(values =  paleta_series(5))+
  labs(title = "Número de Óbitos Totais no Brasil por Escolaridade",
       x="Anos", y="Óbitos Totais", colour = "Escolaridade") +
  scale_x_continuous(
    breaks = dados.escolaridade.br.series$ANOOBITO,
    labels = dados.escolaridade.br.series$ANOOBITO)+
  theme_classic()+
  theme(plot.title = element_text(size = 15),
        legend.title = element_text(size = size_titulo_legenda),
        legend.text = element_text(size = size_texto_legenda))

print (series_escolaridade_br_total)

save(series_escolaridade_br_total, file="GRAFICOS_RDA/series_escolaridade_br_total.RData")

#2.2 quantidade de mortes TOTAIS por escolaridade no espirito santo

# montando os dados
dados.escolaridade.es.series <- data.frame(
  dados_es_total %>% group_by(ANOOBITO, ESC) %>%
    summarise(N.obitos = n())
)

# grafico
series_escolaridade_es_total <- ggplot(data = dados.escolaridade.es.series, aes(x = ANOOBITO, y = N.obitos,
                                                                    colour = ESC)) +
  geom_line(linewidth = 0.5, linetype = "solid") +
  geom_point(shape = 15, aes(colour = ESC,
                             text  = paste("Ano: ", ANOOBITO,
                                           "<br>Quantidade: ", N.obitos,
                                           "<br>Escolaridade: ", ESC))) +
  scale_colour_manual(values =  paleta_series(5))+
  labs(title = "Número de Óbitos Totais no ES por Escolaridade",
       x="Anos", y="Óbitos Totais", colour = "Escolaridade") +
  scale_x_continuous(
    breaks = dados.escolaridade.es.series$ANOOBITO,
    labels = dados.escolaridade.es.series$ANOOBITO)+
  theme_classic()+
  theme(plot.title = element_text(size = 15),
        legend.title = element_text(size = size_titulo_legenda),
        legend.text = element_text(size = size_texto_legenda))

print (series_escolaridade_es_total)


save(series_escolaridade_es_total, file="GRAFICOS_RDA/series_escolaridade_es_total.RData")

#2.1 quantidade de mortes por psicoativos por escolaridade no brasil

# montando os dados
dados.escolaridade.br.series.psic <- data.frame(
  dados_br_psic %>% group_by(ANOOBITO, ESC) %>%
    summarise(N.obitos = n())
)

# grafico
series_escolaridade_br_psic <- ggplot(data = dados.escolaridade.br.series.psic, aes(x = ANOOBITO, y = N.obitos,
                                                                        colour = ESC)) +
  geom_line(linewidth = 0.5, linetype = "solid") +
  geom_point(shape = 15, aes(colour = ESC,
                             text  = paste("Ano: ", ANOOBITO,
                                           "<br>Quantidade: ", N.obitos,
                                           "<br>Escolaridade: ", ESC))) +
  scale_colour_manual(values =  paleta_series(5))+
  labs(title = "Número de Óbitos por psicoativos no Brasil por Escolaridade",
       x="Anos", y="Óbitos por psicoativos", colour = "Escolaridade") +
  scale_x_continuous(
    breaks = dados.escolaridade.br.series.psic$ANOOBITO,
    labels = dados.escolaridade.br.series.psic$ANOOBITO)+
  theme_classic()+
  theme(plot.title = element_text(size = 13),
        legend.title = element_text(size = size_titulo_legenda),
        legend.text = element_text(size = size_texto_legenda))

print (series_escolaridade_br_psic)

save(series_escolaridade_br_psic, file="GRAFICOS_RDA/series_escolaridade_br_psic.RData")

#2.1 quantidade de mortes por psicoativos por escolaridade no espirito santo

# montando os dados
dados.escolaridade.es.series.psic <- data.frame(
  dados_es_psic %>% group_by(ANOOBITO, ESC) %>%
    summarise(N.obitos = n())
)

# grafico
series_escolaridade_es_psic <- ggplot(data = dados.escolaridade.es.series.psic, aes(x = ANOOBITO, y = N.obitos,
                                                                                    colour = ESC)) +
  geom_line(linewidth = 0.5, linetype = "solid") +
  geom_point(shape = 15, aes(colour = ESC,
                             text  = paste("Ano: ", ANOOBITO,
                                           "<br>Quantidade: ", N.obitos,
                                           "<br>Escolaridade: ", ESC))) +
  scale_colour_manual(values =  paleta_series(5))+
  labs(title = "Número de óbitos por psicoativos no Espirito Santo por Escolaridade",
       x="Anos", y="Óbitos por psicoativos", colour = "Escolaridade") +
  scale_x_continuous(
    breaks = dados.escolaridade.es.series.psic$ANOOBITO,
    labels = dados.escolaridade.es.series.psic$ANOOBITO)+
  theme_classic()+
  theme(plot.title = element_text(size = 13),
        legend.title = element_text(size = size_titulo_legenda),
        legend.text = element_text(size = size_texto_legenda))

print (series_escolaridade_es_psic)


save(series_escolaridade_es_psic, file="GRAFICOS_RDA/series_escolaridade_es_psic.RData")
#GRAFICO DE PROPORCAO Brasil


# 1 Definindo a ordem dos níveis de escolaridade
levels_escolaridade <- c("Nenhuma", "1 a 3 anos", "4 a 7 anos", "8 a 11 anos", "12 anos ou mais")

dados.escolaridade.br.series.psic$ESC <- factor(dados.escolaridade.br.series.psic$ESC, levels = levels_escolaridade)

# 2 Criando coluna de proporção
dados.escolaridade.br.series.psic <- dados.escolaridade.br.series.psic %>%
  group_by(ANOOBITO) %>%
  mutate(total_mortes = sum(N.obitos),
         porcentagem = (N.obitos / total_mortes) * 100) %>%
  ungroup()

# 3 Criando o gráfico
proporcao_escolaridade_br_psic <-ggplot(dados.escolaridade.br.series.psic, aes(x = factor(ANOOBITO), y = porcentagem, fill = ESC,text  = paste("Ano: ", ANOOBITO,
                                                                                                                                               "<br>Quantidade: ", N.obitos,
                                                                                                                                               "<br>Escolaridade: "))) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values =  (paleta_hist(6)))+
  labs(x = "Ano", y = "Porcentagem (%)", fill = "Escolaridade",
       title = "Proporção de Mortes por Psicoativos no Brasil por Escolaridade e Ano") +
  theme_classic()+
  theme(plot.title = element_text(size = 14),
        axis.text = element_text(size = 13),
        legend.title = element_text(size = size_titulo_legenda),
        legend.text = element_text(size = size_texto_legenda))

print(proporcao_escolaridade_br_psic)

save(proporcao_escolaridade_br_psic, file="GRAFICOS_RDA/proporcao_escolaridade_br_psic.RData")

#GRAFICO DE PROPORCAO ES


# 1 Definindo a ordem dos níveis de escolaridade
levels_escolaridade <- c("Nenhuma", "1 a 3 anos", "4 a 7 anos", "8 a 11 anos", "12 anos ou mais")

dados.escolaridade.es.series.psic$ESC <- factor(dados.escolaridade.es.series.psic$ESC, levels = levels_escolaridade)

# 2 Criando coluna de proporção
dados.escolaridade.es.series.psic <- dados.escolaridade.es.series.psic %>%
  group_by(ANOOBITO) %>%
  mutate(total_mortes = sum(N.obitos),
         porcentagem = (N.obitos / total_mortes) * 100) %>%
  ungroup()

# 3 Criando o gráfico
proporcao_escolaridade_es_psic <- ggplot(dados.escolaridade.es.series.psic, aes(x = factor(ANOOBITO), y = porcentagem, fill = ESC,text  = paste("Ano: ", ANOOBITO,
                                                                                                                                                "<br>Quantidade: ", N.obitos,
                                                                                                                                                "<br>Escolaridade: ", ESC))) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values =  (paleta_hist(5)))+
  labs(x = "Ano", y = "Porcentagem (%)", fill = "Escolaridade",
       title = "Proporção de Mortes por Psicoativos no ES por Escolaridade e Ano") +
  theme_classic()+
  theme(plot.title = element_text(size = 14),
        axis.text = element_text(size = 13),
        legend.title = element_text(size = size_titulo_legenda),
        legend.text = element_text(size = size_texto_legenda))

print(proporcao_escolaridade_es_psic)

save(proporcao_escolaridade_es_psic, file="GRAFICOS_RDA/proporcao_escolaridade_es_psic.RData")

