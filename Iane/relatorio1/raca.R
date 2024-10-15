#library(remotes)
#library(microdatasus)
#library(dplyr)
#library(stringr)
#library(ggplot2)
#library(lubridate)



# -------------- ANALISE RACA  -------------

#HISTOGRAMA

#1.1 quantidade de mortes por raca no brasil
freq_raca_br_total <- dados_br_total %>%
  group_by(RACACOR) %>%
  summarise(Quantidade = n(), .groups = "drop") %>%
  mutate(Porcentagem = Quantidade / sum(Quantidade) * 100)

hist_raca_br_total <- ggplot(freq_raca_br_total, aes(x = RACACOR, y = Quantidade, fill = RACACOR)) +
  geom_bar(stat = "identity") +
  theme(plot.title = element_text(size = 15),
        legend.title = element_text(size = size_titulo_legenda),
        legend.text = element_text(size = size_texto_legenda)) +
  labs(title = "Quantidades de mortes por Raça/Cor/Cor no Brasil",
       x = "Raça/Cor",
       y = "Número de Pessoas")

print(hist_raca_br_total)

#1.2 quantidade de mortes por raca no espirito santo

freq_raca_br_psic <- dados_br_psic %>%
  group_by(RACACOR) %>%
  summarise(Quantidade = n(), .groups = "drop") %>%
  mutate(Porcentagem = Quantidade / sum(Quantidade) * 100)

hist_raca_br_psic <- ggplot(freq_raca_br_psic, aes(x = RACACOR, y = Quantidade, fill = RACACOR)) +
  geom_bar(stat = "identity") +
  theme(plot.title = element_text(size = 20),
        legend.title = element_text(size = size_titulo_legenda),
        legend.text = element_text(size = size_texto_legenda)) +
  labs(title = "Quantidade de mortes por psicoativo no Brasil por Raça/Cor/Cor",
       x = "Raça/Cor",
       y = "Número de Pessoas")

print(hist_raca_br_psic)

#1.3 quantidade de mortes por psicoatvos por raca no brasil

freq_raca_es_total <- dados_es_total %>%
  group_by(RACACOR) %>%
  summarise(Quantidade = n(), .groups = "drop") %>%
  mutate(Porcentagem = Quantidade / sum(Quantidade) * 100)

hist_raca_es_total <- ggplot(freq_raca_es_total, aes(x = RACACOR, y = Quantidade, fill = RACACOR)) +
  geom_bar(stat = "identity") +
  theme(plot.title = element_text(size = 15),
        legend.title = element_text(size = size_titulo_legenda),
        legend.text = element_text(size = size_texto_legenda)) +
  labs(title = "Quantidade de mortes por Raça/Cor/Cor no ES",
       x = "Raça/Cor",
       y = "Número de Pessoas")

print(hist_raca_es_total)

#1.4 quantidade de mortes por psicoativos por raca no espirito santo

freq_raca_es_psic <- dados_es_psic %>%
  group_by(RACACOR) %>%
  summarise(Quantidade = n(), .groups = "drop") %>%
  mutate(Porcentagem = Quantidade / sum(Quantidade) * 100)

hist_raca_es_psic <- ggplot(freq_raca_es_psic, aes(x = RACACOR, y = Quantidade, fill = RACACOR)) +
  geom_bar(stat = "identity") +
  theme(plot.title = element_text(size = 15),
        legend.title = element_text(size = size_titulo_legenda),
        legend.text = element_text(size = size_texto_legenda)) +
  labs(title = "Quantidade de mortes por psicoativo no ES por Raça/Cor/Cor",
       x = "Raça/Cor",
       y = "Número de Pessoas")

print(hist_raca_es_psic)






#SERIE

#SERIE

#2.1 quantidade de mortes por raca no brasil

# montando os dados
dados.raca.br.series <- data.frame(
  dados_br_total %>% group_by(ANOOBITO, RACACOR) %>%
    summarise(N.obitos = n())
)

# grafico
series_raca_br_total <- ggplot(data = dados.raca.br.series, aes(x = ANOOBITO, y = N.obitos,
                                                                    colour = RACACOR, group = RACACOR,
                                                                text  = paste("Ano: ", ANOOBITO,
                                                                              "<br>Quantidade: ", N.obitos,
                                                                              "<br>Raça/Cor: ", RACACOR))) +
  geom_line(linewidth = 0.5, linetype = "solid") +
  geom_point(shape = 15, aes(colour = RACACOR,
                             text  = paste("Ano: ", ANOOBITO,
                                           "<br>Quantidade: ", N.obitos,
                                           "<br>Raça/Cor: ", RACACOR))) +
  scale_colour_manual(values = rev(paleta_series(5)))+
  labs(title = "Número de óbitos Totais no Brasil por Raça/Cor",
       x="Anos", y="Óbitos Totais", colour = "Raça/Cor") +
  scale_x_continuous(
    breaks = dados.raca.br.series$ANOOBITO,
    labels = dados.raca.br.series$ANOOBITO)+
  theme_classic()+
  theme(plot.title = element_text(size = 15),
        legend.title = element_text(size = size_titulo_legenda),
        legend.text = element_text(size = size_texto_legenda))

print (series_raca_br_total)

save(series_raca_br_total, file="GRAFICOS_RDA/series_raca_br_total.RData")
#2.2 quantidade de mortes por Raça/Cor/Cor no espirito santo

# montando os dados
dados.raca.es.series <- data.frame(
  dados_es_total %>% group_by(ANOOBITO, RACACOR) %>%
    summarise(N.obitos = n())
)

# grafico
series_raca_es_total <- ggplot(data = dados.raca.es.series,
                               aes(x = ANOOBITO, y = N.obitos,
                                   colour = RACACOR, group = RACACOR,
                                   text  = paste("Ano: ", ANOOBITO,
                                                 "<br>Quantidade: ", N.obitos,
                                                 "<br>Raça/Cor: ", RACACOR))) +
  geom_line(linewidth = 0.5, linetype = "solid") +
  geom_point(shape = 15) +
  scale_colour_manual(values = rev(paleta_series(5)))+
  labs(title = "Número de óbitos Totais no ES por Raça/Cor",
       x="Anos", y="Óbitos Totais", colour = "Raça/Cor") +
  scale_x_continuous(
    breaks = dados.raca.es.series$ANOOBITO,
    labels = dados.raca.es.series$ANOOBITO)+
  theme_classic()+
  theme(plot.title = element_text(size = 15),
        legend.title = element_text(size = size_titulo_legenda),
        legend.text = element_text(size = size_texto_legenda))

print (series_raca_es_total)

save(series_raca_es_total, file="GRAFICOS_RDA/series_raca_es_total.RData")
#2.1 quantidade de mortes por psicoativos por raca no brasil

# montando os dados
dados.raca.br.series.psic <- data.frame(
  dados_br_psic %>% group_by(ANOOBITO, RACACOR) %>%
    summarise(N.obitos = n())
)
# grafico
series_raca_br_psic <- ggplot(data = dados.raca.br.series.psic, aes(x = ANOOBITO, y = N.obitos,
                                                                        colour = RACACOR, group = RACACOR,text  = paste("Ano: ", ANOOBITO,
                                                                                                                        "<br>Quantidade: ", N.obitos,
                                                                                                                        "<br>Raça/Cor: ", RACACOR))) +
  geom_line(linewidth = 0.5, linetype = "solid") +
  geom_point(shape = 15) +
  scale_colour_manual(values = rev(paleta_series(5)))+
  labs(title = "Número de óbitos por psicativos no Brasil por Raça/Cor",
       x="Anos", y="Óbitos por psicoativos", colour = "Raça/Cor") +
  scale_x_continuous(
    breaks = dados.raca.br.series.psic$ANOOBITO,
    labels = dados.raca.br.series.psic$ANOOBITO)+
  theme_classic()+
  theme(plot.title = element_text(size = 15),
        legend.title = element_text(size = size_titulo_legenda),
        legend.text = element_text(size = size_texto_legenda))

print (series_raca_br_psic)

save(series_raca_br_psic, file="GRAFICOS_RDA/series_raca_br_psic.RData")
#2.2 quantidade de mortes por psicoativos por raca no espirito santo

# montando os dados
dados.raca.es.series.psic <- data.frame(
  dados_es_psic %>% group_by(ANOOBITO, RACACOR) %>%
    summarise(N.obitos = n())
)
# grafico
series_raca_es_psic <- ggplot(data = dados.raca.es.series.psic, aes(x = ANOOBITO, y = N.obitos,
                                                                        colour = RACACOR, group = RACACOR,
                                                                    text  = paste("Ano: ", ANOOBITO,
                                                                                  "<br>Quantidade: ", N.obitos,
                                                                                  "<br>Raça/Cor: ", RACACOR))) +
  geom_line(linewidth = 0.5, linetype = "solid") +
  geom_point(shape = 15) +
  scale_colour_manual(values = rev(paleta_series(5)))+
  labs(title = "Número de óbitos por psicoativos no ES por Raça/Cor/Cor",
       x="Anos", y="Óbitos por psicoativos", colour = "Raça/Cor") +
  scale_x_continuous(
    breaks = dados.raca.es.series.psic$ANOOBITO,
    labels = dados.raca.es.series.psic$ANOOBITO)+
  theme_classic()+
  theme(plot.title = element_text(size = 15),
        legend.title = element_text(size = size_titulo_legenda),
        legend.text = element_text(size = size_texto_legenda))

print (series_raca_es_psic)

save(series_raca_es_psic, file="GRAFICOS_RDA/series_raca_es_psic.RData")

#GRAFICO dE PROPORCAO Brasil

#1 Criando coluna de proporC'C#o

dados.raca.br.series.psic <- dados.raca.br.series.psic %>%
  group_by(ANOOBITO) %>%
  mutate(total_mortes = sum(N.obitos),
         porcentagem = (N.obitos / total_mortes) * 100) %>%
  ungroup()
ordem_cores <- c("Branca", "Preta", "Amarela", "Parda", "Indígena")

# Converta RACACOR para um fator com a ordem desejada
dados.raca.br.series.psic$RACACOR <- factor(dados.raca.br.series.psic$RACACOR , levels = ordem_cores)

#2 Criar o grC!fico
proporcao_raca_br_psic <- ggplot(dados.raca.br.series.psic,
                                 aes(x = factor(ANOOBITO),

                                     y = porcentagem,
                                     fill = RACACOR,
                                     text  = paste("Ano: ",
                                                   factor(ANOOBITO),
                                                   "<br>Quantidade: ", N.obitos,
                                                   "<br>Raça/Cor: ", RACACOR))) +
  geom_bar(stat = "identity",
           position = "stack",
           ) +
  scale_fill_manual(values = rev(paleta_hist(5)))+
  labs(x = "Ano", y = "Percentual (%)", fill = "Raca",
       title = "Percentual de Mortes por Psicoativos no BR por Raça/Cor e Ano") +
  theme_classic()+
  theme(plot.title = element_text(size = 14),
        legend.title = element_text(size = size_titulo_legenda),
        legend.text = element_text(size = size_texto_legenda))

print(proporcao_raca_br_psic)


save(proporcao_raca_br_psic, file="GRAFICOS_RDA/proporcao_raca_br_psic.RData")

#GRAFICOD E PROPORCAO ES

#1 Criando coluna de proporC'C#o

dados.raca.es.series.psic <- dados.raca.es.series.psic %>%
  group_by(ANOOBITO) %>%
  mutate(total_mortes = sum(N.obitos),
         porcentagem = (N.obitos / total_mortes) * 100) %>%
  ungroup()
ordem_cores <- c("Branca", "Preta", "Amarela", "Parda", "Indígena")

# Converta RACACOR para um fator com a ordem desejada
dados.raca.es.series.psic$RACACOR <- factor(dados.raca.es.series.psic$RACACOR , levels = ordem_cores)

#2 Criar o grC!fico
proporcao_raca_es_psic <- ggplot(dados.raca.es.series.psic, aes(x = factor(ANOOBITO), y = porcentagem, fill = RACACOR,text  = paste("Ano: ", factor(ANOOBITO),
                                                                                                                                    "<br>", N.obitos,
                                                                                                                                    "<br>Raça/Cor: ", RACACOR))) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = rev(paleta_hist(5)))+
  labs(x = "Ano", y = "Percentual (%)", fill = "Raça/Cor",
       title = "Percentual de Mortes por Psicoativos no ES por Raça/Cor e Ano") +
  theme_classic()+
  theme(plot.title = element_text(size = 14),
        legend.title = element_text(size = size_titulo_legenda),
        legend.text = element_text(size = size_texto_legenda))

print(proporcao_raca_es_psic)

save(proporcao_raca_es_psic, file="GRAFICOS_RDA/proporcao_raca_es_psic.RData")


