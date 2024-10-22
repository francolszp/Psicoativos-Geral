library(remotes)
library(microdatasus)
library(dplyr)
library(stringr)
library(ggplot2)
library(lubridate)


# -------------- ANALISE MORTES TOTAIS  -------------

#HISTOGRAMA

# BR TOTAIS

# agrupamento pelo ano do obito

freq_obito_br_totais <- dados_br_total %>%
  group_by(ANOOBITO) %>%
  summarise(Quantidade = n(), .groups = "drop") %>%
  mutate(Porcentagem = Quantidade / sum(Quantidade) * 100)

hist_obito_br_totais <- ggplot(freq_obito_br_totais, aes(x = ANOOBITO, y = Quantidade, fill = ANOOBITO)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Mortes Totais no Brasil de 2013 a 2022",
       x = "Ano",
       y = "Óbitos")+
  theme(legend.title = element_text(size = size_titulo_legenda),               
        legend.text = element_text(size = size_texto_legenda))


print(hist_obito_br_totais)


# BR PSICOATIVOS

freq_obito_br_psic <- dados_br_psic %>%
  group_by(ANOOBITO) %>%
  summarise(Quantidade = n(), .groups = "drop") %>%
  mutate(Porcentagem = Quantidade / sum(Quantidade) * 100)

hist_obito_br_psic <- ggplot(freq_obito_br_psic, aes(x = ANOOBITO, y = Quantidade, fill = ANOOBITO)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Mortes por Psicoativos no Brasil de 2013 a 2022",
       x = "Ano",
       y = "Óbitos")+
  theme(legend.title = element_text(size = size_titulo_legenda),               
        legend.text = element_text(size = size_texto_legenda))


print(hist_obito_br_psic)


# ES TOTAIS

freq_obito_es_totais <- dados_es_total %>%
  group_by(ANOOBITO) %>%
  summarise(Quantidade = n(), .groups = "drop") %>%
  mutate(Porcentagem = Quantidade / sum(Quantidade) * 100)

hist_obito_es_totais <- ggplot(freq_obito_es_totais, aes(x = ANOOBITO, y = Quantidade, fill = ANOOBITO)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Mortes Totais no Espírito Santo de 2013 a 2022",
       x = "Ano",
       y = "Óbitos")+
  theme(legend.title = element_text(size = size_titulo_legenda),               
        legend.text = element_text(size = size_texto_legenda))


print(hist_obito_es_totais)



#ES PSICOATIVOS

freq_obito_es_psic <- dados_es_psic %>%
  group_by(ANOOBITO) %>%
  summarise(Quantidade = n(), .groups = "drop") %>%
  mutate(Porcentagem = Quantidade / sum(Quantidade) * 100)

hist_obito_es_psic <- ggplot(freq_obito_es_psic, aes(x = ANOOBITO, y = Quantidade, fill = ANOOBITO)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Mortes por Psicoativos no Espírito Santo de 2013 a 2022",
       x = "Ano",
       y = "Óbitos")+
  theme(legend.title = element_text(size = size_titulo_legenda),               
        legend.text = element_text(size = size_texto_legenda))


print(hist_obito_es_psic)





#GRAFICO DE PROPORCAO

# Juntando as duas bases de dados por ano
obitos_combinados_br_es <- freq_obito_br_psic %>%
  rename(Quantidade_BR = Quantidade) %>%
  inner_join(freq_obito_es_psic %>% rename(Quantidade_ES = Quantidade), by = "ANOOBITO")

# Calculando as proporções de mortes no ES e no restante do Brasil
obitos_combinados_br_es <- obitos_combinados_br_es %>%
  mutate(Quantidade_Restante_BR = Quantidade_BR - Quantidade_ES,
         total_mortes = Quantidade_BR,
         porcentagem_ES = (Quantidade_ES / total_mortes) * 100,
         porcentagem_Restante_BR = (Quantidade_Restante_BR / total_mortes) * 100) %>%
  select(ANOOBITO, porcentagem_ES, porcentagem_Restante_BR) %>%
  pivot_longer(cols = c(porcentagem_ES, porcentagem_Restante_BR),
               names_to = "Regiao",
               values_to = "Porcentagem")

Region<- c("Espírito Santo", "Brasil")

# Plotando o gráfico de proporção
grafico_proporcao <- ggplot(obitos_combinados_br_es, aes(x = factor(ANOOBITO), y = Porcentagem, fill = Regiao)) +
  geom_bar(stat = "identity", position = "stack", aes(text  = paste("Ano: ", factor(ANOOBITO), 
                                                                    "<br>Percentual: ", round(Porcentagem, 2), "%",
                                                                    "<br>Região: ", Regiao))) +
  labs(x = "Ano", 
       y = "Percentual (%)", 
       fill = "Região",
       title = "% de Mortes por Psicoativos no ES em relação ao Restante do BR anualmente") +
  theme_minimal() +
  scale_fill_manual(values = rev(paleta_hist(2)), labels = c("Espírito Santo", "Restante do Brasil")) +
  theme(legend.title = element_text(size = size_titulo_legenda),               
        legend.text = element_text(size = size_texto_legenda))


# Mostrar o gráfico
print(grafico_proporcao)

#ggplotly(grafico_proporcao, tooltip = "text")
#save(grafico_proporcao, file = "GRAFICOS_RDA/grafico_proporcao.RData")


#SERIE


# juntando os dados

dados.mortes.totais.series <- data.frame(
  dados_br_total %>% group_by(ANOOBITO) %>%
    summarise(N.obitos = n())
)

# grafico

series_mortes_br_total <- ggplot(data = dados.mortes.totais.series, aes(x = ANOOBITO, y = N.obitos)) +
  geom_line(linetype = "solid",
            linewidth = 0.5, color = paleta_series(1)) +
  geom_point(shape = 15, aes(text  = paste("Ano: ", ANOOBITO, 
                                           "<br>Quantidade: ", N.obitos))) +
  labs(title = "Óbitos Totais no Brasil de 2013 a 2022",
       x="Anos", y="Óbitos Totais") +
  scale_x_continuous(
    breaks = dados.mortes.totais.series$ANOOBITO,
    labels = dados.mortes.totais.series$ANOOBITO)+
  theme_classic()+
  theme(legend.title = element_text(size = size_titulo_legenda),               
        legend.text = element_text(size = size_texto_legenda))

series_mortes_br_total


#save(series_mortes_br_total, file = "GRAFICOS_RDA/series_mortes_br_total.RData")

#2.1 quantidade de mortes psic no brasil

# juntando os dados

dados.mortes.psic.series <- data.frame(
  dados_br_psic %>% group_by(ANOOBITO) %>%
    summarise(N.obitos = n())
)

# grafico

series_mortes_br_psic <- ggplot(data = dados.mortes.psic.series, aes(x = ANOOBITO, y = N.obitos)) +
  geom_line(linetype = "solid" ,color = paleta_series(1),
            linewidth = 0.5) +
  geom_point(shape = 15, aes(text  = paste("Ano: ", ANOOBITO, 
                                           "<br>Quantidade: ", N.obitos))) +
  labs(title = "Óbitos por Psicoativos no Brasil de 2013 a 2022",
       x="Anos", y="Óbitos Totais") +
  scale_x_continuous(
    breaks = dados.mortes.psic.series$ANOOBITO,
    labels = dados.mortes.psic.series$ANOOBITO)+
  theme_classic()+
  theme(legend.title = element_text(size = size_titulo_legenda),               
        legend.text = element_text(size = size_texto_legenda))

series_mortes_br_psic


#save(series_mortes_br_psic, file = "GRAFICOS_RDA/series_mortes_br_psic.RData")


#2.1 quantidade de mortes totais no es

# juntando os dados

dados.mortes.es.totais.series <- data.frame(
  dados_es_total %>% group_by(ANOOBITO) %>%
    summarise(N.obitos = n())
)

# grafico

series_mortes_es_total <- ggplot(data = dados.mortes.es.totais.series, aes(x = ANOOBITO, y = N.obitos)) +
  geom_line(linetype = "solid" ,color = paleta_series(1),
            linewidth = 0.5) +
  geom_point(shape = 15, aes(text  = paste("Ano: ", ANOOBITO, 
                                           "<br>Quantidade: ", N.obitos))) +
  labs(title = "Número de Óbitos Totais no Espírito Santo de 2013 a 2022",
       x="Anos", y="Óbitos Totais") +
  scale_x_continuous(
    breaks = dados.mortes.es.totais.series$ANOOBITO,
    labels = dados.mortes.es.totais.series$ANOOBITO)+
  theme_classic()+
  theme(legend.title = element_text(size = size_titulo_legenda),               
        legend.text = element_text(size = size_texto_legenda))

series_mortes_es_total


#save(series_mortes_es_total, file = "GRAFICOS_RDA/series_mortes_es_total.RData")

#2.1 quantidade de mortes psic no es

# juntando os dados

dados.mortes.es.psic.series <- data.frame(
  dados_es_psic %>% group_by(ANOOBITO) %>%
    summarise(N.obitos = n())
)

# grafico

series_mortes_es_psic <- ggplot(data = dados.mortes.es.psic.series, aes(x = ANOOBITO, y = N.obitos)) +
  geom_line(linetype = "solid" ,color = paleta_series(1),
            linewidth = 0.5) +
  geom_point(shape = 15, aes(text  = paste("Ano: ", ANOOBITO, 
                                           "<br>Quantidade: ", N.obitos))) +
  labs(title = "Óbitos por Psicoativos no Espírito Santo de 2013 a 2022",
       x="Anos", y="Óbitos Totais") +
  scale_x_continuous(
    breaks = dados.mortes.es.psic.series$ANOOBITO,
    labels = dados.mortes.es.psic.series$ANOOBITO)+
  theme_classic()+
  theme(legend.title = element_text(size = size_titulo_legenda),               
        legend.text = element_text(size = size_texto_legenda))

series_mortes_es_psic

#save(series_mortes_es_psic, file = "GRAFICOS_RDA/series_mortes_es_psic.RData")

