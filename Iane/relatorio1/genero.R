#library(remotes)
#library(microdatasus)
#library(dplyr)
#library(stringr)
#library(ggplot2)
#library(lubridate)



# -------------- ANALISE GENERO -------------

#HISTOGRAMA

#2.1 quantidade de mortes por genero no brasil
freq_genero_br_total <- dados_br_total %>%
  group_by(SEXO) %>%
  summarise(Quantidade = n(), .groups = "drop") %>%
  mutate(Porcentagem = Quantidade / sum(Quantidade) * 100)

hist_genero_br_total <- ggplot(freq_genero_br_total, aes(x = SEXO, y = Quantidade, fill = SEXO)) +
  geom_bar(stat = "identity") +
  theme(plot.title = element_text(size = 20)) +
  labs(title = "Quantidades de mortes no Brasil entre o periodo de 2013 a 2022 de acordo com o genero",
       x = "genero",
       y = "Numero de Pessoas")

print(hist_genero_br_total)

#2.2 quantidade de mortes por genero no espirito santo

freq_genero_br_psic <- dados_br_psic %>%
  group_by(SEXO) %>%
  summarise(Quantidade = n(), .groups = "drop") %>%
  mutate(Porcentagem = Quantidade / sum(Quantidade) * 100)

hist_genero_br_psic <- ggplot(freq_genero_br_psic, aes(x = SEXO, y = Quantidade, fill = SEXO)) +
  geom_bar(stat = "identity") +
  theme(plot.title = element_text(size = 20)) +
  labs(title = "Quantidades de mortes por psicoativo no Brasil entre o periodo de 2013 a 2022 de acordo com o genero",
       x = "genero",
       y = "Numero de Pessoas")

print(hist_genero_br_psic)

#2.3 quantidade de mortes por psicoatvos por genero no brasil

freq_genero_es_total <- dados_es_total %>%
  group_by(SEXO) %>%
  summarise(Quantidade = n(), .groups = "drop") %>%
  mutate(Porcentagem = Quantidade / sum(Quantidade) * 100)

hist_genero_es_total <- ggplot(freq_genero_es_total, aes(x = SEXO, y = Quantidade, fill = SEXO)) +
  geom_bar(stat = "identity") +
  theme(plot.title = element_text(size = 20)) +
  labs(title = "Quantidades de mortes no ES entre o periodo de 2013 a 2022 de acordo com o genero",
       x = "genero",
       y = "Numero de Pessoas")

print(hist_genero_es_total)

#2.4 quantidade de mortes por psicoativos por genero no espirito santo

freq_genero_es_psic <- dados_es_psic %>%
  group_by(SEXO) %>%
  summarise(Quantidade = n(), .groups = "drop") %>%
  mutate(Porcentagem = Quantidade / sum(Quantidade) * 100)

hist_genero_es_psic <- ggplot(freq_genero_es_psic, aes(x = SEXO, y = Quantidade, fill = SEXO)) +
  geom_bar(stat = "identity") +
  theme(plot.title = element_text(size = 20)) +
  labs(title = "Quantidades de mortes por psicoativo no ES entre o periodo de 2013 a 2022 de acordo com o genero",
       x = "Genero",
       y = "Numero de Pessoas")

print(hist_genero_es_psic)







#SERIE

#2.1 quantidade de mortes por genero no brasil

# montando os dados
dados.genero.br.series <- data.frame(
  dados_br_total %>% group_by(ANOOBITO, SEXO) %>%
    summarise(N.obitos = n())
)

# grafico
series_genero_br_total <- ggplot(data = dados.genero.br.series, aes(x = ANOOBITO, y = N.obitos,
                                                         colour = SEXO)) +
  geom_line(linewidth = 0.5, linetype = "solid") +
  geom_point(shape = 15, aes(colour = SEXO,
                             text  = paste("Ano: ", ANOOBITO, 
                                           "<br>Quantidade: ", N.obitos,
                                           "<br>Sexo: ", SEXO))) +
  scale_colour_manual(values=rev(paleta_series(2)))+
  labs(title = "Número de óbitos Totais no Brasil por gênero",
       x="Anos", y="Cbitos Totais", colour = "Sexo") +
  scale_x_continuous(
    breaks = dados.genero.br.series$ANOOBITO,
    labels = dados.genero.br.series$ANOOBITO)+
  theme_classic()+
  theme(plot.title = element_text(size = 15))

ggplotly(series_genero_br_total)

save(series_genero_br_total, file="GRAFICOS_RDA/series_genero_br_total.RData")

#2.2 quantidade de mortes por genero no espirito santo

# montando os dados
dados.genero.es.series <- data.frame(
  dados_es_total %>% group_by(ANOOBITO, SEXO) %>%
    summarise(N.obitos = n())
)

# grafico
series_genero_es_total <- ggplot(data = dados.genero.es.series, aes(x = ANOOBITO, y = N.obitos,
                                                                 colour = SEXO)) +
  geom_line(linewidth = 0.5, linetype = "solid") +
  geom_point(shape = 15, aes(colour = SEXO,
                             text  = paste("Ano: ", ANOOBITO, 
                                           "<br>Quantidade: ", N.obitos,
                                           "<br>Sexo: ", SEXO))) +
  scale_colour_manual(values=rev(paleta_series(2)))+
  labs(title = "Número de óbitos Totais no Espírito Santo por gênero",
       x="Anos", y="Cbitos Totais", colour = "Sexo") +
  scale_x_continuous(
    breaks = dados.genero.es.series$ANOOBITO,
    labels = dados.genero.es.series$ANOOBITO)+
  theme_classic()+
  theme(plot.title = element_text(size = 15))

print (series_genero_es_total)

save(series_genero_es_total, file="GRAFICOS_RDA/series_genero_es_total.RData")
#2.1 quantidade de mortes por psicoativos por genero no brasil

# montando os dados
dados.genero.br.series.psic <- data.frame(
  dados_br_psic %>% group_by(ANOOBITO, SEXO) %>%
    summarise(N.obitos = n())
)

# grafico
series_genero_br_psic <- ggplot(data = dados.genero.br.series.psic, aes(x = ANOOBITO, y = N.obitos,
                                                                    colour = SEXO)) +
  geom_line(linewidth = 0.5, linetype = "solid") +
  geom_point(shape = 15, aes(colour = SEXO,
                             text  = paste("Ano: ", ANOOBITO, 
                                           "<br>Quantidade: ", N.obitos,
                                           "<br>Sexo: ", SEXO))) +
  scale_colour_manual(values=rev(paleta_series(2)))+
  labs(title = "Número de óbitos por psicoativos no Brasil por gênero",
       x="Anos", y="obitos por psicativos", colour = "Sexo") +
  scale_x_continuous(
  breaks = dados.genero.br.series.psic$ANOOBITO,
  labels = dados.genero.br.series.psic$ANOOBITO)+
  theme_classic()+
  theme(plot.title = element_text(size = 15))

print (series_genero_br_psic)

save(series_genero_br_psic, file="GRAFICOS_RDA/series_genero_br_psic.RData")

#2.2 quantidade de mortes por psicoativos por genero no espirito santo

# montando os dados
dados.genero.es.series.psic <- data.frame(
  dados_es_psic %>% group_by(ANOOBITO, SEXO) %>%
    summarise(N.obitos = n())
)

# grafico
series_genero_es_psic <- ggplot(data = dados.genero.es.series.psic, aes(x = ANOOBITO, y = N.obitos,
                                                                        colour = SEXO)) +
  geom_line(linewidth = 0.5, linetype = "solid") +
  geom_point(shape = 15, aes(colour = SEXO,
                             text  = paste("Ano: ", ANOOBITO, 
                                           "<br>Quantidade: ", N.obitos,
                                           "<br>Sexo: ", SEXO))) +
  scale_colour_manual(values=rev(paleta_series(2)))+
  labs(title = "Número de óbitos por psicoativos no Espírito Santo por gênero",
       x="Anos", y="Cbitos por psicativos", colour = "Sexo") +
  scale_x_continuous(
    breaks = dados.genero.es.series.psic$ANOOBITO,
    labels = dados.genero.es.series.psic$ANOOBITO)+
    theme_classic()+
    theme(plot.title = element_text(size = 15))

print (series_genero_es_psic)


save(series_genero_es_psic, file="GRAFICOS_RDA/series_genero_es_psic.RData")

#TABELA E INDICADOR

#uniao dos dados
# primeiras duas tabelas
#uniao_genero1 <- merge(freq_genero_br_total, freq_genero_br_psic, by = "SEXO")
# resultado com a terceira tabela
#uniao_genero2 <- merge(uniao_genero1, freq_genero_es_total, by = "SEXO")
# resultado com a quarta tabela
#uniao_genero <- merge(uniao_genero2, freq_genero_es_psic, by = "SEXO")
# Visualize a tabela final
#print(uniao_genero)


#names(uniao_genero) <- c("Genero", "Mortes Totais BR", "% mortes BR", "Mortes por Psicoativos BR", "% mortes psic BR",
                        # "Mortes Totais ES", "% mortes ES", "Mortes por Psicoativos ES", "% mortes psic ES")

# Calcule mortes por psicoativos a cada 1000 mortes totais no BR
#uniao_genero <- uniao_genero %>%
 # mutate(Mortes_por_1000_BR = (Mortes_Psicoativos_BR / Mortes_Totais_BR) * 1000)



#GRAFICOD E PROPORCAO Brasil

#1 Criando coluna de proporcao

dados.genero.br.series.psic <- dados.genero.br.series.psic %>%
  group_by(ANOOBITO) %>%
  mutate(total_mortes = sum(N.obitos),
         porcentagem = (N.obitos / total_mortes) * 100) %>%
  ungroup()

#2 Criar o grC!fico
proporcao_genero_br_psic <- ggplot(dados.genero.br.series.psic, aes(x = factor(ANOOBITO), y = porcentagem, fill = SEXO)) +
  geom_bar(stat = "identity", position = "stack",
           aes(text = paste("Ano: ", factor(ANOOBITO), 
                            "<br>Percentual: ", round(porcentagem, 2),
                            "<br>Sexo: ", SEXO)))+
  scale_fill_manual(values=rev(paleta_hist(2)))+
  labs(x = "Ano", y = "Porcentagem (%)", fill = "Gênero",
       title = "Percentual de Mortes por Psicoativos no Brasil por Gênero e Ano") +
  theme_classic()+
  theme(plot.title = element_text(size = 13),
        axis.text= element_text(size = 15))

save(proporcao_genero_br_psic, file="GRAFICOS_RDA/proporcao_genero_br_psic.RData")

#GRAFICOD E PROPORCAO ES

#1 Criando coluna de proporcao

dados.genero.es.series.psic <- dados.genero.es.series.psic %>%
  group_by(ANOOBITO) %>%
  mutate(total_mortes = sum(N.obitos),
         porcentagem = (N.obitos / total_mortes) * 100) %>%
  ungroup()

#2 Criar o grC!fico
proporcao_genero_es_psic <- ggplot(dados.genero.es.series.psic, aes(x = factor(ANOOBITO), y = porcentagem, fill = SEXO)) +
  geom_bar(stat = "identity", position = "stack",
           aes(text = paste("Ano: ", factor(ANOOBITO), 
                            "<br>Percentual: ", round(porcentagem, 2),
                            "<br>Sexo: ", SEXO))) +
  scale_fill_manual(values=rev(paleta_hist(2)))+
  labs(x = "Ano", y = "Porcentagem (%)", fill = "Gênero",
       title = "Percentual de Mortes por Psicoativos no ES por Gênero e Ano") +
  theme_classic()+
  theme(plot.title = element_text(size = 13),
        axis.text= element_text(size = 15))

save(proporcao_genero_es_psic, file="GRAFICOS_RDA/proporcao_genero_es_psic.RData")

proporcao_genero_br_psic
proporcao_genero_es_psic



