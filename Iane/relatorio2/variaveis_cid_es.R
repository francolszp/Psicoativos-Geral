#-------------- CONFIGURACOES INICIAIS ----------------------------

library(ggplot2)
library(scales)
library(dplyr)
library(tidyr)
library(DT)

Sys.setlocale("LC_ALL", "pt_BR.UTF-8") #configurar o R pra aceitar acentuação nas palavras

# Definir um tema global para todos os gráficos
#theme_padronizado_es <- theme(
#  plot.title = element_text(size = 16),   # Tamanho do título
#  axis.title = element_text(size = 11), # Tamanho dos títulos dos eixos
#  axis.text = element_text(size = 11),    # Tamanho dos textos dos eixos
#  legend.title = element_text(size = 11),  # Tamanho dos títulos das legendas
#  legend.text = element_text(size = 11)   # Tamanho dos textos das legendas
#)

# Aplicar o tema a todos os gráficos subsequentes
#theme_set(theme_minimal() + theme_padronizado_es)

#criar pasta pra salvar os graficos
#dir.create("graficos_comparacao")

#---------------- GRAFICO DE BARRAS DE NUMERO DE OBSERVACOES (LINHAS) POR VARIÁVEL -----------------

lista_dfs_es <- list(dados_es_causabas, dados_es_causabaso, dados_es_linhaa, dados_es_linhab, dados_es_linhab, 
                     dados_es_linhac, dados_es_linhad, dados_es_linhaii) # criar uma lista com todos os dataframes das variaveis 
nomes_dfs_es <- c("dados_es_causabas", "dados_es_causabaso", "dados_es_linhaa", "dados_es_linhab", "dados_es_linhab", 
                  "dados_es_linhac", "dados_es_linhad", "dados_es_linhaii") # criar um vetor com o nome dos dataframes

nomes_variaveis_es <- sub("dados_es_", "", nomes_dfs_es) #remover o prefixo dos dataframes e salvar apenas os nomes das variaveis

num_obs_es <- sapply(lista_dfs_es, nrow) #contar o numero de linhas de cada dataframe

obs_variavel_es <- data.frame(Nome = nomes_variaveis_es, Observacoes = num_obs_es) #criar dataframe com o numero de linhas de cada variavel


#plotando grafico 

obs.por.variavel.es <- ggplot(obs_variavel_es, aes(x = Nome, y = Observacoes, fill = Nome, 
                                                   text = paste("Variável:", Nome, "<br>",
                                                                "N. de Observações:", Observacoes))) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = comma) +  # tirar notação científica
  labs(title = "Observações por Variável CID - ES",
       x = "Variável",
       y = "Número de Observações") +
  theme(legend.position = "none")  # Remove a legenda

# Converta o gráfico para um objeto plotly
obs.por.variavel.es_plotly <- ggplotly(obs.por.variavel.es, tooltip = "text")

# Exiba o gráfico interativo
obs.por.variavel.es_plotly

# Opcional: Salve como HTML interativo
#htmlwidgets::saveWidget(obs.por.variavel.es_plotly, "graficos_comparacao/obs_por_variavel.es.html")

# -------- COMPARACAO ENTRE O NUMERO TOTAL DE MORTES POR ANO EM CADA VARIAVEL ----------------

df_comparacoes_es <- data.frame() #dataframe vazio pra armazenar os dados de morte por ano e variavel

# Loop para agrupar as mortes por ANOOBITO em cada data frame
for (i in seq_along(lista_dfs_es)) {
  df <- lista_dfs_es[[i]]
  nome_variavel_es <- sub("dados_es_", "", nomes_dfs_es[i])
  
  # Agrupar por ANOOBITO, somar o número de mortes e tirar media das idades
  df_agrupado_es <- df %>%
    group_by(ANOOBITO) %>%
    summarise(Total_Mortes = n(),
              Media_Idade = mean(IDADE2, na.rm = TRUE)) %>%
    mutate(Variavel = nome_variavel_es)
  
  # Combinar com o dataframe final
  df_comparacoes_es <- rbind(df_comparacoes_es, df_agrupado_es)
} 

# Plotar o gráfico
mortes.por.variavel.es <- ggplot(df_comparacoes_es, aes(x = ANOOBITO, y = Total_Mortes, color = Variavel, group = Variavel, 
                                                         text = paste("Ano do Óbito:", ANOOBITO, "<br>",
                                                                      "N. de Mortes:", Total_Mortes, "<br>", "Variável CID:", Variavel))) +
  geom_line(size = 1, show.legend = FALSE) +
  labs(title = "Mortes por Psicoativos por Ano - ES",
       x = "Ano",
       y = "Número de Mortes") +
  scale_x_continuous(breaks = 2013:2022) # Definir os anos no eixo x
  

# Converta o gráfico para um objeto plotly
mortes.por.variavel.es_plotly <- ggplotly(mortes.por.variavel.es, tooltip = "text")

# Exiba o gráfico interativo
mortes.por.variavel.es_plotly

# Opcional: Salve como HTML interativo
#htmlwidgets::saveWidget(mortes.por.variavel.es_plotly, "graficos_comparacao/mortes.por.variavel.es.html")

# ----------------- IDADE E FAIXA ETARIA -------------------------------

# Gráfico da idade média das mortes por ano
idade.por.variavel.es <- ggplot(df_comparacoes_es, aes(x = ANOOBITO, y = Media_Idade, color = Variavel, group = Variavel,
                                                       text = paste("Ano do Óbito:", ANOOBITO, "<br>",
                                                                    "Media das Idade:", Media_Idade, "<br>", "Variável CID:", Variavel))) +
  geom_line(size = 1, show.legend = FALSE) +
  labs(title = "Idade Média das Mortes por Ano - ES",
       x = "Ano",
       y = "Idade Média") +
  scale_x_continuous(breaks = 2013:2022) +
  scale_y_continuous(breaks = seq(floor(min(df_comparacoes_es$Media_Idade)), 
                                  ceiling(max(df_comparacoes_es$Media_Idade)), 
                                  by = 2))

# Converta o gráfico para um objeto plotly
idade.por.variavel.es_plotly <- ggplotly(idade.por.variavel.es, tooltip = "text")

# Exiba o gráfico interativo
idade.por.variavel.es_plotly

# Opcional: Salve como HTML interativo
#htmlwidgets::saveWidget(idade.por.variavel.es_plotly, "graficos_comparacao/idade.por.variavel.es.html")

# BOXPLOT IDADES

df_idades_es <- data.frame() 

for (i in seq_along(lista_dfs_es)) {
  df <- lista_dfs_es[[i]]
  nome_variavel_es <- sub("dados_es_", "", nomes_dfs_es[i])
  
  df <- df %>% mutate(Variavel = nome_variavel_es) # Adicionar uma coluna com o nome da variável do cid
  
  df_idades_es <- rbind(df_idades_es, df[, c("IDADE2", "Variavel")]) # Selecionar apenas as colunas que eu quero e combinar com o dataframe criado
}


  
  
boxplot.idades.es <- ggplot(df_idades_es, aes(x = Variavel, y = IDADE2, color = Variavel)) +
geom_boxplot() +
labs(title = "Idades por Variável CID -ES",
      x = "Variável",
      y = "Idade")
  
  

# Converta o gráfico para um objeto plotly
boxplot.idades.es_plotly <- ggplotly(boxplot.idades.es, tooltip = "text")

# Exiba o gráfico interativo
boxplot.idades.es_plotly

# Opcional: Salve como HTML interativo
#htmlwidgets::saveWidget(boxplot.idades.es_plotly, "graficos_comparacao/boxplot.idades.es.html")

# SERIES POR FAIXA ETÁRIA

df_faixas_etarias_es <- data.frame()

for (i in seq_along(lista_dfs_es)) {
  df <- lista_dfs_es[[i]]
  nome_variavel_es <- sub("dados_es_", "", nomes_dfs_es[i])
  
  # Filtrar as idades para manter apenas [30-60) e [60-infinito)
  df <- df %>%
    filter(IDADE2 >= 30) %>%  # Filtrar idades >= 30
    mutate(Faixa_Etaria = case_when(
      IDADE2 >= 30 & IDADE2 < 60 ~ "[30-60)",
      IDADE2 >= 60 ~ "[60-infinito)"
    )) %>%
    group_by(ANOOBITO, Faixa_Etaria) %>%
    summarise(Total = n()) %>%
    mutate(Variavel = nome_variavel_es)
  
  df_faixas_etarias_es <- rbind(df_faixas_etarias_es, df) # Combinar os dados no dataframe
} 

# Criar o gráfico de séries
grafico.series.faixaeta.es <- ggplot(df_faixas_etarias_es, aes(x = ANOOBITO, y = Total, color = Variavel, shape = Faixa_Etaria,
                                                               text = paste("Ano do Óbito:", ANOOBITO, "<br>",
                                                                            "N. de Mortes:", Total, "<br>", 
                                                                            "Variável CID:", Variavel, "<br>", 
                                                                            "Faixa Etária:", Faixa_Etaria))) +
  geom_line(aes(group = interaction(Variavel, Faixa_Etaria, show.legend = FALSE)), size = 0.5) +
  geom_point(size = 1.5) +
  scale_shape_manual(values = c("[30-60)" = 16, "[60-infinito)" = 15)) +  # Bolinha para [30-60), quadrado para [60-infinito)
  labs(title = "Mortes por Faixa Etária e Variável CID - ES",
       x = "Ano",
       y = "Quantidade de Pessoas",
       shape = "Faixa Etária") +
  scale_x_continuous(breaks = 2013:2022)  # Definir os anos no eixo x

# Converta o gráfico para um objeto plotly
grafico.series.faixaeta.es_plotly <- ggplotly(grafico.series.faixaeta.es, tooltip = "text")

# Exiba o gráfico interativo
grafico.series.faixaeta.es_plotly

# Opcional: Salve como HTML interativo
#htmlwidgets::saveWidget(grafico.series.faixaeta.es_plotly, "graficos_comparacao/grafico.series.faixaeta.html")

#---------------------- SEXO ---------------

df_genero_es <- data.frame()

for (i in seq_along(lista_dfs_es)) {
  df <- lista_dfs_es[[i]]
  nome_variavel_es <- sub("dados_es_", "", nomes_dfs_es[i])
  
  # Agrupar por ANOOBITO, SEXO e calcular o número de mortes
  df_agrupado_es <- df %>%
    filter(!is.na(SEXO)) %>%
    group_by(ANOOBITO, SEXO) %>%
    summarise(Total = n()) %>%
    mutate(Variavel = nome_variavel_es)
  
  df_genero_es <- rbind(df_genero_es, df_agrupado_es)
}

# Criar o gráfico
grafico.series.genero.es <- ggplot(df_genero_es, aes(x = ANOOBITO, y = Total, color = Variavel, shape = SEXO,
                                                     text = paste("Ano do Óbito:", ANOOBITO, "<br>",
                                                                  "N. de Mortes:", Total, "<br>", "Variável CID:", Variavel, "<br>", "Sexo:", SEXO ))) +
  geom_line(aes(group = interaction(Variavel, SEXO, show.legend = FALSE)), size = 0.5) +
  geom_point(size = 1.5) +
  scale_shape_manual(values = c("Masculino" = 16, "Feminino" = 17)) +  # Bolinha para masculino, triângulo para feminino
  labs(title = "Mortes por Sexo e Variável CID - ES",
       x = "Ano",
       y = "Número de Mortes",
       shape = "Sexo") +
  scale_x_continuous(breaks = 2013:2022)

# Converta o gráfico para um objeto plotly
grafico.series.genero.es_plotly <- ggplotly(grafico.series.genero.es, tooltip = "text")

# Exiba o gráfico interativo
grafico.series.genero.es_plotly

# Opcional: Salve como HTML interativo
#htmlwidgets::saveWidget(grafico.series.genero.es_plotly, "graficos_comparacao/grafico.series.genero.es.html")


#----------------------- RACA ---------------------------------

df_raca_es <- data.frame()

for (i in seq_along(lista_dfs_es)) {
  df <- lista_dfs_es[[i]]
  nome_variavel_es <- sub("dados_es_", "", nomes_dfs_es[i])
  
  # Filtrar brancos, pretos e pardos e juntar pretos com pardos
  df <- df %>%
    filter(RACACOR %in% c("Branca", "Preta", "Parda")) %>%
    mutate(Raca_Agrupada = case_when(
      RACACOR %in% c("Preta", "Parda") ~ "Pretos/Pardos",
      RACACOR == "Branca" ~ "Brancos"
    ))
  
  # Agrupar por ANOOBITO, Raca_Agrupada e calcular o número de mortes
  df_agrupado_es <- df %>%
    group_by(ANOOBITO, Raca_Agrupada) %>%
    summarise(Total = n()) %>%
    mutate(Variavel = nome_variavel_es)
  
  df_raca_es <- rbind(df_raca_es, df_agrupado_es)
}

# Criar o gráfico de séries
grafico.series.raca.es <- ggplot(df_raca_es, aes(x = ANOOBITO, y = Total, color = Variavel, shape = Raca_Agrupada,
                                                 text = paste("Ano do Óbito:", ANOOBITO, "<br>",
                                                              "N. de Mortes:", Total, "<br>", "Variável CID:", Variavel, "<br>", "Cor/Raça:", Raca_Agrupada ))) +
  geom_line(aes(group = interaction(Variavel, Raca_Agrupada, show.legend = FALSE)), size = 0.5) +
  geom_point(size = 1.5) +
  scale_shape_manual(values = c("Brancos" = 16, "Pretos/Pardos" = 17)) +  # Bolinha para Brancos, triângulo para Pretos/Pardos
  labs(title = "Mortes por Raça e Variável CID - ES",
       x = "Ano",
       y = "Número de Mortes",
       color = "Variável CID", 
       shape = "Raça") +
  scale_x_continuous(breaks = 2013:2022) +
  guides(
    color = guide_legend(title = "Variável CID", order = 1),
    shape = guide_legend(title = "Raça", order = 2)
  )

# Converta o gráfico para um objeto plotly
grafico.series.raca.es_plotly <- ggplotly(grafico.series.raca.es, tooltip = "text")

# Exiba o gráfico interativo
grafico.series.raca.es_plotly

# Opcional: Salve como HTML interativo
#htmlwidgets::saveWidget(grafico.series.raca.es_plotly, "graficos_comparacao/grafico.series.raca.es.html")


#------------- TABELA DE DADOS NA ----------------------------

df_na_porcentagem_es <- data.frame()

# Variáveis sociodemográficas que serão analisadas
var_sociodemo_es <- c("SEXO", "RACACOR", "ESC", "ESTCIV")

for (i in seq_along(lista_dfs_es)) {
  df <- lista_dfs_es[[i]]
  nome_variavel_es <- sub("dados_es_", "", nomes_dfs_es[i])
  
  # Loop para calcular a porcentagem de NA para cada variável sociodemográfica
  for (var_soc in var_sociodemo_es) {
    df_na_es <- df %>%
      group_by(ANOOBITO) %>%
      summarise(
        Total_Casos = n(),
        Total_NA = sum(is.na(.data[[var_soc]])),
        Porcentagem_NA = (Total_NA / Total_Casos) * 100
      ) %>%
      mutate(Variavel_CID = nome_variavel_es,
             Var_Sociodemo = var_soc)
    
    # Combinar os resultados no dataframe final
    df_na_porcentagem_es <- bind_rows(df_na_porcentagem_es, df_na_es)
  }
}

# Criar tabela interativa
datatable(
  df_na_porcentagem_es,
  filter = 'top',  # Adiciona filtros no topo de cada coluna
  options = list(pageLength = 10),  # Mostra 10 linhas por página
  caption = 'Porcentagem de Dados NA por Ano, Variável do CID e Variável Sociodemográfica - ES'
)

# Salvar a tabela como CSV ou outro formato se necessário
#write.csv(df_na_porcentagem_es, "graficos_comparacao/na_porcentagem_por_ano_es.csv", row.names = FALSE)

