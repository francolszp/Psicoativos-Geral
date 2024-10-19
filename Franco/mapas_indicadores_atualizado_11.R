
library(sf)
library(lwgeom)
# baixando info estados brasil (geobr) ------------------------------------

brasil = read_state(code_state = "all", year = 2018)
pop13a21<-readRDS("Bases/pop13a21.rds")
### MAPA DO BRASIL OBITOS POR ESTADO POR PSICO/POP ESTADO 2013 e 2022


### numeros de obitos por estado 13 e 22
dados_mapa_psic_pop13 <- data.frame(
  dados_br_psic %>%
    filter(ANOOBITO == "2013") %>%
    group_by(codigoUF, ANOOBITO) %>%
    summarise(N.obitos = n()))

dados_mapa_psic_pop22 <- data.frame(
  dados_br_psic %>%
    filter(ANOOBITO == "2022") %>%
    group_by(codigoUF, ANOOBITO) %>%
    summarise(N.obitos = n()))

### selecionando pop de 2013 dos estados
pop_UF_13 <- pop13a21 %>%
  filter(ano == "2013" & codigoUF != 1) %>%
  reframe(unique(valor), codigoUF, ano)

pop_UF_13 <- rename(pop_UF_13, pop13 = `unique(valor)`)

dados_mapa_psic_pop13<- left_join(dados_mapa_psic_pop13,pop_UF_13,
                                  by = "codigoUF")


var_extras_UF <- read_table("Bases/var_extras_UF.txt")

dados_mapa_psic_pop13$codigoUF <- as.numeric(dados_mapa_psic_pop13$codigoUF)
dados_mapa_psic_pop22$codigoUF <- as.numeric(dados_mapa_psic_pop22$codigoUF)

#preparando dados com as siglas
dados_mapa_psic_pop13 <- left_join(dados_mapa_psic_pop13, var_extras_UF,
                                   by = c("codigoUF"))

dados_mapa_psic_pop22<- left_join(dados_mapa_psic_pop22, var_extras_UF,
                                  by = c("codigoUF"))


# joins com base brasil coordenadas
dados_mapa_psic_pop13 <- brasil %>%
  left_join(dados_mapa_psic_pop13, by = c("abbrev_state"="Sigla"))

dados_mapa_psic_pop22 <- brasil %>%
  left_join(dados_mapa_psic_pop22, by = c("abbrev_state"="Sigla"))

set.seed(0411)

points_within <- st_sample(dados_mapa_psic_pop13, size = 10000, type = "random")  # Ajuste o 'size' conforme necessário

# Converter os pontos gerados em um dataframe com a geometria correta
points_within_df_13 <- st_as_sf(points_within) %>%
  st_set_crs(st_crs(dados_mapa_psic_pop13)) %>%
  st_join(dados_mapa_psic_pop13) %>%
  mutate(label = paste("UF: ", abbrev_state, "<br>Taxa de Óbitos: ", round((N.obitos / pop13) * 100000, 2)))


# Converter os pontos gerados em um dataframe com a geometria correta
points_within_df_22 <- st_as_sf(points_within) %>%
  st_set_crs(st_crs(dados_mapa_psic_pop22)) %>%
  st_join(dados_mapa_psic_pop22) %>%
  mutate(label = paste("UF: ", abbrev_state, "<br>Taxa de Óbitos: ", round((N.obitos / pop22) * 100000, 2)))


# Verificar se a geometria está correta

mapa_psic_pop13 <- ggplot() +
  geom_sf(data = dados_mapa_psic_pop13, 
          aes(fill = (N.obitos/pop13)*100000),
          color = "#788881")+ 
  geom_sf(data = points_within_df_13, aes(text = label),
          color = "transparent") +
  scale_fill_gradient(low = "white", high = "#010440",limits = c(0,22),
                      name="Óbitos por 100000 hab.") +
  labs(title="Óbitos por Psicoativos em 2013 por 100000 habitantes") +
  theme_void() + theme(title = element_text(size = 15),
                       axis.line = element_blank(),
                       legend.title = element_text(size = size_titulo_legenda),               
                       legend.text = element_text(size = size_texto_legenda))

ggplotly(mapa_psic_pop13, tooltip = "text")

#save(mapa_psic_pop13, file="graficos/mapa_psic_pop13.RData")

mapa_psic_pop22 <- ggplot() +
  geom_sf(data = dados_mapa_psic_pop22, aes(fill = (N.obitos/pop22)*100000),
          color = "#788881")+
  geom_sf(data = points_within_df_22, aes(text = label),
          color = "transparent") +
  scale_fill_gradient(low = "white", high = "#010440", limits = c(0,22),
                      name="Obitos por 100000 hab.") +
  labs(title="Obitos por Psicoativos em 2022 por 100000 habitantes", size=15) +
  theme_void() + theme(title = element_text(size = 15),
                       axis.line = element_blank(),
                       legend.title = element_text(size = size_titulo_legenda),               
                       legend.text = element_text(size = size_texto_legenda))

ggplotly(mapa_psic_pop22, tooltip = "text")

#save(mapa_psic_pop22, file="graficos/mapa_psic_pop22.RData")

