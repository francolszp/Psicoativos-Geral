library(tidytext)
library(wordcloud2)
library(ggwordcloud)
library(htmlwidgets)
library(wordcloud)

## OCUPAÇÃO

dados_ocup_es <- data.frame(
  dados_es_total %>% group_by(ANOOBITO, OCUP) %>%
    summarise(N.obitos = n())
)

dados_ocup_br <- data.frame(
  dados_br_total %>% group_by(ANOOBITO, OCUP) %>%
    summarise(N.obitos = n())
)
  
dados_ocup_es_psic <- data.frame(
  dados_es_psic %>% group_by(ANOOBITO, OCUP) %>%
    summarise(N.obitos = n())
)

dados_ocup_br_psic <- data.frame(
  dados_br_psic %>% group_by(ANOOBITO, OCUP) %>%
    summarise(N.obitos = n())
)


# 1. Saúde
regex_saude <- "(?i)\\b(enfermeiro|psic[oó]logo|m[eé]dico|dentista|sa[uú]de|t[eé]cnico de enfermagem|auxiliar de enfermagem|laborat[oó]rio|cirurgi[aã]o)\\b"

# 2. Educação
regex_educacao <- "(?i)\\b(professor|pedagogo|educa[cç][aã]o|ensino|instrutor|pesquisador|escola|fundamental)\\b"

# 3. Agronegócio
regex_agro <- "(?i)\\b(agricultura|agricola|feirante|agricultor|pecu[aá]ria|agropec[uú]ario|trabalhador da cultura|pescador|pecuarista|caseiro|trabalhador rural|pecuaria|oleri[aã]cultura|florestal|cultivo|produtor)\\b"

# 4. Serviços Gerais
regex_servicos <- "(?i)\\b(relojeiro|joalheiro|borracheiro|vendedor|cabeleireiro|sapateiro|comerciante|costurador|padeiro|maquinista|frentista|contador|lavador|lavadora|lavadeir[oa]|jardineiro|cobrador|cozinha|carregador|a[cç]ougueiro|fot[óo]grafo|encanador|dom[eé]stico|diarista|cozinhadora|cozinhador|cozinheir[oa]|costureir[ao]|assistente|catador|faxineiro|porteiro|motorista|vendedor|gar[cç]om|manicure|pedicuro|vigilante|auxiliar geral|atendente|coletor de lixo|varredor de rua|balconista|cobrador|camareiro|recepcionista|motorista)\\b"

# 5. Segurança e Justiça
regex_seguranca_justica <- "(?i)\\b(justi[cç]a|advogado|juiz|vigilante|policial|bombeiro|investigador|seguran[cç]a|guarda|soldado|vigia|cabo|taifeiro|pol[ií]cia|fiscal|procurador|militar|oficial|bombeiro|policial)\\b"

# 6. Indústria, Mecânica, Elétrica, Construção Civil
regex_industria_construcao <- "(?i)\\b(engenharia|construçao|el[ée]trica|mec[âa]nica|ind[uú]stria|ve[ií]culos|marceneiro|eletr[oô]nico|produ[cç]ao|metal[úu]rgico|metalurgia|linha de produ[cç][aã]o|pedreiro|engenheiro|mestre|obra|carpinteiro|armador|gesseiro|pintor de obras|servente|serralheiro|montador de andaimes|montador de estruturas metalicas|operador|mec[aâ]nico|soldador|montador|ajustador|eletricista|m[aá]quinas|embalador|conferente|bobinador|art[ií]fice|supervisor|manuten[cç][aã]o|instalador|usinagem|ferramenteiro|industria|fabricac[aã]o|constru[cç][aã]o|tecel[aã]o|metalicas|produtoras|vidraceiro|engenharia|sistemas|tecnicos)\\b"

# 7. Tecnologia e Setor Financeiro
regex_tecnologia_financeiro <- "(?i)\\b(telecomunica[cç][oõ]es|computa[cç][aã]o|analista|tecnologia|desenvolvedor|inform[aá]tica|sistemas|projetista|administrativo|contabil[íí]stico|financeiro|bancos|empresa||banc[aã]rio|analista de credito|auditor|consultor|tesoureiro|gestor|planejador|controlador|economista|financiamento|bancario|administracao financeira|auditoria)\\b"




dados_ocup_br <- dados_br_total %>%
  mutate(categoria_ocupacao = case_when(
    is.na(OCUP) ~ NA_character_,
    str_detect(OCUP, regex_saude) ~ "Saúde",
    str_detect(OCUP, regex_educacao) ~ "Educação",
    str_detect(OCUP, regex_agro) ~ "Agropecuária",
    str_detect(OCUP, regex_servicos) ~ "Serviços Gerais",
    str_detect(OCUP, regex_seguranca_justica) ~ "Justiça e Segurança",
    str_detect(OCUP, regex_industria_construcao) ~ "Indústria, Mecânica, Elétrica e Construção Civil",
    #str_detect(OCUP, regex_tecnologia_financeiro) ~ "Tecnologia e Setor Financeiro",
    TRUE ~ "Outros"
  )) %>% 
  group_by(ANOOBITO, categoria_ocupacao) %>%
  summarise(N.obitos = n())







dados_ocup_es <- dados_es_total %>%
  mutate(categoria_ocupacao = case_when(
    is.na(OCUP) ~ NA_character_,
    str_detect(OCUP, regex_saude) ~ "Saúde",
    str_detect(OCUP, regex_educacao) ~ "Educação",
    str_detect(OCUP, regex_agro) ~ "Agropecuária",
    str_detect(OCUP, regex_servicos) ~ "Serviços Gerais",
    str_detect(OCUP, regex_seguranca_justica) ~ "Justiça e Segurança",
    str_detect(OCUP, regex_industria_construcao) ~ "Indústria, Mecânica, Elétrica e Construção Civil",
    #str_detect(OCUP, regex_tecnologia_financeiro) ~ "Tecnologia e Setor Financeiro",
    TRUE ~ "Outros"
  )) %>% 
  group_by(ANOOBITO, categoria_ocupacao) %>%
  summarise(N.obitos = n())







dados_ocup_es_psic <- dados_es_psic %>%
  mutate(categoria_ocupacao = case_when(
    is.na(OCUP) ~ NA_character_,
    str_detect(OCUP, regex_saude) ~ "Saúde",
    str_detect(OCUP, regex_educacao) ~ "Educação",
    str_detect(OCUP, regex_agro) ~ "Agropecuária",
    str_detect(OCUP, regex_servicos) ~ "Serviços Gerais",
    str_detect(OCUP, regex_seguranca_justica) ~ "Justiça e Segurança",
    str_detect(OCUP, regex_industria_construcao) ~ "Indústria, Mecânica, Elétrica e Construção Civil",
    #str_detect(OCUP, regex_tecnologia_financeiro) ~ "Tecnologia e Setor Financeiro",
    TRUE ~ "Outros"
  )) %>% 
  group_by(ANOOBITO, categoria_ocupacao) %>%
  summarise(N.obitos = n())
  



dados_ocup_br_psic <- dados_br_total %>%
  mutate(categoria_ocupacao = case_when(
    is.na(OCUP) ~ NA_character_,
    str_detect(OCUP, regex_saude) ~ "Saúde",
    str_detect(OCUP, regex_educacao) ~ "Educação",
    str_detect(OCUP, regex_agro) ~ "Agropecuária",
    str_detect(OCUP, regex_servicos) ~ "Serviços Gerais",
    str_detect(OCUP, regex_seguranca_justica) ~ "Justiça e Segurança",
    str_detect(OCUP, regex_industria_construcao) ~ "Indústria, Mecânica, Elétrica e Construção Civil",
    #str_detect(OCUP, regex_tecnologia_financeiro) ~ "Tecnologia e Setor Financeiro",
    TRUE ~ "Outros"
  )) %>% 
  group_by(ANOOBITO, categoria_ocupacao) %>%
  summarise(N.obitos = n())


## GRÁFICOS

series_ocup_br<- ggplot(data = dados_ocup_br, 
                        aes(x = ANOOBITO, y = N.obitos/1000,
                            colour = categoria_ocupacao)) +
  geom_line(linewidth = 0.5, linetype = "solid") +
  geom_point(shape = 15, aes(colour = categoria_ocupacao,
                             text  = paste("Ano: ", ANOOBITO,
                                           "<br>Quantidade: ", N.obitos/1000,
                                           "<br>Área de Ocupação: ", categoria_ocupacao))) +
  scale_colour_manual(values =  paleta_series(10))+
  labs(title = "Número de Óbitos Totais no Brasil por Área de Ocupação",
       x="Anos", y="Óbitos Totais/1000", colour = "") +
  scale_x_continuous(
    breaks = dados_ocup_es$ANOOBITO,
    labels = dados_ocup_es$ANOOBITO)+
  theme_classic()+
  theme(plot.title = element_text(size = 15),
        legend.title = element_text(size = size_titulo_legenda),
        legend.text = element_text(size = size_texto_legenda))


ggplotly(series_ocup_br,  tooltip = "text")

#save(series_ocup_br, file="graficos/series_ocup_br.RData")




series_ocup_es<- ggplot(data = dados_ocup_es, 
                             aes(x = ANOOBITO, y = N.obitos,
                                 colour = categoria_ocupacao)) +
  geom_line(linewidth = 0.5, linetype = "solid") +
  geom_point(shape = 15, aes(colour = categoria_ocupacao,
                             text  = paste("Ano: ", ANOOBITO,
                                           "<br>Quantidade: ", N.obitos,
                                           "<br>Área de Ocupação: ", categoria_ocupacao))) +
  scale_colour_manual(values =  paleta_series(10))+
  labs(title = "Número de Óbitos Totais no ES por Área de Ocupação",
       x="Anos", y="Óbitos Totais", colour = "") +
  scale_x_continuous(
    breaks = dados_ocup_es$ANOOBITO,
    labels = dados_ocup_es$ANOOBITO)+
  theme_classic()+
  theme(plot.title = element_text(size = 15),
        legend.title = element_text(size = size_titulo_legenda),
        legend.text = element_text(size = size_texto_legenda))


ggplotly(series_ocup_es,  tooltip = "text")

#save(series_ocup_es, file="graficos/series_ocup_es.RData")


series_ocup_br_psic<- ggplot(data = dados_ocup_br_psic, 
                             aes(x = ANOOBITO, y = N.obitos/1000,
                                 colour = categoria_ocupacao)) +
  geom_line(linewidth = 0.5, linetype = "solid") +
  geom_point(shape = 15, aes(colour = categoria_ocupacao,
                             text  = paste("Ano: ", ANOOBITO,
                                           "<br>Quantidade: ", N.obitos/1000,
                                           "<br>Área de Ocupação: ", categoria_ocupacao))) +
  scale_colour_manual(values =  paleta_series(10))+
  labs(title = "Número de Óbitos por Psicoativos no Brasil por Área de Ocupação",
       x="Anos", y="Óbitos Totais", colour = "") +
  scale_x_continuous(
    breaks = dados_ocup_br_psic$ANOOBITO,
    labels = dados_ocup_br_psic$ANOOBITO)+
  theme_classic()+
  theme(plot.title = element_text(size = 15),
        legend.title = element_text(size = size_titulo_legenda),
        legend.text = element_text(size = size_texto_legenda))


ggplotly(series_ocup_br_psic,  tooltip = "text")

#save(series_ocup_br_psic, file="graficos/series_ocup_br_psic.RData")



series_ocup_es_psic<- ggplot(data = dados_ocup_es_psic, 
                              aes(x = ANOOBITO, y = N.obitos,
                                  colour = categoria_ocupacao)) +
  geom_line(linewidth = 0.5, linetype = "solid") +
  geom_point(shape = 15, aes(colour = categoria_ocupacao,
                             text  = paste("Ano: ", ANOOBITO,
                                           "<br>Quantidade: ", N.obitos,
                                           "<br>Área de Ocupação: ", categoria_ocupacao))) +
  scale_colour_manual(values =  paleta_series(10))+
  labs(title = "Número de Óbitos por Psicoativos no ES por Área de Ocupação",
       x="Anos", y="Óbitos Totais", colour = "") +
  scale_x_continuous(
    breaks = dados_ocup_es_psic$ANOOBITO,
    labels = dados_ocup_es_psic$ANOOBITO)+
  theme_classic()+
  theme(plot.title = element_text(size = 15),
        legend.title = element_text(size = size_titulo_legenda),
        legend.text = element_text(size = size_texto_legenda))


ggplotly(series_ocup_es_psic,  tooltip = "text")

#save(series_ocup_es_psic, file="graficos/series_ocup_es_psic.RData")




# NUVEM DE PALAVRAS


stopwords_pt <- c("de", "da", "do", "e", "o", "a", "em", 
                  "para", "com", "geral", "gerais",
                  "nos", "na", "exceto", "ou")  # Exemplo para o português


palavras_ocupacao_br <- dados_br_psic %>%
 unnest_tokens(word, OCUP) %>%
 count(word, sort = TRUE)


palavras_ocupacao_filtradas_br <- palavras_ocupacao_br %>%
  filter(!word %in% stopwords_pt) %>% 
  filter(n >= 10) %>% na.omit(words)



palavras_ocupacao_es <- dados_es_psic %>%
  unnest_tokens(word, OCUP) %>%
  count(word, sort = TRUE)


palavras_ocupacao_filtradas_es <- palavras_ocupacao_es %>%
  filter(!word %in% stopwords_pt) %>% 
  filter(n >= 10) %>% na.omit(words)



set.seed(0411)
wordcloud_br_estatica <- ggplot(palavras_ocupacao_filtradas_br, aes(label = word, size = n)) +
  geom_text_wordcloud_area(eccentricity = 1) +
  scale_size_area(max_size = 40) +
  theme_minimal()

wordcloud_es_estatica <- ggplot(palavras_ocupacao_filtradas_es, aes(label = word, size = n)) +
  geom_text_wordcloud_area(eccentricity = 1) +
  scale_size_area(max_size = 40) +
  theme_minimal()

#save(wordcloud_br_estatica, file = "graficos/wordcloud_br_estatica.RData")

#save(wordcloud_es_estatica, file = "graficos/wordcloud_es_estatica.RData")

