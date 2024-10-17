
# instalar e carregar pacotes

pacman::p_load("tidyverse", "janitor", "Hmisc",
               "summarytools", "DataExplorer", 
               "knitr","readr", "dplyr", "glue", "scales",
               "geobr", "sf", "latex2exp", "patchwork", "plotly")


#LER E SALVAR DADOS TOTAIS BR


dados_br_total <- c()

for (ano in 2013:2022) {
  
  df <- readRDS(paste0("Bases/bases_de_dados/dados_", ano,".rds"))
  dados_br_total <- bind_rows(df,dados_br_total)
  
}


#ADICIONANDO COLUNAS EXTRAS

#criar coluna do ano em que o obito ocorreu
dados_br_total$DTOBITO <- ymd(dados_br_total$DTOBITO)
dados_br_total$ANOOBITO <- year(dados_br_total$DTOBITO)

#criar coluna da idade em anos completos
dados_br_total$DTNASC  <- ymd(dados_br_total$DTNASC)  
dados_br_total$DTOBITO  <- ymd(dados_br_total$DTOBITO)
dados_br_total$IDADE2  <- floor(interval(start  =  dados_br_total$DTNASC , 
                                   end = dados_br_total$DTOBITO) / years(1)) # arrendondado p baixo - anos completos


#codigos UF

codigosUF <- read_table("Bases/var_extras_UF.txt")

codigosUF$codigoUF <- as.character(codigosUF$codigoUF)


# criando a var codigosUF

dados_br_total <- dados_br_total %>% 
  mutate(
    codigoUF = substr(as.character(CODMUNOCOR), 1, 2)
  )

dados_br_total <- inner_join(dados_br_total, codigosUF, by = "codigoUF")



dados_es_total <- dados_br_total %>%
  filter(Sigla == "ES")


#ler codigos das cids que tem a ver com uso de psicoativos
codigos_psic <- readLines("Bases/codigos_psic.txt")



#---------- Filtrar cids na variavel CAUSABAS

### BR PSIC
dados_br_psic <- dados_br_total%>%
  filter(str_detect(toupper(CAUSABAS), paste(codigos_psic, collapse = "|")))

### ES PSIC
dados_es_psic <- dados_es_total %>%
  filter(str_detect(toupper(CAUSABAS), paste(codigos_psic, collapse = "|")))








#--------- FILTRADO POR OUTRAS CIDS ---------------

#---------- Filtrar cids na variavel CAUSABAS_O

### BR PSIC
dados_br_causabaso <- dados_br_total%>%
  filter(str_detect(toupper(CAUSABAS_O), paste(codigos_psic, collapse = "|")))

### ES PSIC
dados_es_causabaso <- dados_es_total %>%
  filter(str_detect(toupper(CAUSABAS_O), paste(codigos_psic, collapse = "|")))


#---------- Filtrar cids na variavel LINHAA

### BR PSIC
dados_br_linhaa <- dados_br_total %>%
  filter(str_detect(toupper(LINHAA), paste(codigos_psic, collapse = "|")))

### ES PSIC
dados_es_linhaa <- dados_es_total %>%
  filter(str_detect(toupper(LINHAA), paste(codigos_psic, collapse = "|")))


#---------- Filtrar cids na variavel LINHAB

### BR PSIC
dados_br_linhab <- dados_br_total%>%
  filter(str_detect(toupper(LINHAB), paste(codigos_psic, collapse = "|")))

### ES PSIC
dados_es_linhab <- dados_es_total %>%
  filter(str_detect(toupper(LINHAB), paste(codigos_psic, collapse = "|")))


#---------- Filtrar cids na variavel LINHAC

### BR PSIC
dados_br_linhac <- dados_br_total%>%
  filter(str_detect(toupper(LINHAC), paste(codigos_psic, collapse = "|")))

### ES PSIC
dados_es_linhac <- dados_es_total %>%
  filter(str_detect(toupper(LINHAC), paste(codigos_psic, collapse = "|")))


#---------- Filtrar cids na variavel LINHAD

### BR PSIC
dados_br_linhad <- dados_br_total%>%
  filter(str_detect(toupper(LINHAD), paste(codigos_psic, collapse = "|")))

### ES PSIC
dados_es_linhad <- dados_es_total %>%
  filter(str_detect(toupper(LINHAD), paste(codigos_psic, collapse = "|")))


#---------- Filtrar cids na variavel LINHAII

### BR PSIC
dados_br_linhaii <- dados_br_total%>%
  filter(str_detect(toupper(LINHAII), paste(codigos_psic, collapse = "|")))

### ES PSIC
dados_es_linhaii <- dados_es_total %>%
  filter(str_detect(toupper(LINHAII), paste(codigos_psic, collapse = "|")))

