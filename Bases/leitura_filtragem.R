
# instalar e carregar pacotes

pacman::p_load("tidyverse", "janitor", "Hmisc",
               "summarytools", "DataExplorer", 
               "knitr","readr", "dplyr", "glue", "scales",
               "geobr", "sf", "latex2exp", "patchwork")


#LER E SALVAR DADOS TOTAIS BR


dados_br_total <- c()

for (ano in 2013:2022) {
  
  df <- readRDS(paste0("base_de_dados/dados_", ano,".rds"))
  dados_br_total <- bind_rows(df,dados_br_total)
  
}

#codigos UF

codigosUF <- read_table("codigosUF.txt")

codigosUF$codigoUF <- as.character(codigosUF$codigoUF)

# criando a var codigosUF
dados_br_total <- dados_br_total %>% 
  mutate(
    codigoUF = substr(as.character(CODMUNOCOR), 1, 2)
  )

dados_br_total <- inner_join(dados_br_total, codigosUF, by = "codigoUF")



dados_es_total <- dados_br_total %>%
  filter(Sigla == "ES")


### BR PSIC
dados_br_psic <- dados_br_total[grepl("F1", dados_br_total$CAUSABAS, ignore.case = TRUE),]

### ES PSIC

dados_es_psic <- dados_es_total[grepl("F1", dados_es_total$CAUSABAS, ignore.case = TRUE),]




