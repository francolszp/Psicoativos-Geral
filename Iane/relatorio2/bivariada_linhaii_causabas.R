# ANALISE BIVARIADA RELACIONANDO AS VARIÁVEIS CID LINHAII E CAUSABAS

#necesário rodar o código "filtragem.R" antes

library(plotly)
library(htmlwidgets)
library(stringr)


#------------- QUAL A CAUSA BÁSICA DE MORTE MAIS COMUM EM MORTES QUE TIVERAM A CONTRIBUIÇÃO DE PSICOATIVOS? --------

#definir UTF-8
Sys.setlocale("LC_CTYPE", "en_US.UTF-8")


# Função para categorizar os códigos CID, considerando apenas os três primeiros caracteres
categorizar_cid <- function(causa) {
  # Extrair os três primeiros caracteres
  causa_principal <- substr(causa, 1, 3)
  
  if (is.na(causa_principal)) {
    return(NA)  # Retorna NA se o valor for ausente
  } else if (causa_principal >= "A00" & causa_principal <= "B99") {
    return("Doenças infecciosas e parasitárias (A00-B99)")
  } else if (causa_principal >= "C00" & causa_principal <= "D48") {
    return("Neoplasias (tumores) (C00-D48)")
  } else if (causa_principal >= "D50" & causa_principal <= "D89") {
    return("Doenças do sangue e órgãos hematopoéticos (D50-D89)")
  } else if (causa_principal >= "E00" & causa_principal <= "E90") {
    return("Doenças endócrinas, metabólicas e nutricionais  (E00-E90)")
  } else if (causa_principal >= "F00" & causa_principal <= "F99") {
    return("Transtornos mentais e comportamentais  (F00-F99)")
  } else if (causa_principal >= "G00" & causa_principal <= "G99") {
    return("Doenças do sistema nervoso  (G00-G99)")
  } else if (causa_principal >= "H00" & causa_principal <= "H59") {
    return("Doenças dos olhos  (H00-H59)")
  } else if (causa_principal >= "H60" & causa_principal <= "H95") {
    return("Doenças do ouvido e apófise mastoide  (H60-H95")
  } else if (causa_principal >= "I00" & causa_principal <= "I99") {
    return("Doenças do aparelho circulatório  (I00-I99)")
  } else if (causa_principal >= "J00" & causa_principal <= "J99") {
    return("Doenças do aparelho respiratório  (J00-J99)")
  } else if (causa_principal >= "K00" & causa_principal <= "K93") {
    return("Doenças do aparelho digestivo  (K00-K93)")
  } else if (causa_principal >= "L00" & causa_principal <= "L99") {
    return("Doenças da pele e tecido subcutâneo  (L00-L99)")
  } else if (causa_principal >= "M00" & causa_principal <= "M99") {
    return("Doenças do sistema osteomuscular (M00-M99)")
  } else if (causa_principal >= "N00" & causa_principal <= "N99") {
    return("Doenças do aparelho geniturinário (N00-N99)")
  } else if (causa_principal >= "O00" & causa_principal <= "O99") {
    return("Gravidez, parto e puerpério (O00-O99)")
  } else if (causa_principal >= "P00" & causa_principal <= "P96") {
    return("Afecções perinatais (P00-P96)")
  } else if (causa_principal >= "Q00" & causa_principal <= "Q99") {
    return("Malformações congênitas (Q00-Q99)")
  } else if (causa_principal >= "R00" & causa_principal <= "R99") {
    return("Sinais e achados anormais (R00-R99)")
  } else if (causa_principal >= "S00" & causa_principal <= "T98") {
    return("Lesões e envenenamentos (S00-T98)")
  } else if (causa_principal >= "V01" & causa_principal <= "Y98") {
    return("Causas externas de morbidade e mortalidade")
  } else if (causa_principal >= "Z00" & causa_principal <= "Z99") {
    return("Fatores que influenciam o estado de saúde (Z00-Z99)")
  } else {
    return("Categoria desconhecida")
  }
}

# Aplicar a função de categorização
dados_br_linhaii$categoria_causabas <- sapply(dados_br_linhaii$CAUSABAS, categorizar_cid)

#Agrupar dados da coluna categoria_causabas
linhaii_causabas <- dados_br_linhaii %>%
  group_by(categoria_causabas) %>%
  summarize(quantidade = n()) %>%
  arrange(desc(quantidade))

# Adicionar quebras de linha usando str_wrap
linhaii_causabas$categoria_causabas <- str_wrap(linhaii_causabas$categoria_causabas, width = 75)

# Reordenar a variável 'categoria_causabas' como um fator ordenado
linhaii_causabas$categoria_causabas <- factor(
  linhaii_causabas$categoria_causabas, 
  levels = linhaii_causabas$categoria_causabas[order(linhaii_causabas$quantidade, decreasing = FALSE)]
)


#plotar grafico plotly com as cids no eixo y e a quantidade do eixo x
gbarra_linhaii_causabas <- plot_ly(
  data = linhaii_causabas,
  x = ~quantidade,
  y = ~categoria_causabas,
  type = 'bar',
  orientation = 'h',  # 'h' para horizontal
  marker = list(color = '#be3100')
)

# Ajustar layout
gbarra_linhaii_causabas <-gbarra_linhaii_causabas %>%
  layout(
    title = list(
      text = 'Causas Básicas de Mortes que tiveram os Psicoativos como Contribuintes - Brasil',  
      font = list(size = 16)  
    ),
    xaxis = list(title = 'Número de Óbitos'),
    yaxis = list(
      title = 'Causa Básica do Óbito',
      standoff = 40
    )
  )

# Salvar o gráfico HTML
pasta <- "graficos_comparacao/gbarra_linhaii_causabas.html"
saveWidget(gbarra_linhaii_causabas, pasta)


#-------------- Analise mais aprofundada CID I00-I99 -----------------

df_i00ai99 <- dados_br_linhaii %>%
  filter(categoria_causabas == "Doenças do aparelho circulatório  (I00-I99)")

# Função para categorizar os códigos CID I00-I99 , considerando apenas os três primeiros caracteres
categorizar_cidi00ai99 <- function(causa) {
  # Extrair os três primeiros caracteres
  causa_principal <- substr(causa, 1, 3)
  
  if (causa_principal >= "I00" & causa_principal <= "I02") {
    return("Febre reumática aguda")
  } else if (causa_principal >= "I05" & causa_principal <= "I09") {
    return("Doenças reumáticas crônicas do coração")
  } else if (causa_principal >= "I10" & causa_principal <= "I15") {
    return("Doenças hipertensivas")
  } else if (causa_principal >= "I20" & causa_principal <= "I25") {
    return("Doenças isquêmicas do coração")
  } else if (causa_principal >= "I26" & causa_principal <= "I28") {
    return("Doenças cardíaca pulmonar e da circulação pulmonar")
  } else if (causa_principal >= "I30" & causa_principal <= "I52") {
    return("Outras formas de doença do coração")
  } else if (causa_principal >= "I60" & causa_principal <= "I69") {
    return("Doenças cerebrovasculares")
  } else if (causa_principal >= "I70" & causa_principal <= "I79") {
    return("Doenças das artérias, das arteríolas e dos capilares")
  } else if (causa_principal >= "I80" & causa_principal <= "I89") {
    return("Doenças das veias, dos vasos linfáticos e dos gânglios linfáticos, não classificadas em outra parte")
  } else if (causa_principal >= "I95" & causa_principal <= "I99") {
    return("Outros transtornos, e os não especificados do aparelho circulatório")
  } else {
    return("Categoria desconhecida")
  }
}

# Aplicar a função de categorização
df_i00ai99$categoria_i00ai99 <- sapply(df_i00ai99$CAUSABAS, categorizar_cidi00ai99)


#Agrupar dados da coluna categoria_causabas
group_i00ai99 <- df_i00ai99 %>%
  group_by(categoria_i00ai99) %>%
  summarize(quantidade = n()) %>%
  arrange(desc(quantidade))

# Adicionar quebras de linha usando str_wrap
group_i00ai99$categoria_i00ai99 <- str_wrap(group_i00ai99$categoria_i00ai99, width = 75) 

# Reordenar a variável 
group_i00ai99$categoria_i00ai99 <- factor(
  group_i00ai99$categoria_i00ai99, 
  levels = group_i00ai99$categoria_i00ai99[order(group_i00ai99$quantidade, decreasing = FALSE)]
)

#plotar grafico plotly com as cids no eixo y e a quantidade do eixo x
gbarra_i00ai99 <- plot_ly(
  data = group_i00ai99,
  x = ~quantidade,
  y = ~categoria_i00ai99,
  type = 'bar',
  orientation = 'h'  # 'h' para horizontal
)

# Ajustar layout
gbarra_i00ai99 <-gbarra_i00ai99 %>%
  layout(
    title = list(
      text = 'Mortes por Doenças do Aparelho Circulatório que tiveram os Psicoativos como Contribuintes (BR)',  
      font = list(size = 16)  
    ),
    xaxis = list(title = 'Número de Óbitos'),
    yaxis = list(
      title = 'Causa Básica do Óbito',
      standoff = 40
    )
  )

# Salvar o gráfico HTML
pasta <- "graficos_comparacao/gbarra_i00ai99.html"
saveWidget(gbarra_i00ai99, pasta)



#-------------- Analise mais aprofundada CID J00-J99 -----------------

df_j00aj99 <- dados_br_linhaii %>%
  filter(categoria_causabas == "Doenças do aparelho respiratório  (J00-J99)")

# Função para categorizar os códigos CID J00-J99 , considerando apenas os três primeiros caracteres
categorizar_cidj00aj99 <- function(causa) {
  # Extrair os três primeiros caracteres
  causa_principal <- substr(causa, 1, 3)
  
  if (causa_principal >= "J00" & causa_principal <= "J06") {
    return("Infecções agudas das vias aéreas superiores")
  } else if (causa_principal >= "J09" & causa_principal <= "J18") {
    return("Influenza [gripe] e pneumonia")
  } else if (causa_principal >= "J20" & causa_principal <= "J22") {
    return("Outras infecções agudas das vias aéreas inferiores")
  } else if (causa_principal >= "J30" & causa_principal <= "J39") {
    return("Outras doenças das vias aéreas superiores")
  } else if (causa_principal >= "J40" & causa_principal <= "J47") {
    return("Doenças crônicas das vias aéreas inferiores")
  } else if (causa_principal >= "J60" & causa_principal <= "J70") {
    return("Doenças pulmonares devidas a agentes externos")
  } else if (causa_principal >= "J80" & causa_principal <= "J84") {
    return("Outras doenças respiratórias que afetam principalmente o interstício")
  } else if (causa_principal >= "J85" & causa_principal <= "J86") {
    return("Afecções necróticas e supurativas das vias aéreas inferiores")
  } else if (causa_principal >= "J90" & causa_principal <= "J94") {
    return("Outras doenças da pleura")
  } else if (causa_principal >= "J95" & causa_principal <= "J99") {
    return("Outras doenças do aparelho respiratório")
  } else {
    return("Categoria desconhecida")
  }
}

# Aplicar a função de categorização
df_j00aj99$categoria_j00aj99 <- sapply(df_j00aj99$CAUSABAS, categorizar_cidj00aj99)


#Agrupar dados da coluna categoria_causabas
group_j00aj99 <- df_j00aj99 %>%
  group_by(categoria_j00aj99) %>%
  summarize(quantidade = n()) %>%
  arrange(desc(quantidade))

# Adicionar quebras de linha usando str_wrap
group_j00aj99$categoria_j00aj99 <- str_wrap(group_j00aj99$categoria_j00aj99, width = 50) 

# Reordenar a variável 
group_j00aj99$categoria_j00aj99 <- factor(
  group_j00aj99$categoria_j00aj99, 
  levels = group_j00aj99$categoria_j00aj99[order(group_j00aj99$quantidade, decreasing = FALSE)]
)

#plotar grafico plotly com as cids no eixo y e a quantidade do eixo x
gbarra_j00aj99 <- plot_ly(
  data = group_j00aj99,
  x = ~quantidade,
  y = ~categoria_j00aj99,
  type = 'bar',
  orientation = 'h'  # 'h' para horizontal
)

# Ajustar layout
gbarra_j00aj99 <-gbarra_j00aj99 %>%
  layout(
      title = list(
        text = 'Mortes por Doenças do Aparelho Respiratório que tiveram os Psicoativos como Contribuintes (BR)',  
        font = list(size = 16)  
      ),
    xaxis = list(title = 'Número de Óbitos'),
    yaxis = list(
      title = 'Causa Básica do Óbito',
      standoff = 40
    )
  )

# Salvar o gráfico HTML
pasta <- "graficos_comparacao/gbarra_j00aj99.html"
saveWidget(gbarra_j00aj99, pasta)



#-------------- Analise mais aprofundada CID C00-D48 -----------------

df_c00ad48 <- dados_br_linhaii %>%
  filter(categoria_causabas == "Neoplasias (tumores) (C00-D48)")

# Função para categorizar os códigos CID C00-D48 , considerando apenas os três primeiros caracteres
categorizar_cidc00ad48 <- function(causa) {
  # Extrair os três primeiros caracteres
  causa_principal <- substr(causa, 1, 3)
  
  if (causa_principal >= "C00" & causa_principal <= "C14") {
    return("Neoplasias malignas do lábio, cavidade oral e faringe")
  } else if (causa_principal >= "C15" & causa_principal <= "C26") {
    return("Neoplasias malignas dos órgãos digestivos")
  } else if (causa_principal >= "C30" & causa_principal <= "C39") {
    return("Neoplasias malignas do aparelho respiratório e dos órgãos intratorácicos")
  } else if (causa_principal >= "C40" & causa_principal <= "C41") {
    return("Neoplasias malignas dos ossos e das cartilagens articulares")
  } else if (causa_principal >= "C43" & causa_principal <= "C44") {
    return("Melanoma e outras(os) neoplasias malignas da pele")
  } else if (causa_principal >= "C45" & causa_principal <= "C49") {
    return("Neoplasias malignas do tecido mesotelial e tecidos moles")
  } else if (causa_principal >= "C50" & causa_principal <= "C50") {
    return("Neoplasias malignas da mama")
  } else if (causa_principal >= "C51" & causa_principal <= "C58") {
    return("Neoplasias malignas dos órgãos genitais femininos")
  } else if (causa_principal >= "C60" & causa_principal <= "C63") {
    return("Neoplasias malignas dos órgãos genitais masculinos")
  } else if (causa_principal >= "C64" & causa_principal <= "C68") {
    return("Neoplasias malignas do trato urinário")
  } else if (causa_principal >= "C69" & causa_principal <= "C72") {
    return("Neoplasias malignas dos olhos, do encéfalo e de outras partes do sistema nervoso central")
  } else if (causa_principal >= "C73" & causa_principal <= "C75") {
    return("Neoplasias malignas da tireóide e de outras glândulas endócrinas")
  } else if (causa_principal >= "C76" & causa_principal <= "C80") {
    return("Neoplasias malignas de localizações mal definidas, secundárias e de localizações não especificadas")
  } else if (causa_principal >= "C81" & causa_principal <= "C96") {
    return("Neoplasias [tumores] malignas(os), declaradas ou presumidas como primárias, dos tecidos linfático, hematopoético e tecidos correlatos")
  } else if (causa_principal >= "C97" & causa_principal <= "C97") {
    return("Neoplasias malignas de localizações múltiplas independentes (primárias)")
  } else if (causa_principal >= "D00" & causa_principal <= "D09") {
    return("Neoplasias [tumores] in situ")
  } else if (causa_principal >= "D10" & causa_principal <= "D36") {
    return("Neoplasias [tumores] benignas(os)")
  } else if (causa_principal >= "D37" & causa_principal <= "D48") {
    return("Neoplasias [tumores] de comportamento incerto ou desconhecido")
  } else {
    return("Categoria desconhecida")
  }
}

# Aplicar a função de categorização
df_c00ad48$categoria_c00ad48 <- sapply(df_c00ad48$CAUSABAS, categorizar_cidc00ad48)


#Agrupar dados da coluna categoria_causabas
group_c00ad48 <- df_c00ad48 %>%
  group_by(categoria_c00ad48) %>%
  summarize(quantidade = n()) %>%
  arrange(desc(quantidade))

# Adicionar quebras de linha usando str_wrap
group_c00ad48$categoria_c00ad48 <- str_wrap(group_c00ad48$categoria_c00ad48, width = 80)

# Reordenar a variável 
group_c00ad48$categoria_c00ad48 <- factor(
  group_c00ad48$categoria_c00ad48, 
  levels = group_c00ad48$categoria_c00ad48[order(group_c00ad48$quantidade, decreasing = FALSE)]
)

#plotar grafico plotly com as cids no eixo y e a quantidade do eixo x
gbarra_c00ad48 <- plot_ly(
  data = group_c00ad48,
  x = ~quantidade,
  y = ~categoria_c00ad48,
  type = 'bar',
  orientation = 'h'  # 'h' para horizontal
)

# Ajustar layout
gbarra_c00ad48 <-gbarra_c00ad48 %>%
  layout(
    title = list(
      text = 'Neoplasias (tumores) que tiveram os Psicoativos como Contribuintes (BR)',  
      font = list(size = 16)  
    ),
    xaxis = list(title = 'Número de Óbitos'),
    yaxis = list(
      title = 'Causa Básica do Óbito',
      standoff = 40
    )
  )


# Salvar o gráfico HTML
pasta <- "graficos_comparacao/gbarra_c00ad48.html"
saveWidget(gbarra_c00ad48, pasta)





#-------------- Analise mais aprofundada CID K00-K93 -----------------

df_k00ak93 <- dados_br_linhaii %>%
  filter(categoria_causabas == "Doenças do aparelho digestivo  (K00-K93)")

# Função para categorizar os códigos CID J00-J99 , considerando apenas os três primeiros caracteres
categorizar_cidk00ak93 <- function(causa) {
  # Extrair os três primeiros caracteres
  causa_principal <- substr(causa, 1, 3)
  
  if (causa_principal >= "K00" & causa_principal <= "K14") {
    return("Doenças da cavidade oral, das glândulas salivares e dos maxilares")
  } else if (causa_principal >= "K20" & causa_principal <= "K31") {
    return("Doenças do esôfago, do estômago e do duodeno")
  } else if (causa_principal >= "K35" & causa_principal <= "K38") {
    return("Doenças do apêndice")
  } else if (causa_principal >= "K40" & causa_principal <= "K46") {
    return("Hérnias")
  } else if (causa_principal >= "K50" & causa_principal <= "K52") {
    return("Enterites e colites não-infecciosas")
  } else if (causa_principal >= "K55" & causa_principal <= "K63") {
    return("Outras doenças dos intestinos")
  } else if (causa_principal >= "K65" & causa_principal <= "K67") {
    return("Doenças do peritônio")
  } else if (causa_principal >= "K70" & causa_principal <= "K77") {
    return("Doenças do fígado")
  } else if (causa_principal >= "K80" & causa_principal <= "K87") {
    return("Transtornos da vesícula biliar, das vias biliares e do pâncreas")
  } else if (causa_principal >= "K90" & causa_principal <= "K93") {
    return("Outras doenças do aparelho digestivo")
  } else {
    return("Categoria desconhecida")
  }
}

# Aplicar a função de categorização
df_k00ak93$categoria_k00ak93 <- sapply(df_k00ak93$CAUSABAS, categorizar_cidk00ak93)


#Agrupar dados da coluna categoria_causabas
group_k00ak93 <- df_k00ak93 %>%
  group_by(categoria_k00ak93) %>%
  summarize(quantidade = n()) %>%
  arrange(desc(quantidade))

# Adicionar quebras de linha usando str_wrap
group_k00ak93$categoria_k00ak93 <- str_wrap(group_k00ak93$categoria_k00ak93, width = 50)

# Reordenar a variável 
group_k00ak93$categoria_k00ak93 <- factor(
  group_k00ak93$categoria_k00ak93, 
  levels = group_k00ak93$categoria_k00ak93[order(group_k00ak93$quantidade, decreasing = FALSE)]
)

#plotar grafico plotly com as cids no eixo y e a quantidade do eixo x
gbarra_k00ak93 <- plot_ly(
  data = group_k00ak93,
  x = ~quantidade,
  y = ~categoria_k00ak93,
  type = 'bar',
  orientation = 'h'  # 'h' para horizontal
)

# Ajustar layout
gbarra_k00ak93 <-gbarra_k00ak93 %>%
  layout(
    title = list(
      text = 'Mortes por Doenças do Aparelho Digestivo que tiveram os Psicoativos como Contribuintes (BR)',  
      font = list(size = 16)  
    ),
    xaxis = list(title = 'Número de Óbitos'),
    yaxis = list(
      title = 'Causa Básica do Óbito',
      standoff = 40
    )
  )

# Salvar o gráfico HTML
pasta <- "graficos_comparacao/gbarra_k00ak93.html"
saveWidget(gbarra_k00ak93, pasta)



#-------------- Analise mais aprofundada CID F00-F99 -----------------

df_f00af99 <- dados_br_linhaii %>%
  filter(categoria_causabas == "Transtornos mentais e comportamentais  (F00-F99)")

# Função para categorizar os códigos CID F00-F99 , considerando apenas os três primeiros caracteres
categorizar_cidf00af99 <- function(causa) {
  # Extrair os três primeiros caracteres
  causa_principal <- substr(causa, 1, 3)
  
  if (causa_principal >= "F00" & causa_principal <= "F09") {
    return("Transtornos mentais orgânicos, inclusive os sintomáticos")
  } else if (causa_principal >= "F10" & causa_principal <= "F19") {
    return("Transtornos mentais e comportamentais devidos ao uso de substância psicoativa")
  } else if (causa_principal >= "F20" & causa_principal <= "F29") {
    return("Esquizofrenia, transtornos esquizotípicos e transtornos delirantes")
  } else if (causa_principal >= "F30" & causa_principal <= "F39") {
    return("Transtornos do humor [afetivos]")
  } else if (causa_principal >= "F40" & causa_principal <= "F48") {
    return("Transtornos neuróticos, transtornos relacionados com o “stress” e transtornos somatoformes")
  } else if (causa_principal >= "F50" & causa_principal <= "F59") {
    return("Síndromes comportamentais associadas a disfunções fisiológicas e a fatores físicos")
  } else if (causa_principal >= "F60" & causa_principal <= "F69") {
    return("Transtornos da personalidade e do comportamento do adulto")
  } else if (causa_principal >= "F70" & causa_principal <= "F79") {
    return("Retardo mental")
  } else if (causa_principal >= "F80" & causa_principal <= "F89") {
    return("Transtornos do desenvolvimento psicológico")
  } else if (causa_principal >= "F90" & causa_principal <= "F98") {
    return("Transtornos do comportamento e transtornos emocionais que aparecem habitualmente durante a infância ou a adolescência")
  } else if (causa_principal >= "F99" & causa_principal <= "F99") {
    return("Transtorno mental não especificado")
  } else {
    return("Categoria desconhecida")
  }
}

# Aplicar a função de categorização
df_f00af99$categoria_f00af99 <- sapply(df_f00af99$CAUSABAS, categorizar_cidf00af99)


#Agrupar dados da coluna categoria_causabas
group_f00af99 <- df_f00af99 %>%
  group_by(categoria_f00af99) %>%
  summarize(quantidade = n()) %>%
  arrange(desc(quantidade))

# Adicionar quebras de linha usando str_wrap
group_f00af99$categoria_f00af99 <- str_wrap(group_f00af99$categoria_f00af99, width = 75)

# Reordenar a variável 
group_f00af99$categoria_f00af99 <- factor(
  group_f00af99$categoria_f00af99, 
  levels = group_f00af99$categoria_f00af99[order(group_f00af99$quantidade, decreasing = FALSE)]
)

#plotar grafico plotly com as cids no eixo y e a quantidade do eixo x
gbarra_f00af99 <- plot_ly(
  data = group_f00af99,
  x = ~quantidade,
  y = ~categoria_f00af99,
  type = 'bar',
  orientation = 'h'  # 'h' para horizontal
)

# Ajustar layout
gbarra_f00af99 <-gbarra_f00af99 %>%
  layout(
    title = list(
      text = 'Mortes por Transtornos Mentais e Comportamentais que tiveram os Psicoativos como Contribuintes (BR)',  
      font = list(size = 16)  
    ),
    xaxis = list(title = 'Número de Óbitos'),
    yaxis = list(
      title = 'Causa Básica do Óbito',
      standoff = 40
    )
  )

# Salvar o gráfico HTML
pasta <- "graficos_comparacao/gbarra_f00af99.html"
saveWidget(gbarra_f00af99, pasta)
  
