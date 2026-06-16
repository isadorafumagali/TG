library(readxl)
library(tidyr)
library(dplyr)
library(ggplot2)
library(janitor)
library(scales)
library(knitr)
library(kableExtra)
library(moments)
library(tseries)
library(lmtest)
library(sandwich)
library(corrplot)
library(psych)

########################################################################
############################## Área das UFs ############################
########################################################################

areas_ufs <- read_excel("C:/Users/carol/Desktop/areas ufs.xlsx")

#Converte apenas as colunas numéricas (de áreas) de km² para hectares mantendo "Estado" e "Ano"
areas_ufs <- areas_ufs |>
  mutate(across(where(is.numeric), ~ .x * 100))

#Converte formato largo para longo
areas_ufs_long <- areas_ufs |>
  pivot_longer(
    cols = -c(Estado),
    names_to = "Ano",
    values_to = "Area"
  )

#Converte Ano para numérico
areas_ufs_long$Ano <- as.numeric(areas_ufs_long$Ano)


#Calcula a média entre 2000 e 2010 por UF para assumir o valor referente a 2005
media_2005 <- areas_ufs_long |>
  filter(Ano %in% c(2000, 2010)) |>
  group_by(Estado) |>
  summarise(Ano = 2005, Area = mean(Area, na.rm = TRUE)) |>
  ungroup()



#Adiciona a média à base
areas_ufs_final <- areas_ufs_long |>
  bind_rows(media_2005) |>
#Remove os anos 2000 e 2010 originais
  filter(!(Ano %in% c(2000, 2010))) |>
  arrange(Estado, Ano)


#Gera gráfico de linhas da evolução das areas das UFs ao longo do tempo
#Valida a hipótese de que os estados se mantém aproximadamente do mesmo tamanho até hoje
ggplot(areas_ufs_final, aes(x = Ano, y = Area)) +
  geom_line(color = "steelblue") +
  scale_y_log10() +
  facet_wrap(~ Estado) +
  labs(
    title = "Gráficos de Linha da Evolução da Área das Unidades Federativas Entre as Décadas 1980 e 2010",
    x = "Ano",
    y = "Área (ha) [escala logarítmica]"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

#Calcula a média da área das UFs ao longo do tempo
areas_ufs_media <- areas_ufs_final |>
  group_by(Estado) |>
  summarise("Área média" = mean(Area, na.rm = TRUE)) |>
  arrange(desc("Área média"))
View(areas_ufs_media)

#Exibe a tabela formatada
areas_ufs_media |>
  kbl(
    caption = "Média da Área das UFs (em ha) ao longo do período de estudo",
    digits = 3,
    align = "lc"
  ) |>
  kable_styling(full_width = FALSE, position = "center")



########################################################################
######################## Área dos estabelecimentos #####################
########################################################################


areas_estab <- read_excel("C:/Users/carol/Desktop/Área total dos estabelecimentos.xlsx")


#Converte formato largo para longo
areas_estab_long <- areas_estab |>
  pivot_longer(
    cols = -c(Estado),
    names_to = "Ano",
    values_to = "Area_Estab"
  )

#Converte Ano para numérico
areas_estab_long$Ano <- as.numeric(areas_estab_long$Ano)

#Renomeia o ano
areas_estab_long <- areas_estab_long %>%
  mutate(
    Ano = case_when(
      Ano == 2006 ~ 2005,
      Ano == 2017 ~ 2015,
      TRUE ~ Ano
    )
  )


#Gera o gráfico de linhas da evolução das areas das UFs ao longo do tempo
ggplot(areas_estab_long, aes(x = Ano, y = Area_Estab)) +
  geom_line(color = "steelblue") +
  scale_y_log10() +
  facet_wrap(~ Estado) +
  labs(
    title = "Gráficos de Linha da Evolução da Área Ocupada por Estabelecimentos Agropecuários das Unidade Federativas Entre as Décadas 1980 e 2010",
    x = "Ano",
    y = "Área (ha) [escala logarítmica]"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

#Calcula a média da área ao longo do tempo para cada UF
areas_estab_media <- areas_estab_long |>
  group_by(Estado) |>
  summarise(
    Area_media = mean(Area_Estab, na.rm = TRUE)
  ) |>
  arrange(desc(Area_media))  #Ordena do maior para o menor


#Exibe a tabela formatada
areas_estab_media |>
  kbl(
    caption = "Média da área ocupada por estabelecimentos agropecuários em cada UF (1985–2017)",
    digits = 3,  #Arredonda para 3 casas decimais
    align = "lc",
    col.names = c("UF", "Área média (ha)")
  ) |>
  kable_styling(full_width = FALSE, position = "center")

########################################################################
##################### Proporção Area Agro #####################
########################################################################

proporcao_area <- areas_ufs_final %>%
  left_join(areas_estab_long, by = c("Estado", "Ano")) %>%
  mutate(
    prop_area_agro = Area_Estab / Area
  ) %>%
  select(-Area_Estab, -Area)

#Calcula a média da proporção ao longo do tempo por UF (índice de concentração de terras)
proporcao_media_por_UF <- proporcao_area |>
  group_by(Estado) |>
  summarise(
    Proporcao_media = mean(prop_area_agro, na.rm = TRUE)
  ) |>
  arrange(desc(Proporcao_media))

proporcao_media_por_UF |>
  kbl(
    caption = "Proporção Média da Área das UFs Ocupada por Estabelecimentos Agropecuários (1985–2017)",
    digits = 2,
    col.names = c("Estado", "Proporção Média (%)"),
    align = "lc"
  ) |>
  kable_styling(full_width = FALSE, position = "center")

#Garante Estado em ordem alfabética
proporcao_media_por_UF <- proporcao_media_por_UF |>
  mutate(Estado = factor(Estado, levels = sort(unique(Estado))))

#Gera a tabela de siglas
ufs_siglas <- tibble::tibble(
  Estado = c("Acre","Alagoas","Amapá","Amazonas","Bahia","Ceará","Distrito Federal",
             "Espírito Santo","Goiás","Maranhão","Mato Grosso","Mato Grosso do Sul",
             "Minas Gerais","Paraná","Paraíba","Pará","Pernambuco","Piauí",
             "Rio de Janeiro","Rio Grande do Norte","Rio Grande do Sul",
             "Rondônia","Roraima","Santa Catarina","São Paulo","Sergipe","Tocantins"),
  UF = c("AC","AL","AP","AM","BA","CE","DF","ES","GO","MA","MT","MS",
         "MG","PR","PB","PA","PE","PI","RJ","RN","RS","RO","RR","SC","SP","SE","TO")
)

proporcao_media_por_UF <- proporcao_media_por_UF |>
  left_join(ufs_siglas, by = "Estado")

colnames(proporcao_media_por_UF)
View(proporcao_media_por_UF)

#Gera o gráfico de barras verticais
ggplot(proporcao_media_por_UF,
       aes(x = UF, y = Proporcao_media)) +
  geom_col(fill = "steelblue") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Gráfico de Barras da Proporção Média da Área das Unidades Federativas que era Ocupada por Estabelecimentos Agropecuários Entre as Décadas 1980 e 2010",
    x = "UF",
    y = "Proporção Média (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1))

########################################################################
###################### Número dos estabelecimentos #####################
########################################################################
num_estabelecimentos <- read_excel("C:/Users/carol/Desktop/Número de estabelecimentos.xlsx")

#Converte formato largo para longo
num_estab_long <- num_estabelecimentos |>
  pivot_longer(
    cols = -c(Estado),
    names_to = "Ano",
    values_to = "Número_Estab"
  )

#Converte Ano para numérico
num_estab_long$Ano <- as.numeric(num_estab_long$Ano)

#Ajusta o Ano no número de estab. agropec. das ufs
num_estab <- num_estab_long
num_estab <- num_estab_long %>%
  mutate(
    Ano = case_when(
      Ano == 2006 ~ 2005,
      Ano == 2017 ~ 2015,
      TRUE ~ Ano
    )
  )

#Gera gráfico de linhas da evolução do numero de estab. nas UFs ao longo do tempo
ggplot(num_estab, aes(x = Ano, y = Número_Estab)) +
  geom_line(color = "steelblue") +
  scale_y_log10() +
  facet_wrap(~ Estado) +
  labs(
    title = "Gráficos de Linha da Evolução do Número de Estabelecimentos Agropecuários das Unidades Federativas Entre as Décadas de 1980 e 2010",
    x = "Ano",
    y = "Número (unidade) [escala logarítmica]"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

#Combina as duas bases pelo Estado e Ano
dados_combinados <- areas_estab_long |>
  inner_join(num_estab, by = c("Estado", "Ano"))


#Calcula o número médio  de estab. agrop. nas UFs ao longo do tempo
num_ufs_media <- dados_combinados |>
  group_by(Estado) |>
  summarise("Número médio" = mean(Número_Estab, na.rm = TRUE)) |>
  arrange(desc("Número médio"))


num_ufs_media |>
  kbl(
    caption = "Número Médio de Estabelecimentos Agropecuáros em cada UF",
    digits = 2,
    col.names = c("Estado", "Número Médio"),
    align = "lc"
  ) |>
  kable_styling(full_width = FALSE, position = "center")

########################################################################
#################### Tamanho médio dos estabelecimentos ################
########################################################################

#Calcula o tamanho médio dos estabelecimentos
#Área total dos estabelecimentos / Número de estabelecimentos
tamanho_medio <- dados_combinados |>
  mutate(Tamanho_medio_ha = Area_Estab / Número_Estab)



#Calcula o tamanho médio dos estabelecimentos ao longo do tempo
tamanho_medio_por_UF_across_time <- tamanho_medio |>
  group_by(Estado) |>
  summarise(
    Tamanho_medio_geral_ha = mean(Tamanho_medio_ha, na.rm = TRUE),
    .groups = "drop"
  )


tamanho_medio_por_UF_across_time |>
  kbl(
    caption = "Tamanho Médio das Áreas dos Estabelecimentos Agropecuáros em cada UF",
    digits = 2,
    col.names = c("Estado", "Tamanho Médio"),
    align = "lc"
  ) |>
  kable_styling(full_width = FALSE, position = "center")

#Gera o gráfico de linhas da evolução do tamanho médio dos estab. nas UFs ao longo do tempo
ggplot(tamanho_medio, aes(x = Ano, y = Tamanho_medio_ha)) +
  geom_line(color = "steelblue") +
  scale_y_log10() +
  facet_wrap(~ Estado) +
  labs(
    title = "Gráficos de Linha da Evolução do Tamanho Médio dos Estabelecimentos Agropecuários das Unidades Federativas Entre as Décadas 1980 e 2010",
    x = "Ano",
    y = "Tamanho Médio (ha) [escala logarítmica]"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


########################################################################
################## Corr Número Estab vs Área Estab. ####################
########################################################################

#Junta os dados de área das UFs e número de estabelecimentos
dados_area_num <- num_estab |>
  inner_join(areas_estab_long, by = c("Estado", "Ano"), suffix = c("_num", "_uf"))


#Cria uma lista de estados únicos
estados <- unique(dados_area_num$Estado)

#Gera um gráfico por estado
for (uf in estados) {
  dados_estado <- dados_area_num |> filter(Estado == uf)

  dispersao_area_num <- ggplot(dados_estado, aes(x = Area_Estab, y = Número_Estab)) +
    geom_point(color = "steelblue", size = 3, alpha = 0.7) +
    geom_smooth(method = "lm", se = FALSE, color = "darkred", linewidth = 0.8) +
    labs(
      title = paste("Gráfico de Dispersão entre o Número e a Área ocupada por Estabelecimentos Agropecuários", uf),
      x = "Área Ocupada por Estabelecimentos (ha)",
      y = "Número de Estabelecimentos"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
  print(dispersao_area_num)
}


#Calcula a correlação de Pearson entre área e número de estabelecimentos
correlacao_area_num <- dados_area_num |>
  group_by(Estado) |>
  summarise(
    Correlacao = cor(Area_Estab, Número_Estab, use = "complete.obs", method = "pearson")
  ) |>
  arrange(desc(Correlacao))


correlacao_area_num |>
  kbl(
    caption = "Correlação entre o Número e a Área ocupada por Estabelecimentos Agropecuários (por Estado)",
    digits = 3,
    col.names = c("Estado", "Correlação de Pearson"),
    align = "lc"
  ) |>
  kable_styling(full_width = FALSE, position = "center")


########################################################################
########################## Índice de Gini ##############################
########################################################################
ind_gini <- read_excel("C:/Users/carol/Desktop/Gini_Renda_per_capita.xlsx")


#Converte formato largo para longo
gini_long<- ind_gini |>
  pivot_longer(
    cols = -c(Estado),
    names_to = "Ano",
    values_to = "Gini"
  )

#Converte Ano para numérico
gini_long$Ano <- as.numeric(gini_long$Ano)


#Calcula a média do Gini por estado
gini_media <- gini_long |>
  group_by(Estado) |>
  summarise(Gini_medio = mean(Gini, na.rm = TRUE))


#Gera a tabela final
gini_media |>
  kbl(
    caption = "Índice de Gini Médio da Renda Per Capita das Unidades Federativas Entre as décadas 1980 e 2010",
    digits = 2,
    col.names = c("Estado", "Índice de Gini Médio"),
    align = "lc"
  ) |>
  kable_styling(full_width = FALSE, position = "center")


gini_long <- gini_long |>
  arrange(Estado, Ano)

ggplot(gini_long,
       aes(x = Ano,
           y = Gini,
           group = Estado)) +
  geom_line(color = "steelblue", linewidth = 0.8) +
  geom_point(color = "steelblue", size = 1.5) +
  facet_wrap(~ Estado, scales = "free_y") +
  labs(
    title = "Gráficos de Linha da Evolução do Índice de Gini das Unidades Federativas Entre as Décadas 1980 e 2010",
    x = "Ano",
    y = "Índice de Gini"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )


########################################################################
############################# PIB Estadual #############################
########################################################################
pib_estadual <- read_excel("C:/Users/carol/Desktop/PIB.xlsx")


#Converte formato largo para longo
pib_estadual_long<- pib_estadual |>
  pivot_longer(
    cols = -c(Estado),
    names_to = "Ano",
    values_to = "PIB_Total"
  )

#Converte Ano para numérico
pib_estadual_long$Ano <- as.numeric(pib_estadual_long$Ano)

#Calcula a média do PIB Estadual por estado
pib_estad_media <- pib_estadual_long |>
  group_by(Estado) |>
  summarise(pib_estadual_media = mean(PIB_Total, na.rm = TRUE)) |>
  arrange(Estado)

View(pib_estad_media)


#Gera a tabela final
pib_estad_media |>
  kbl(
    caption = "PIB Estadual Médio por Unidade da Federação das Décadas de 1980 à 2010",
    digits = 3,
    col.names = c("Estado", "PIB Estadual Médio"),
    align = "lc"
  ) |>
  kable_styling(full_width = FALSE, position = "center")


ggplot(pib_estadual_long,
       aes(x = Ano,
           y = PIB_Total,
           group = Estado)) +
  geom_line(color = "steelblue", linewidth = 0.8) +
  geom_point(color = "steelblue", size = 1.5) +
  facet_wrap(~ Estado, scales = "free_y") +
  labs(
    title = "Gráficos de Linha da Evolução do PIB das Unidades Federativas Entre as Décadas 1980 e 2010",
    x = "Ano",
    y = "PIB Total"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )



########################################################################
############################# PIB Estadual Rural #############################
########################################################################
pib_estadual_agro <- read_excel("C:/Users/carol/Desktop/PIB_agropecuário.xlsx")


#Converte formato largo para longo
pib_estadual_agro_long<- pib_estadual_agro |>
  pivot_longer(
    cols = -c(Estado),
    names_to = "Ano",
    values_to = "PIB_Agropecuário"
  )

#Converte Ano para numérico
pib_estadual_agro_long$Ano <- as.numeric(pib_estadual_agro_long$Ano)

#Calcula a média do PIB Agro Estadual por estado
pib_agro_estad_media <- pib_estadual_agro_long |>
  group_by(Estado) |>
  summarise(PIB_Agro_estad_medio = mean(PIB_Agropecuário, na.rm = TRUE)) |>
  arrange(Estado)


#Gera a tabela final
pib_agro_estad_media |>
  kbl(
    caption = "PIB Agropecuário Estadual Médio da Década de 1980 à 2010",
    digits = 3,
    col.names = c("Estado", "PIB Agropecuário Estadual Médio"),
    align = "lc"
  ) |>
  kable_styling(full_width = FALSE, position = "center")

ggplot(pib_estadual_agro_long,
       aes(x = Ano,
           y = PIB_Agropecuário,
           group = Estado)) +
  geom_line(color = "steelblue", linewidth = 0.8) +
  geom_point(color = "steelblue", size = 1.5) +
  facet_wrap(~ Estado, scales = "free_y") +
  labs(
    title = "Gráficos de Linha da Evolução do PIB Agropecuário das Unidades Federativas Entre as Décadas 1980 e 2010",
    x = "Ano",
    y = "PIB Total"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

########################################################################
############################# Pop residente Rural #############################
########################################################################
pop_residente_rural <- read_excel("C:/Users/carol/Desktop/Pop_residente_rural.xlsx")

pop_residente_rural <- pop_residente_rural |>
  filter(!is.na(Estado))

#Converte formato largo para longo
pop_residente_rural_long<- pop_residente_rural |>
  pivot_longer(
    cols = -c(Estado),
    names_to = "Ano",
    values_to = "População_Rural"
  )

#Converte Ano para numérico
pop_residente_rural_long$Ano <- as.numeric(pop_residente_rural_long$Ano)



#Calcula a média entre 2010 e 2022 por UF para assumir o valor referente a 2017
media_pop_residente_rural_2015 <- pop_residente_rural_long |>
  filter(Ano %in% c(2010, 2022)) |>
  group_by(Estado) |>
  summarise(Ano = 2015, População_Rural = mean(População_Rural, na.rm = TRUE)) |>
  ungroup()

#Adiciona a média ao dataset
pop_residente_rural_final <- pop_residente_rural_long |>
  bind_rows(media_pop_residente_rural_2015) |>
  #Removendo os anos 2010 e 2022 originais
  filter(!(Ano %in% c(2010, 2022))) |>
  arrange(Estado, Ano)


#Calcula a média da população residente total por estado
pop_residente_rural_final_media <- pop_residente_rural_final |>
  group_by(Estado) |>
  summarise(pop_residente_total_final_media = mean(População_Rural, na.rm = TRUE)) |>
  arrange(Estado)


#Gera a tabela final
pop_residente_rural_final_media |>
  kbl(
    caption = "População média residente rural em cada UF da Década de 1980 à 2010",
    digits = 2,
    col.names = c("Estado", "População média residente rural"),
    align = "lc"
  ) |>
  kable_styling(full_width = FALSE, position = "center")


pop_residente_rural_final <- pop_residente_rural_final %>%
  mutate(
    Ano = case_when(
      Ano == 1980 ~ 1985,
      Ano == 1996 ~ 1995,
      Ano == 2007 ~ 2005,
      Ano == 2017 ~ 2015,
      TRUE ~ Ano
    )
  )


ggplot(pop_residente_rural_final,
       aes(x = Ano,
           y = População_Rural,
           group = Estado)) +
  geom_line(color = "steelblue", linewidth = 0.8) +
  geom_point(color = "steelblue", size = 1.5) +
  facet_wrap(~ Estado, scales = "free_y") +
  labs(
    title = "Gráficos de Linha da Evolução da População Rural das Unidades Federativas Entre as Décadas 1980 e 2010",
    x = "Ano",
    y = "PIB Total"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )


########################################################################
############################# Pop residente Total #############################
########################################################################
pop_residente_total <- read_excel("C:/Users/carol/Desktop/Pop_residente_total.xlsx")


#Converte formato largo para longo
pop_residente_total_long<- pop_residente_total |>
  pivot_longer(
    cols = -c(Estado),
    names_to = "Ano",
    values_to = "População_Total"
  )

#Converte Ano para numérico
pop_residente_total_long$Ano <- as.numeric(pop_residente_total_long$Ano)


#Calcula a média entre 2010 e 2022 por UF para assumir o valor referente a 2017
media_pop_residente_total_2015 <- pop_residente_total_long |>
  filter(Ano %in% c(2010, 2022)) |>
  group_by(Estado) |>
  summarise(Ano = 2015, População_Total = mean(População_Total, na.rm = TRUE)) |>
  ungroup()



#Adiciona a média ao dataset
pop_residente_total_final <- pop_residente_total_long |>
  bind_rows(media_pop_residente_total_2015) |>
  #Remove os anos 2010 e 2022 originais
  filter(!(Ano %in% c(2010, 2022))) |>
  arrange(Estado, Ano)


#Calcula a média da pop. residente total por estado
pop_residente_total_final_media <- pop_residente_total_final |>
  group_by(Estado) |>
  summarise(pop_residente_total_final_media = mean(População_Total, na.rm = TRUE)) |>
  arrange(Estado)


#Gera a tabela final
pop_residente_total_final_media |>
  kbl(
    caption = "População média residente em cada UF da Década de 1980 à 2010",
    digits = 3,
    col.names = c("Estado", "População média residente"),
    align = "lc"
  ) |>
  kable_styling(full_width = FALSE, position = "center")



#pop total das ufs
pop_residente_total_final <- pop_residente_total_final %>%
  mutate(
    Ano = case_when(
      Ano == 1980 ~ 1985,
      Ano == 1996 ~ 1995,
      Ano == 2007 ~ 2005,
      Ano == 2017 ~ 2015,
      TRUE ~ Ano
    )
  )

ggplot(pop_residente_total_final,
       aes(x = Ano,
           y = População_Total,
           group = Estado)) +
  geom_line(color = "steelblue", linewidth = 0.8) +
  geom_point(color = "steelblue", size = 1.5) +
  facet_wrap(~ Estado, scales = "free_y") +
  labs(
    title = "Gráficos de Linha da Evolução da População das Unidades Federativas Entre as Décadas 1980 e 2010",
    x = "Ano",
    y = "PIB Total"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )


########################################################################
############################# IDHM #####################################
########################################################################
IDHM <- read_excel("C:/Users/carol/Desktop/IDHM.xlsx")


#Converte formato largo para longo
IDHM_long<- IDHM |>
  pivot_longer(
    cols = -c(Estado),
    names_to = "Ano",
    values_to = "IDHM"
  )

#Converte Ano para numérico
IDHM_long$Ano <- as.numeric(IDHM_long$Ano)



#Calcula a média entre 2000 e 2010 por UF para assumir o valor referente a 2005
media_IDHM_2005 <- IDHM_long |>
  filter(Ano %in% c(2000, 2010)) |>
  group_by(Estado) |>
  summarise(Ano = 2005, IDHM = mean(IDHM, na.rm = TRUE)) |>
  ungroup()


#Adiciona a média ao dataset
IDHM_final <- IDHM_long |>
  bind_rows(media_IDHM_2005) |>
  #Removendo os anos 2000 e 2010 originais
  filter(!(Ano %in% c(2000, 2010))) |>
  arrange(Estado, Ano)

#Calcula a média da pop. residente total por estado
IDHM_final_media <- IDHM_final |>
  group_by(Estado) |>
  summarise(IDHM_final_media = mean(IDHM, na.rm = TRUE)) |>
  arrange(Estado)


#Gera a tabela final
IDHM_final_media |>
  kbl(
    caption = "IDHM médio em cada UF da Década de 1980 à 2010",
    digits = 2,
    col.names = c("Estado", "IDHM médio"),
    align = "lc"
  ) |>
  kable_styling(full_width = FALSE, position = "center")


#IDHM dos municípios das ufs
IDHM_final <- IDHM_final %>%
  mutate(
    Ano = case_when(
      Ano == 1980 ~ 1985,
      Ano == 1991 ~ 1995,
      Ano == 2006 ~ 2005,
      Ano == 2007 ~ 2005,
      Ano == 2017 ~ 2015,
      TRUE ~ Ano
    )
  )

ggplot(IDHM_final,
       aes(x = Ano,
           y = IDHM,
           group = Estado)) +
  geom_line(color = "steelblue", linewidth = 0.8) +
  geom_point(color = "steelblue", size = 1.5) +
  facet_wrap(~ Estado, scales = "free_y") +
  labs(
    title = "Gráficos de Linha da Evolução do IDHM das Unidades Federativas Entre as Décadas 1980 e 2010",
    x = "Ano",
    y = "PIB Total"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

base <- IDHM_final

########################################################################
############################# Biomas ###################################
########################################################################
biomas <- read_excel("C:/Users/carol/Desktop/Bioma.xlsx")


########################################################################
############################# PBF ###################################
########################################################################
pbf <- read_excel("C:/Users/carol/Desktop/PBF.xlsx")

pbf_long <- pbf %>%
  pivot_longer(
    cols = -Estado,
    names_to = "Ano",
    values_to = "PBF"
  ) %>%
  mutate(Ano = as.numeric(Ano))

PBF <- pbf_long

########################################################################
############################# BPC ###################################
########################################################################
bpc <- read_excel("C:/Users/carol/Desktop/BPC.xlsx")

bpc_long <- bpc %>%
  pivot_longer(
    cols = -Estado,
    names_to = "Ano",
    values_to = "BPC"
  ) %>%
  mutate(Ano = as.numeric(Ano))

BPC <- bpc_long


########################################################################
############################# PREV. RURAL ##############################
########################################################################
prev_rural <- read_excel("C:/Users/carol/Desktop/Previdência Rural.xlsx")

prev_rural_long <- prev_rural %>%
  pivot_longer(
    cols = -Estado,
    names_to = "Ano",
    values_to = "PREV RURAL"
  ) %>%
  mutate(Ano = as.numeric(Ano))

Prev_Rural <- prev_rural_long


########################################################################
############################# PRONAF ##############################
########################################################################
pronaf <- read_excel("C:/Users/carol/Desktop/PRONAF.xlsx")

pronaf_long <- pronaf %>%
  pivot_longer(
    cols = -Estado,
    names_to = "Ano",
    values_to = "PRONAF"
  ) %>%
  mutate(Ano = as.numeric(Ano))

PRONAF <- pronaf_long

########################################################################
############################# BASE ##############################
#######################################################################
base <- base %>%
  left_join(biomas, by = "Estado")

base <- base %>%
  left_join(pop_residente_total_final, by=c("Estado","Ano")) %>%
  left_join(pop_residente_rural_final, by=c("Estado","Ano")) %>%
  left_join(pib_estadual_agro_long, by=c("Estado","Ano")) %>%
  left_join(pib_estadual_long, by=c("Estado","Ano")) %>%
  left_join(gini_long, by=c("Estado","Ano")) %>%
  left_join(num_estab, by=c("Estado","Ano")) %>%
  left_join(proporcao_area, by = c("Estado", "Ano")) %>%
  left_join(PBF, by=c("Estado","Ano")) %>%
  left_join(BPC, by=c("Estado","Ano")) %>%
  left_join(Prev_Rural, by=c("Estado","Ano")) %>%
  left_join(PRONAF, by=c("Estado","Ano"))  %>%
  left_join(areas_ufs_final, by=c("Estado","Ano")) %>%
  left_join(areas_estab_long, by=c("Estado","Ano"))
View(base)


########################################################################
############################# MODELO ##############################
#######################################################################

#install.packages("stargazer")
#install.packages("plm")
library(plm)
library(janitor)
library(stargazer)



base_modelagem <- base %>%
  clean_names() %>%
  mutate(
    #Garante que as indicadoras sejam tratadas como numéricas 0/1 ou fator
    pbf = as.numeric(pbf),
    bpc = as.numeric(bpc),
    prev_rural = as.numeric(prev_rural),
    pronaf = as.numeric(pronaf)
  )
View(base_modelagem)


dados_p <- pdata.frame(base_modelagem, index = c("estado", "ano"))

dados_p$proporcao_area <- unlist(dados_p$proporcao_area)
dados_p$prop_area_agro <- as.numeric(dados_p$prop_area_agro)


#log() para evitar distorções
#O intercepto será o valor esperado para um estado 100% Amazônia.
formula_proporcao_1 <- gini ~
  idhm +
  log(area) +
  log(area_estab) +
  log(pib_total) +
  log(pib_agropecuario) +
  log(populacao_total) +
  log(populacao_rural) +
  log(numero_estab) +
  prop_area_agro +
  pbf +
  prev_rural +
  bpc +
  pronaf +
  caatinga +
  cerrado +
  mata +
  pampa +
  pantanal
mod_aleatorio_1 <- plm(formula_proporcao_1, data = dados_p, model = "random")
summary(mod_aleatorio_1)


formula_proporcao_2 <- gini ~
  idhm +
  log(pib_total) +
  log(pib_agropecuario) +
  log(populacao_total) +
  log(populacao_rural) +
  log(numero_estab) +
  prop_area_agro +
  pbf +
  prev_rural +
  caatinga +
  cerrado +
  mata +
  pampa +
  pantanal
mod_aleatorio_2 <- plm(formula_proporcao_2, data = dados_p, model = "random")
summary(mod_aleatorio_2)


formula_proporcao_3 <- gini ~  idhm +
  log(pib_agropecuario) +
  log(populacao_rural) +
  log(numero_estab) +
  prop_area_agro+
  pbf + prev_rural +
  caatinga + cerrado + mata + pampa + pantanal
mod_aleatorio_3 <- plm(formula_proporcao_3, data = dados_p, model = "random")
summary(mod_aleatorio_3)


formula_proporcao_4 <- gini ~  idhm +
  log(populacao_rural) +
  log(populacao_total) +
  log(pib_total) +
  log(numero_estab) +
  pbf
mod_aleatorio_4 <- plm(formula_proporcao_4, data = dados_p, model = "random")
summary(mod_aleatorio_4)



#Modelo final
formula_final <- gini ~ idhm +
  log(populacao_total) +
  log(pib_total) +
  log(numero_estab) +
  pbf
mod_aleatorio_final <- plm(formula_final, data = dados_p, model = "random")
summary(mod_aleatorio_final)



mod_fixo <- plm(formula_final, data = dados_p, model = "within")
summary(mod_fixo)

phtest(mod_aleatorio_final, mod_fixo) #Hausman #Compara efeitos fixos vs aleatorios

bptest(mod_aleatorio_final) #Breusch-Pagan #Heterocedasticidade

pbgtest(mod_aleatorio_final) #Wooldridge #Correlação serial

pcdtest(mod_aleatorio_final, test = "cd") #Teste de Pesaran para dependência transversal


bptest(mod_fixo)

pbgtest(mod_fixo)

pcdtest(mod_fixo, test = "cd")

#Plota o histograma com curva de densidade
residuos_re <- residuals(mod_aleatorio_final)
hist(residuos_re, breaks = 20, main = "Histograma dos Resíduos do Modelo Final de Efeitos Aleatórios", col = "lightblue", probability = TRUE)
lines(density(residuos_re), col = "red", lwd = 2)

residuos_fe <- residuals(mod_fixo)
hist(residuos_fe, breaks = 20, main = "Histograma dos Resíduos do Modelo Final de Efeitos Fixos", col = "lightblue", probability = TRUE)
lines(density(residuos_fe), col = "red", lwd = 2)

#Teste de Assimetria
#install.packages("moments")
library(moments)
skewness(residuos_re)
skewness(residuos_fe)

#Teste de Shapiro-Wilk
shapiro.test(residuos_re)
shapiro.test(residuos_fe)

#Teste de Jarque-Bera
#install.packages("tseries")
library(tseries)
jarque.bera.test(residuos_re)
jarque.bera.test(residuos_fe)


#Gera o Q-Q Plot
qqnorm(residuos_re)
qqline(residuos_re, col = "red")

qqnorm(residuos_fe)
qqline(residuos_fe, col = "red")


#library(lmtest)
#library(sandwich)
################################################################################
#Estima os coeficientes com correção de Driscoll-Kraay (ajustado para dependência espacial/temporal)
coef_robustos_re <- coeftest(mod_aleatorio_final, vcov = vcovSCC(mod_aleatorio_final))

print(coef_robustos_re)

coef_robustos_fe <- coeftest(mod_fixo, vcov = vcovSCC(mod_fixo))

print(coef_robustos_fe)



#Estima os erros-padrão robustos com cluster por UF
vcov_cluster_uf <- vcovHC(mod_aleatorio_final,
                          method = "arellano",
                          type = "HC1",
                          cluster = "group")

#Aplica os coeficientes robustos no teste t
coef_cluster_uf <- coeftest(mod_aleatorio_final, vcov = vcov_cluster_uf)
print(coef_cluster_uf)


vcov_cluster_uf_fe <- vcovHC(mod_fixo,
                             method = "arellano",
                             type = "HC1",
                             cluster = "group")

#Aplica os coeficientes robustos no teste t
coef_cluster_uf_fe <- coeftest(mod_fixo, vcov = vcov_cluster_uf)
print(coef_cluster_uf_fe)


#Estima os erros-padrão robustos com cluster duplo: por UF e por Tempo simultaneamente
vcov_cluster_duplo <- vcovHC(mod_aleatorio_final,
                             method = "arellano",
                             type = "HC1",
                             cluster = c("group", "time"))

#Aplica a matriz de covariância corrigida no teste t de coeficientes
coef_cluster_duplo <- coeftest(mod_aleatorio_final, vcov = vcov_cluster_duplo)
print(coef_cluster_duplo)




vcov_cluster_duplo_fe <- vcovHC(mod_fixo,
                                method = "arellano",
                                type = "HC1",
                                cluster = c("group", "time"))

#Aplica a matriz de covariância corrigida no teste t de coeficientes
coef_cluster_duplo_fe <- coeftest(mod_fixo, vcov = vcov_cluster_duplo)
print(coef_cluster_duplo_fe)


#Verifica o theta
ercomp(mod_aleatorio_final)
