library(tidyverse)
library(readxl)
library(broom)
library(lmtest)

################################################################################
###########################Interpolação Pop Total##############################
################################################################################

pop_total_cru <- read_excel("C:/Users/carol/Desktop/pop_total_cru.xlsx")
View(pop_total_cru)

#Identifica o "Ano de Corte" (Primeiro ano com dados separados de TO)
ano_corte <- 1991

#Converte formato largo para longo
pop_total_cru_long<- pop_total_cru |>
  pivot_longer(
    cols = -c(Estado),
    names_to = "Ano",
    values_to = "PopTotalCru"
  )
View(pop_total_cru_long)

#Calcula a Proporção
proporcoes_to <- pop_total_cru_long %>%
  filter(Ano == ano_corte) %>%
  summarise(
    share_pop_total = sum(PopTotalCru[Estado == "Tocantins"], na.rm = TRUE) /
      (sum(PopTotalCru[Estado == "Tocantins"], na.rm = TRUE) +
         sum(PopTotalCru[Estado == "Goiás"], na.rm = TRUE))
  )
print(proporcoes_to$share_pop_total)

######################################################################################
###########################Interpolaçã Pop Rural##############################
#######################################################################################

pop_rural_cru <- read_excel("C:/Users/carol/Desktop/pop_rural_cru.xlsx")


#Identifica o "Ano de Corte" (Primeiro ano com dados separados de TO)
ano_corte <- 1991

#Converte formato largo para longo
pop_rural_cru_long<- pop_rural_cru |>
  pivot_longer(
    cols = -c(Estado),
    names_to = "Ano",
    values_to = "PopRuralCru"
  )


#Calcula a Proporção
proporcoes_to <- pop_rural_cru_long %>%
  filter(Ano == ano_corte) %>%
  summarise(
    share_pop_rural = sum(PopRuralCru[Estado == "Tocantins"], na.rm = TRUE) /
      (sum(PopRuralCru[Estado == "Tocantins"], na.rm = TRUE) +
         sum(PopRuralCru[Estado == "Goiás"], na.rm = TRUE))
  )
print(proporcoes_to$share_pop_rural)


################################################################################
###########################Interpolação PIB Estadual###########################
################################################################################


PIB_total_cru <- read_excel("C:/Users/carol/Desktop/PIB_total_cru.xlsx")


#Identifica o "Ano de Corte" (Primeiro ano com dados separados de TO)
ano_corte <- 1991

#Converte formato largo para longo
PIB_total_cru_long<- PIB_total_cru |>
  pivot_longer(
    cols = -c(Estado),
    names_to = "Ano",
    values_to = "PIBTotalCru"
  )

#Calcula a Proporção
proporcoes_to <- PIB_total_cru_long %>%
  filter(Ano == ano_corte) %>%
  summarise(
    share_pib_total = sum(PIBTotalCru[Estado == "Tocantins"], na.rm = TRUE) /
      (sum(PIBTotalCru[Estado == "Tocantins"], na.rm = TRUE) +
         sum(PIBTotalCru[Estado == "Goiás"], na.rm = TRUE))
  )
print(proporcoes_to$share_pib_total)


#################################################################################
########################### Interpolação PIB Rural #############################
#################################################################################


PIB_rural_cru <- read_excel("C:/Users/carol/Desktop/PIB_rural_cru.xlsx")


#Identifica o Ano de Corte (Primeiro ano com dados separados de TO)
ano_corte <- 1991

#Converte formato largo para longo
PIB_rural_cru_long<- PIB_rural_cru |>
  pivot_longer(
    cols = -c(Estado),
    names_to = "Ano",
    values_to = "PIBRuralCru"
  )

#Calcula a Proporção
proporcoes_to <- PIB_rural_cru_long %>%
  filter(Ano == ano_corte) %>%
  summarise(
    share_pib_rural = sum(PIBRuralCru[Estado == "Tocantins"], na.rm = TRUE) /
      (sum(PIBRuralCru[Estado == "Tocantins"], na.rm = TRUE) +
         sum(PIBRuralCru[Estado == "Goiás"], na.rm = TRUE))
  )
print(proporcoes_to$share_pib_rural)


#################################################################################
########################### Interpolação Índice de Gini #########################
#################################################################################


gini_cru <- read_excel("C:/Users/carol/Desktop/gini_cru.xlsx")

#Transforma formato largo para longo e limpa
df_longo_gini <- gini_cru %>%
  pivot_longer(
    cols = -Estado,
    names_to = "Ano",
    values_to = "Gini"
  ) %>%
  mutate(
    Ano = as.numeric(Ano),
    Gini = as.numeric(Gini)
  ) %>%
  filter(!is.na(Gini))

#Filtra apenas os dados históricos do Tocantins (pós-separação)
df_historico_to <- df_longo_gini %>%
  filter(grepl("Tocantins", Estado))

#Ajusta o modelo Log-Linear
modelo_gini_log <- lm(log(Gini) ~ Ano, data = df_historico_to)

dados_diag_log <- augment(modelo_gini_log)

#Traz a predição para 1985
log_predito_1985 <- predict(modelo_gini_log, newdata = data.frame(Ano = 1985))
gini_final_1985 <- exp(log_predito_1985)
print(gini_final_1985)


#Gera o gráfico de Resíduos
ggplot(dados_diag_log, aes(x = .fitted, y = .resid)) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Resíduos Modelo Log-Linear", x = "Ajustados", y = "Resíduos") +
  theme_minimal()

