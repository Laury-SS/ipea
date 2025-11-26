#--------------------------------------------------------
# Analise Descritiva
#--------------------------------------------------------

# Verificar rapidamente as variáveis ----
summary(PARTICIPANTES_2024$TP_COR_RACA)
summary(PARTICIPANTES_2024$Q001)
summary(PARTICIPANTES_2024$Q002)

# Tabelas de proporção Escolaridade do pai × Raça ----
library(dplyr)

tabela_pai <- PARTICIPANTES_2024 %>%
  group_by(TP_COR_RACA, Q001) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(TP_COR_RACA) %>%
  mutate(proporcao = n / sum(n))

tabela_pai

# Tabelas de proporção Escolaridade da mãe × Raça ----
tabela_mae <- PARTICIPANTES_2024 %>%
  group_by(TP_COR_RACA, Q002) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(TP_COR_RACA) %>%
  mutate(proporcao = n / sum(n))

tabela_mae

# Escolaridade do pai
# Criar tabela de proporções do pai
tab_pai <- table(PARTICIPANTES_2024$TP_COR_RACA,
                 PARTICIPANTES_2024$Q001)

df_pai <- prop.table(tab_pai, margin = 1) %>%
  as.data.frame() %>%
  rename(Raca = Var1,
         Escolaridade_Pai = Var2,
         Proporcao = Freq)

# Escolaridade da mãe
# Criar tabela de proporções da mãe
tab_mae <- table(PARTICIPANTES_2024$TP_COR_RACA,
                 PARTICIPANTES_2024$Q002)

df_mae <- prop.table(tab_mae, margin = 1) %>%
  as.data.frame() %>%
  rename(Raca = Var1,
         Escolaridade_Mae = Var2,
         Proporcao = Freq)

# Comparar proporções ----

# escolaridade do pai
prop.table(tab_pai, margin = 1) * 100

# escolaridade da mãe
prop.table(tab_mae, margin = 1) * 100