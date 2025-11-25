#--------------------------------------------------------
#  PROGRAMA:
#        INPUT_R_PARTICIPANTES_2024
#--------------------------------------------------------
#  DESCRIÇÃO:
#        Leitura da base PARTICIPANTES_2024 e formatação
#        dos rótulos das variáveis utilizadas neste script.
#--------------------------------------------------------

# Instalação do pacote data.table, caso não esteja instalado
if (!require(data.table)) {
  install.packages("data.table")
}


#------------------
# Carga dos microdados
library(data.table)

# Escolha do arquivo CSV (PARTICIPANTES_2024.csv) ----
arquivo <- file.choose()

# Leitura dos microdados ----
PARTICIPANTES_2024 <- fread(
  arquivo,
  encoding = "Latin-1",
  integer64 = "character",
  na.strings = ""
)

#--------------------------------------------------------
# Formatação dos rótulos das variáveis utilizadas
#--------------------------------------------------------

# Faixa Etária ----
PARTICIPANTES_2024$TP_FAIXA_ETARIA <- factor(
  PARTICIPANTES_2024$TP_FAIXA_ETARIA,
  levels = 1:20,
  labels = c(
    'Menor de 17 anos','17 anos','18 anos','19 anos','20 anos','21 anos','22 anos',
    '23 anos','24 anos','25 anos','Entre 26 e 30 anos','Entre 31 e 35 anos','Entre 36 e 40 anos',
    'Entre 41 e 45 anos','Entre 46 e 50 anos','Entre 51 e 55 anos','Entre 56 e 60 anos',
    'Entre 61 e 65 anos','Entre 66 e 70 anos','Maior de 70 anos'
  )
)

# Sexo ----
PARTICIPANTES_2024$TP_SEXO <- factor(
  PARTICIPANTES_2024$TP_SEXO,
  levels = c('M','F'),
  labels = c('Masculino','Feminino')
)

# Cor/Raça ----
PARTICIPANTES_2024$TP_COR_RACA <- factor(
  PARTICIPANTES_2024$TP_COR_RACA,
  levels = c(0,1,2,3,4,5,6),
  labels = c(
    'Não declarado','Branca','Preta','Parda',
    'Amarela','Indígena','Não dispõe da informação'
  )
)

# Q001 – Escolaridade do pai ----
PARTICIPANTES_2024$Q001 <- factor(
  PARTICIPANTES_2024$Q001,
  levels = c('A','B','C','D','E','F','G','H'),
  labels = c(
    'Nunca estudou.',
    'Não completou a 4ª série/5ª ano do Ensino Fundamental.',
    'Completou a 4ª série/5ª ano, mas Não completou a 8ª série/9º ano do Ensino Fundamental.',
    'Completou a 8ª série/9º ano do Ensino Fundamental, mas Não completou o Ensino Médio.',
    'Completou o Ensino Médio, mas Não completou a Faculdade.',
    'Completou a Faculdade, mas Não completou a Pós-graduação.',
    'Completou a Pós-graduação.',
    'Não sei.'
  )
)

# Q002 – Escolaridade da mãe ----
PARTICIPANTES_2024$Q002 <- factor(
  PARTICIPANTES_2024$Q002,
  levels = c('A','B','C','D','E','F','G','H'),
  labels = c(
    'Nunca estudou.',
    'Não completou a 4ª série/5ª ano do Ensino Fundamental.',
    'Completou a 4ª série/5ª ano, mas Não completou a 8ª série/9º ano do Ensino Fundamental.',
    'Completou a 8ª série/9º ano do Ensino Fundamental, mas Não completou o Ensino Médio.',
    'Completou o Ensino Médio, mas Não completou a Faculdade.',
    'Completou a Faculdade, mas Não completou a Pós-graduação.',
    'Completou a Pós-graduação.',
    'Não sei.'
  )
)

# Verificar rapidamente as variáveis ----
summary(PARTICIPANTES_2024$TP_COR_RACA)
summary(PARTICIPANTES_2024$Q001)
summary(PARTICIPANTES_2024$Q002)

# Distribuição da Raça ----
library(scales)
library(ggplot2)

ggplot(PARTICIPANTES_2024, aes(x = TP_COR_RACA)) +
  geom_bar(fill = "#345385") +
  geom_text(
    stat = "count",
    aes(label = label_number(
      big.mark = ".",
      decimal.mark = ","
    )(..count..)),
    vjust = -0.3, size = 4
  ) +
  labs(
    title = "Distribuição da Raça Declarada",
    x = "Raça",
    y = "Frequência"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Distribuição da Escolaridade dos Pais ----
ggplot(PARTICIPANTES_2024, aes(x = Q001)) +
  geom_bar(fill = "#345385") +
  geom_text(
    stat = "count",
    aes(label = scales::label_number(
      big.mark = ".",
      decimal.mark = ","
    )(..count..)),
    hjust = -0.1, size = 4
  ) +
  labs(
    title = "Distribuição da Escolaridade do Pai",
    x = "Frequência",
    y = "Escolaridade do Pai"
  ) +
  coord_flip() +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.y = element_text(hjust = 1)
  )

ggplot(PARTICIPANTES_2024, aes(x = Q002)) +
  geom_bar(fill = "#346f85") +
  geom_text(
    stat = "count",
    aes(label = scales::label_number(
      big.mark = ".",
      decimal.mark = ","
    )(..count..)),
    hjust = -0.1, size = 4
  ) +
  labs(
    title = "Distribuição da Escolaridade da Mãe",
    x = "Frequência",
    y = "Escolaridade da Mãe"
  ) +
  coord_flip() +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.y = element_text(hjust = 1)
  )

# Checar os valores NA -----
table(PARTICIPANTES_2024$TP_COR_RACA, PARTICIPANTES_2024$Q001)

# Remover apenas o nível sem observações
PARTICIPANTES_2024$TP_COR_RACA <- droplevels(PARTICIPANTES_2024$TP_COR_RACA)
PARTICIPANTES_2024$Q001        <- droplevels(PARTICIPANTES_2024$Q001)
PARTICIPANTES_2024$Q002        <- droplevels(PARTICIPANTES_2024$Q002)

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

# Gráfico — Escolaridade dos pais por raça (pai) ----
ggplot(tabela_pai, aes(x = TP_COR_RACA, y = proporcao, fill = Q001)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    x = "Raça/Cor",
    y = "Proporção",
    fill = "Escolaridade do pai",
    title = "Distribuição da Escolaridade do Pai por Raça — ENEM 2024"
  ) +
  theme_minimal()

# Gráfico — Escolaridade da mãe por raça ----
ggplot(tabela_mae, aes(x = TP_COR_RACA, y = proporcao, fill = Q002)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    x = "Raça/Cor",
    y = "Proporção",
    fill = "Escolaridade da mãe",
    title = "Distribuição da Escolaridade da Mãe por Raça — ENEM 2024"
  ) +
  theme_minimal()

library(ggplot2)
library(viridis)
library(patchwork)
library(dplyr)
library(scales)

# --- TABELA DO PAI (ajuste para incluir valores brutos) ----
tabela_pai_plot <- tabela_pai %>%
  mutate(
    percent = proporcao,
    label_raw = n
  )

g_pai <- ggplot(tabela_pai_plot,
                aes(x = TP_COR_RACA, y = percent, fill = Q001)) +
  geom_col(position = "fill", color = "white") +
  geom_text(
    aes(label = label_raw),
    position = position_fill(vjust = 0.5),
    size = 4.5,               # tamanho maior dos rótulos
    color = "black"
  ) +
  scale_fill_viridis(discrete = TRUE, option = "D") +
  scale_y_continuous(labels = percent_format()) +
  labs(
    x = "Raça/Cor",
    y = "Proporção",
    fill = "Escolaridade do pai",
    title = "Distribuição da Escolaridade do Pai por Raça — ENEM 2024"
  ) +
  theme_minimal(base_size = 15) +       # fonte geral maior
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14)
  )


# --- TABELA DA MÃE (ajuste para incluir valores brutos) ----
tabela_mae_plot <- tabela_mae %>%
  mutate(
    percent = proporcao,
    label_raw = n
  )

g_mae <- ggplot(tabela_mae_plot,
                aes(x = TP_COR_RACA, y = percent, fill = Q002)) +
  geom_col(position = "fill", color = "white") +
  geom_text(
    aes(label = label_raw),
    position = position_fill(vjust = 0.5),
    size = 4.5,     # tamanho maior dos rótulos
    color = "black"
  ) +
  scale_fill_viridis(discrete = TRUE, option = "D") +
  scale_y_continuous(labels = percent_format()) +
  labs(
    x = "Raça/Cor",
    y = "Proporção",
    fill = "Escolaridade da mãe",
    title = "Distribuição da Escolaridade da Mãe por Raça — ENEM 2024"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14)
  )


# --- GRID LADO A LADO ----
g_pai / g_mae / plot_layout(ncol = 2)



# Teste estatístico — associação entre raça e escolaridade ----

# Teste Qui-quadrado (pai)
chisq_pai <- chisq.test(table(PARTICIPANTES_2024$TP_COR_RACA,
                              PARTICIPANTES_2024$Q001))
chisq_pai
#p < 2.2e-16

# Teste Qui-quadrado (mãe)
chisq_mae <- chisq.test(table(PARTICIPANTES_2024$TP_COR_RACA,
                              PARTICIPANTES_2024$Q002))
chisq_mae
#p < 2.2e-16

# Os dois testes rejeitam H₀ fortemente:
#   
# Há associação estatisticamente significativa entre raça e escolaridade dos pais.
# (tanto pai quanto mãe)
# 
# Isso reforça que seu tema de desigualdades educacionais por raça está extremamente bem fundamentado.


# V de Cramer → tamanho da associação ----
library(DescTools)

# PAI
tab_pai <- table(PARTICIPANTES_2024$TP_COR_RACA, PARTICIPANTES_2024$Q001)
V_Cramer_pai <- CramerV(tab_pai)
V_Cramer_pai
# V = 0.1115581

# MÃE
tab_mae <- table(PARTICIPANTES_2024$TP_COR_RACA, PARTICIPANTES_2024$Q002)
V_Cramer_mae <- CramerV(tab_mae)
V_Cramer_mae
# V = 0.1052403

# 0.11 e 0.10 ⇒ associação fraca, mas estatisticamente significativa (você já viu que p < 2e-16).
# Isso é normal em amostras muito grandes como ENEM (3 milhões de registros).
# Existe desigualdade racial na escolaridade dos pais, mas o tamanho da associação é fraco, 
# o que significa que raça explica uma parte pequena da variação na escolaridade parental, 
# porém ainda assim há um padrão consistente desfavorável a pretos, pardos e indígenas.

# Comparar proporções ----

# escolaridade do pai
prop.table(tab_pai, margin = 1) * 100

# escolaridade da mãe
prop.table(tab_mae, margin = 1) * 100

# Gráfico de barras empilhadas normalizadas (percentual) ----

# Escolaridade do pai
library(dplyr)
library(tidyr)

# Criar tabela de proporções do pai
tab_pai <- table(PARTICIPANTES_2024$TP_COR_RACA,
                 PARTICIPANTES_2024$Q001)

df_pai <- prop.table(tab_pai, margin = 1) %>%
  as.data.frame() %>%
  rename(Raca = Var1,
         Escolaridade_Pai = Var2,
         Proporcao = Freq)

# Gráfico
ggplot(df_pai, aes(x = Raca, y = Proporcao, fill = Escolaridade_Pai)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Distribuição da Escolaridade do Pai por Raça/Cor – ENEM 2024",
    x = "Raça/Cor do Estudante",
    y = "Proporção (%)",
    fill = "Escolaridade do Pai"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1)
  )

# Escolaridade da mãe
# Criar tabela de proporções da mãe
tab_mae <- table(PARTICIPANTES_2024$TP_COR_RACA,
                 PARTICIPANTES_2024$Q002)

df_mae <- prop.table(tab_mae, margin = 1) %>%
  as.data.frame() %>%
  rename(Raca = Var1,
         Escolaridade_Mae = Var2,
         Proporcao = Freq)

# Gráfico
ggplot(df_mae, aes(x = Raca, y = Proporcao, fill = Escolaridade_Mae)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Distribuição da Escolaridade da Mãe por Raça/Cor – ENEM 2024",
    x = "Raça/Cor do Estudante",
    y = "Proporção (%)",
    fill = "Escolaridade da Mãe"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1)
  )

# Regressão Logística Ordinal ----

# Referência para “Branca”
PARTICIPANTES_2024$TP_COR_RACA <- relevel(PARTICIPANTES_2024$TP_COR_RACA,
                                          ref = "Branca")

# PAI
library(MASS)

modelo_pai <- polr(
  Q001 ~ TP_COR_RACA,
  data = PARTICIPANTES_2024,
  Hess = TRUE
)
summary(modelo_pai)

# MÃE
modelo_mae <- polr(
  Q002 ~ TP_COR_RACA,
  data = PARTICIPANTES_2024,
  Hess = TRUE
)
summary(modelo_mae)

# Obter valores-p
ctable_pai <- coef(summary(modelo_pai))
p_values_pai <- pnorm(abs(ctable_pai[, "t value"]), lower.tail = FALSE) * 2

ctable_mae <- coef(summary(modelo_mae))
p_values_mae <- pnorm(abs(ctable_mae[, "t value"]), lower.tail = FALSE) * 2

p_values_pai
p_values_mae

# GRÁFICOS DE BARRAS EMPILHADAS (Pai e Mãe) -----
library(viridis)

### --- Pai ---
graf_pai_viridis <- ggplot(df_pai, aes(x = Raca, y = Proporcao, fill = Escolaridade_Pai)) +
  geom_bar(stat = "identity", color = "black", linewidth = 0.2) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_viridis(discrete = TRUE, option = "viridis") +
  labs(
    title = "Distribuição da Escolaridade do Pai por Raça/Cor",
    x = "Raça/Cor do Participante", y = "Proporção (%)",
    fill = "Escolaridade do Pai"
  ) +
  theme_bw(base_size = 11) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 30, hjust = 1)
  )

### --- Mãe ---
graf_mae_viridis <- ggplot(df_mae, aes(x = Raca, y = Proporcao, fill = Escolaridade_Mae)) +
  geom_bar(stat = "identity", color = "black", linewidth = 0.2) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_viridis(discrete = TRUE, option = "viridis") +
  labs(
    title = "Distribuição da Escolaridade da Mãe por Raça/Cor",
    x = "Raça/Cor do Participante", y = "Proporção (%)",
    fill = "Escolaridade da Mãe"
  ) +
  theme_bw(base_size = 11) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 30, hjust = 1)
  )

graf_pai_viridis
graf_mae_viridis


# Gráficos de Efeitos Marginais ----

## Função para obter efeitos marginais simples
marginais <- function(modelo, varname){
  nd <- data.frame(TP_COR_RACA = levels(PARTICIPANTES_2024$TP_COR_RACA))
  probs <- predict(modelo, newdata = nd, type="probs")
  as.data.frame(probs) %>%
    mutate(TP_COR_RACA = nd$TP_COR_RACA) %>%
    pivot_longer(cols = -TP_COR_RACA,
                 names_to = "Categoria",
                 values_to = "Prob")
}

ef_pai <- marginais(modelo_pai, "TP_COR_RACA")
ef_mae <- marginais(modelo_mae, "TP_COR_RACA")

## --- Gráfico Pai ---
graf_marg_pai <- ggplot(ef_pai,
                        aes(x = TP_COR_RACA, y = Prob, color = Categoria, group = Categoria)) +
  geom_line(linewidth = 1.6) +             # linhas mais espessas
  geom_point(size = 3.5) +                 # pontos maiores
  scale_color_viridis(discrete=TRUE) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Efeitos Marginais — Escolaridade do Pai",
       x = "Raça/Cor", y = "Probabilidade", color = "Categoria") +
  theme_bw(base_size = 18) +               # aumenta toda a base
  theme(
    axis.text.x = element_text(size = 16, angle = 30, hjust = 1),
    axis.text.y = element_text(size = 16),
    axis.title = element_text(size = 18, face = "bold"),
    plot.title = element_text(size = 20, face = "bold"),
    legend.title = element_text(size = 18, face = "bold"),
    legend.text = element_text(size = 16)
  )

graf_marg_pai


## --- Gráfico Mãe ---
graf_marg_mae <- ggplot(ef_mae,
                        aes(x = TP_COR_RACA, y = Prob, color = Categoria, group = Categoria)) +
  geom_line(linewidth = 1.6) +
  geom_point(size = 3.5) +
  scale_color_viridis(discrete=TRUE) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Efeitos Marginais — Escolaridade da Mãe",
       x = "Raça/Cor", y = "Probabilidade", color = "Categoria") +
  theme_bw(base_size = 18) +
  theme(
    axis.text.x = element_text(size = 16, angle = 30, hjust = 1),
    axis.text.y = element_text(size = 16),
    axis.title = element_text(size = 18, face = "bold"),
    plot.title = element_text(size = 20, face = "bold"),
    legend.title = element_text(size = 18, face = "bold"),
    legend.text = element_text(size = 16)
  )

graf_marg_mae


# Heatmap das Probabilidades Preditas ----

# PAI
# Gerar dataframe de predições
newdata <- expand.grid(
  TP_COR_RACA = levels(PARTICIPANTES_2024$TP_COR_RACA)
)

# Probabilidades preditas
pred <- predict(modelo_pai, newdata, type = "probs") %>%
  as.data.frame()

pred$TP_COR_RACA <- newdata$TP_COR_RACA

# Transformar em formato longo
pred_long <- pred %>%
  pivot_longer(
    cols = -TP_COR_RACA,
    names_to = "Escolaridade_Pai",
    values_to = "Probabilidade"
  )

# Ordenar escolaridade corretamente
pred_long$Escolaridade_Pai <- factor(
  pred_long$Escolaridade_Pai,
  levels = levels(PARTICIPANTES_2024$Q001)
)

# HEATMAP
ggplot(pred_long, aes(x = TP_COR_RACA, y = Escolaridade_Pai, fill = Probabilidade)) +
  geom_tile(color = "white") +
  scale_fill_viridis(option = "D", direction = 1, name = "Probabilidade") +
  labs(
    title = "Probabilidades Preditas da Escolaridade do Pai por Raça",
    x = "Raça/Cor",
    y = "Categoria de Escolaridade"
  ) +
  theme_minimal(base_size = 18) +   # aumenta fonte geral
  theme(
    axis.text.x = element_text(size = 16, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 16),
    axis.title = element_text(size = 18, face = "bold"),
    plot.title = element_text(size = 20, face = "bold"),
    legend.title = element_text(size = 18, face = "bold"),
    legend.text = element_text(size = 16),
    panel.grid = element_blank()
  )


# MÃE

# Gerar novos dados (todas as raças)
newdata_mae <- expand.grid(
  TP_COR_RACA = levels(PARTICIPANTES_2024$TP_COR_RACA)
)

# Probabilidades preditas para MÃE
pred_mae <- predict(modelo_mae, newdata_mae, type = "probs") %>%
  as.data.frame()

pred_mae$TP_COR_RACA <- newdata_mae$TP_COR_RACA

# Transformar em formato longo
pred_mae_long <- pred_mae %>%
  pivot_longer(
    cols = -TP_COR_RACA,
    names_to = "Escolaridade_Mae",
    values_to = "Probabilidade"
  )

# Ordenar escolaridade corretamente
pred_mae_long$Escolaridade_Mae <- factor(
  pred_mae_long$Escolaridade_Mae,
  levels = levels(PARTICIPANTES_2024$Q002)
)

# HEATMAP
ggplot(pred_mae_long, aes(x = TP_COR_RACA, y = Escolaridade_Mae, fill = Probabilidade)) +
  geom_tile(color = "white") +
  scale_fill_viridis(option = "D", direction = 1, name = "Probabilidade") +
  labs(
    title = "Probabilidades Preditas da Escolaridade da Mãe por Raça",
    x = "Raça/Cor",
    y = "Categoria de Escolaridade"
  ) +
  theme_minimal(base_size = 18) +  # aumenta fonte geral
  theme(
    axis.text.x = element_text(size = 16, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 16),
    axis.title = element_text(size = 18, face = "bold"),
    plot.title = element_text(size = 20, face = "bold"),
    legend.title = element_text(size = 18, face = "bold"),
    legend.text = element_text(size = 16),
    panel.grid = element_blank()
  )




