#--------------------------------------------------------
# Visualizacoes
#--------------------------------------------------------

# função para salvar TODOS os gráficos ----
salvar_grafico <- function(grafico, nome, largura = 10, altura = 7, dpi = 300) {
  
  if (!dir.exists("outputs/graficos")) {
    dir.create("outputs/graficos", recursive = TRUE)
  }
  
  caminho <- paste0("outputs/graficos/", nome, ".png")
  
  ggsave(filename = caminho,
         plot = grafico,
         width = largura, height = altura, dpi = dpi)
}

# Distribuição da Raça ----
library(scales)
library(ggplot2)

dist_raca <- ggplot(PARTICIPANTES_2024, aes(x = TP_COR_RACA)) +
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

dist_raca
salvar_grafico(dist_raca, "distribuicao_raca")

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


library(viridis)
library(patchwork)
library(dplyr)

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

# Gráfico de barras empilhadas normalizadas (percentual) ----
library(tidyr)

# Escolaridade do pai
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
