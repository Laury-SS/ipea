#--------------------------------------------------------
# Modelos de Regressao
#--------------------------------------------------------

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

