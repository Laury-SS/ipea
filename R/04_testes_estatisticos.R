#--------------------------------------------------------
# Testes Estatisticos
#--------------------------------------------------------

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
