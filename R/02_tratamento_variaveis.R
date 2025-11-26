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

# Checar os valores NA -----
table(PARTICIPANTES_2024$TP_COR_RACA, PARTICIPANTES_2024$Q001)

# Remover apenas o nível sem observações
PARTICIPANTES_2024$TP_COR_RACA <- droplevels(PARTICIPANTES_2024$TP_COR_RACA)
PARTICIPANTES_2024$Q001        <- droplevels(PARTICIPANTES_2024$Q001)
PARTICIPANTES_2024$Q002        <- droplevels(PARTICIPANTES_2024$Q002)
