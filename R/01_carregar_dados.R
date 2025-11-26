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

# Carregar pacotes
library(scales)
library(ggplot2)
library(dplyr)
library(viridis)
library(patchwork)
library(DescTools)
library(tidyr)
library(MASS)

# Escolha do arquivo CSV (PARTICIPANTES_2024.csv) ----
arquivo <- file.choose()

# Leitura dos microdados ----
PARTICIPANTES_2024 <- fread(
  arquivo,
  encoding = "Latin-1",
  integer64 = "character",
  na.strings = ""
)
