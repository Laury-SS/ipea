## Desigualdades Raciais na Escolaridade dos Pais - ENEM 2024

Este projeto investiga como a escolaridade dos pais dos participantes do ENEM 2024 varia entre diferentes grupos raciais.
O objetivo é compreender padrões de desigualdade educacional associados à raça/cor, com base nos microdados disponibilizados pelo INEP.

A análise utiliza estatísticas descritivas, testes de associação e modelos de regressão logística ordinal para responder à pergunta de pesquisa:

**Como a escolaridade dos pais varia entre os diferentes grupos raciais dos participantes do ENEM 2024 e quais desigualdades educacionais esse padrão revela?**

## Metodologia
1. Pré-processamento dos dados

Remoção de níveis vazios.
Transformação das variáveis em fatores ordenados.
Relevel da variável de raça, usando “Branca” como categoria de referência.

2. Análise Descritiva

Cálculo de tabelas de contingência: escolaridade × raça.
Cálculo de proporções por grupo racial.
Visualização de padrões via:
gráficos de barras empilhadas 
gráficos de proporção 

3. Testes de Associação

Qui-quadrado de independência entre raça e escolaridade dos pais.
V de Cramer para medir a força da associação.

4. Modelagem Estatística

Ajuste de Regressão Logística Ordinal (polr/MASS):
Modelo 1: Q001 ~ TP_COR_RACA

Modelo 2: Q002 ~ TP_COR_RACA

Cálculo de valores-p via distribuição normal assintótica.
Interpretação dos coeficientes como log-odds de maior escolaridade, comparada ao grupo branco.

## Principais Resultados

**Padrão Geral Observado**

Participantes pretos, pardos e indígenas apresentam maior proporção de pais com baixa escolaridade.
Participantes brancos apresentam maior proporção de pais com ensino superior.
Mães tendem a ter escolaridade ligeiramente maior que os pais, em todos os grupos raciais.

**Regressão Logística Ordinal**

Todos coeficientes raciais são negativos e altamente significativos.
Isso indica que não declarados, pretos, pardos, amarelos e indígenas têm menor chance de possuir pais/mães com maior escolaridade, comparados a brancos.

## Estrutura do projeto

ipea/
├── R/ # Scripts R com as análises
├── data/ # Microdados do ENEM 2024 (não incluídos no repositório)
├── outputs/ # Gráficos e resultados exportados
├── docs/ # Documentos, relatórios e arquivos explicativos
├── README.md # Este arquivo

## Scripts em R

- `R/02_tratamento_variaveis.R` – Formatação e tratamento das variáveis.
- `R/03_analises_descritivas.R` – Estatísticas descritivas.
- `R/04_testes_estatisticos.R` – Testes de associação (Qui-quadrado, V de Cramer).
- `R/05_modelos_regressao.R` – Modelos de regressão logística ordinal.
- `R/06_visualizacoes.R` – Gráficos e visualizações dos resultados.

> Cada script pode ser executado sequencialmente usando `source("R/nome_do_script.R")`.

## Dados

**Importante:** Os microdados do ENEM 2024 **não estão incluídos** no repositório devido ao tamanho.  
Para rodar os scripts, siga estas instruções:

1. Baixe o arquivo **PARTICIPANTES_2024.csv** do site oficial do ENEM - [Link](https://www.gov.br/inep/pt-br/acesso-a-informacao/dados-abertos/microdados/enem)
2. Coloque o arquivo dentro da pasta `data/` do projeto.  
3. Os scripts irão ler o arquivo automaticamente.

> Caso a pasta `data/` esteja vazia, ela contém apenas um arquivo `.gitkeep` para manter a estrutura no GitHub.

## Outputs

Os gráficos e resultados exportados são salvos em `outputs/`. Alguns exemplos:

- Distribuição de escolaridade por raça dos pais.
- Gráficos de barras empilhadas e heatmaps das probabilidades preditas.
- Efeitos marginais de modelos de regressão logística ordinal.
