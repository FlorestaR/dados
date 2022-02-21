# leiaGit.r
#
# Script R para leitura direta de dados disponíveis em repositório gitHub
#
# Autoria: Equipe FlorestaR
# Revisão: 12/Ago/2021
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rm(list=ls(all=TRUE))                                   # Limpa memória
gc()

# read_csv() de dados direto de um repositório no gitHub
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ----
library(tidyverse)
gitOnde <- "https://github.com/LuizEstraviz/LivroDados/blob/main/1_Amazonia/"
gitNome <- "Censo_UPA04.csv"
gitArqv <- paste0(gitOnde, gitNome, "?raw=true")
dados <- read_csv(gitArqv)
# O resultado é uma tibble com 20.321 linhas e 19 variáveis
# ----

# Package rio para leitura direta de planilha em um repositório no gitHub
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ----
library(rio)
library(tidyverse)
gitOnde <- "https://github.com/LuizEstraviz/LivroDados/blob/main/1_Amazonia/"
gitNome <- "Censo_UPA04.xlsx"
gitArqv <- paste0(gitOnde, gitNome, "?raw=true")
dados   <- tibble(rio::import(gitArqv))
# O resultado é uma tibble com 20.321 linhas e 19 variáveis
# ----

# Análise exploratória de dados
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ----

# Define fatores
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
colFator <- c('CodArvore', 'UT', 'Linha', 'UCA', 'Especie', 
              'ClasseDAP', 'Destino', 'PatioMaisProximo')
dados[colFator] <- lapply(dados[colFator], factor)
glimpse(dados)

# Conta árvores de cada UT para cada destino
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
table(dados$UT, dados$Destino)

# Conta árvores em cada UCA de cada UT
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
table(dados$UT, dados$UCA)

# ----