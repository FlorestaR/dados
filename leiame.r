# read_csv() de dados direto de um repositório no gitHub
# ------------------------------------------------------------------------ ----
library(tidyverse)
gitOnde <- "https://github.com/LuizEstraviz/LivroDados/blob/main/1_Amazonia/"
gitNome <- "Censo_UPA04.csv"
gitArqv <- paste0(gitOnde, gitNome, "?raw=true")
dados <- read_csv(gitArqv)
# O resultado é uma tibble com 20.321 linhas e 19 variáveis
# ----

# Package rio para leitura direta de planilha em um repositório no gitHub
# ------------------------------------------------------------------------ ----
library(rio)
library(tidyverse)
gitOnde <- "https://github.com/LuizEstraviz/LivroDados/blob/main/1_Amazonia/"
gitNome <- "Censo_UPA04.xlsx"
gitArqv <- paste0(gitOnde, gitNome, "?raw=true")
dados   <- tibble(rio::import(gitArqv))
# O resultado é transformado em uma tibble com 20.321 linhas e 19 variáveis
# ----