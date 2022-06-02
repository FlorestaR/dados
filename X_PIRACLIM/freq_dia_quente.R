# Packages
library(tidyverse)
library(rio)

# Definição do URL completo para a planilha no github, e inclusão
#     do parâmetro que impõe leitura dos dados brutos (?raw=true)
url_1 <- "https://github.com/FlorestaR/dados/blob/main/X_PIRACLIM"
xls_2 <- "DadosClima_Piracicaba.xlsx"
prm_3 <- "?raw=true"
gitFile <- file.path(url_1, xls_2) %>% paste0(prm_3)

# Faz o download da planilha como uma tibble (dataframe)
df <- import(file = gitFile) %>% tibble() # importação

# Renomeia as colunas com o conteúdo da segunda linha df[1,]
colnames(df) <- df[1,]
# Deleta a segunda linha
df <- df[-c(1),]

# Exibe primeiras linhas do dataframe
head(df)

#limpando dados (colunas)
df = subset(df, select = -c(Estiagem,URMED, VentoMED, TMED,
                            TMAX_hora, URMAX, URMAX_hora,
                            VentoMAX, VentoMAX_hora, TMIN,
                            TMIN_hora, URMIN, URMIN_hora,
                            Chuva,Rad.Glob.) )
#limpando dados (linhas)
df = df[-c(1:35454),]

#transformando os dados TMAX em numéricos
df$TMAX = as.numeric(df$TMAX)
t_max = df$TMAX

#Contnstrução do Histograma para a coluna TMAX
hist(t_max)

#Organizar os dados nas temperaturas selecionadas

