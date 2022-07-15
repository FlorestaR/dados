# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Inventário com dados LiDAR na Fazenda Modelo ~~~~~~~~~~~~~~~~~~~~~~~~
#
# Autor: Luiz Carlos Estraviz Rodriguez
#        Departamento de Ciências Florestais
#        ESALQ/USP - 15/Jul/2022
#
# Inventário florestal da Fazenda Modelo
#   - download dos dados mantidos em um repositório público github
#      - LiDAR multitemporal (2013 e 2014)
#      - shape files dos talhões florestais
#   - armazenamento local na pasta C:/LiDAR/:
#
# Linguagem de programação:
#       R (v 4.2)
#       pacote lidR* (v 4.0.2 May 4th, 2022) Jean Romain Roussel
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rm(list=ls(all=TRUE))                                   # Limpa memória
gc()


# Instala packages, caso não estejam disponíveis
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
if(!require(tidyverse))
  install.packages("tidyverse")
library(tidyverse)

# Define os nomes dos tiles LiDAR multitemporais que serão lidos do
# repositório, os respectivos anos e o URL completo do github
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
gitNome <- c("L0004-C0005.laz", "L0004-C0006.laz",
             "L0005-C0005.laz", "L0005-C0006.laz",
             "L0006-C0005.laz", "L0006-C0006.laz")
anoData <- c("A13", "A14")
urlRepo <- "https://github.com/FlorestaR/dados/blob/main/5_LIDARF/Modelo/CLOUDS/"

# Cria diretórios e pastas para onde os tiles LiDAR serão copiados
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
lidarDir <- "C:/LiDAR/";        dir.create(lidarDir, showWarnings = T)
lidarDir <- "C:/LiDAR/Modelo/"; dir.create(lidarDir, showWarnings = T)

# Faz o download dos tiles
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
options(timeout=1000) # Reset timeout oferecendo mais tempo de download
for (ano in anoData) {
  dirLAZ <- paste0(lidarDir, ano)
  dir.create(dirLAZ, showWarnings = T)
  
  for (nome in gitNome){
    gitOnde <- paste0(urlRepo, ano)
    gitFile <- paste0(ano, nome)
    gitArqv <- file.path(gitOnde, gitFile) %>% paste0("?raw=true")
    
    lazf <- file.path(dirLAZ, gitFile)
    if(!file.exists(lazf))  # garante download de dados binários (wb)
      download.file(gitArqv, mode="wb", destfile = lazf) 
  }
}

# Leitura dos dois catálogos de dados LiDAR (2013 e 2014)
# Exibe conteúdo do catálogo e confere se os mapas de tiles são iguais
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
require(lidR)

dirLAZ1 <- paste0(lidarDir, anoData[1])
ctg1    <- readLAScatalog(dirLAZ1)
print(ctg1@data)
plot(ctg1)

dirLAZ2 <- paste0(lidarDir, anoData[2])
ctg2    <- readLAScatalog(dirLAZ2)
print(ctg2@data)
plot(ctg2)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Este script foi criado em Junho/2022 depois de instalar a mais
# recente versão do pacote lidR.
#
# * lidR latest development version: sequência de instalação de pacotes
#   devtools::install_github("Jean-Romain/rlas", dependencies=TRUE)
#   devtools::install_github("Jean-Romain/lidR")
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~