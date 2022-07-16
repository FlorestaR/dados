# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Inventário com dados LiDAR na Fazenda Modelo ~~~~~~~~~~~~~~~~~~~~~~~~
#
# Autor: Luiz Carlos Estraviz Rodriguez
#        Departamento de Ciências Florestais
#        ESALQ/USP - 15/Jul/2022
#
# Inventário florestal da Fazenda Modelo
#   - download dos dados mantidos em um repositório público github
#      - shape files dos talhões florestais
#      - LiDAR multitemporal (2013 e 2014)
#   - armazenamento local na pasta C:/LiDAR/:
#
# Linguagem de programação:
#       R (v 4.2)
#       pacote lidR* (v 4.0.2 May 4th, 2022) Jean Romain Roussel
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rm(list=ls(all=TRUE))                                   # Limpa memória
gc()
options(timeout=1000) # Reset timeout oferecendo mais tempo de download

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Instala packages, caso ainda não tenham sido instalados
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
if(!require(tidyverse))              # Para manipulação de tabelas tidy
  install.packages("tidyverse")
library(tidyverse)
if(!require(sf))                           # Para manipulação de shapes
  install.packages("sf")
library(sf)
if(!require(lidR))                    # Para manipulação de dados LiDAR
  install.packages("lidR")
library(lidR)
if(!require(foreach))                         # Para loops inteligentes
  install.packages("foreach")
library(foreach)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Download do shape da Fazenda Modelo (2 layers: talhoes e parcelas)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
gitOnde <- "https://github.com/FlorestaR/dados/blob/main/5_LIDARF/Modelo/SHAPES"
gitNome <- "shapes.zip"
gitArqv <- file.path(gitOnde, gitNome) %>% paste0("?raw=true")

tmpd <- tempdir(check = TRUE)                    # diretório temporário
zipf <- file.path(tmpd, "shapes.zip")              # arquivo temporário

if(!file.exists(zipf))  # garante download de dados binários (wb)
  download.file(gitArqv, mode="wb", destfile = zipf) 

unzip(zipf, exdir = tmpd)     # shape é unziped no diretório temporário
unlink(zipf)                                  # deleta o arquivo zipado

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Lê tabela de atributos dos talhoes
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
shpArq <- paste0(tmpd, "/Modelo_talhoes.shp")       # shape com talhões
talhoesComGeo <- sf::read_sf(shpArq)                # completo com geom
talhoesSemGeo <- tibble(sf::st_drop_geometry(talhoesComGeo))  # s/ geom

# Imprime tabela de talhoes e áreas
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
AreaTotal <- talhoesSemGeo$AREA %>% sum
talhoesSemGeo %>%
  select(SUBTALHAO, AREA) %>% 
  arrange(SUBTALHAO) %>%
    add_row(SUBTALHAO="TOTAL", AREA = AreaTotal) %>%
  knitr::kable(caption = "Lista de Talhões da Fazenda Modelo")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Lê tabela de atributos das parcelas
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
shpArq <- paste0(tmpd, "/Modelo_parcelas.shp")     # shape com parcelas
parcelasComGeo <- sf::read_sf(shpArq)               # completo com geom
parcelasSemGeo <-tibble(sf::st_drop_geometry(parcelasComGeo)) # s/ geom

# Imprime tabela de talhões e áreas
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
parcelasSemGeo %>%
  select(CHAVE2, DATAREALIZ, IDINV, AREAPARCEL, MHDOM, VTCC, AB) %>% 
  arrange(CHAVE2) %>%
  knitr::kable(caption = "Lista de Parcelas da Fazenda Modelo")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Define funções para cálculos de inventário
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Intervalo de Confiança ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
calcCI = function(err, n, alpha=.05){
  return(
    qt(1 - alpha/2, n-1) * err #/ sqrt(n)
  )
}

# Parâmetros de Estimação e Inferência ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
calcPars = function(df, N, alpha=.05){
  means = apply(df, 2, mean)
  vars   = apply(df, 2, function(y){
    (var(y) / length(y)) * ((N - length(y)) / N)
  })
  err = sqrt(vars)
  cis = calcCI(err, nrow(df), alpha)
  err_pc = 100*cis/means
  out = rbind(means, vars, err, cis, err_pc, nrow(df)) %>% as.data.frame
  row.names(out) = c('mean', 'mean_var', 'std_err', 'ci', 'err_pc', 'n')
  names(out) = names(df)
  return(out)
}

# Número Total de Unidades Amostrais (N) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
parcelaAreaMedia <- mean(parcelasSemGeo$AREAPARCEL) / 10000     # em ha
tamanhoPopulacao <- round(AreaTotal / parcelaAreaMedia , 0)

# =====================================================================
# Amostragem Casual Simples (ACS): VTCC estimação e inferência
# =====================================================================
variaveisDeInteresse <- c("MHDOM", "VTCC", "AB")
df <- parcelasSemGeo %>% select(all_of(variaveisDeInteresse))
simplePars  <- df %>% calcPars(tamanhoPopulacao)
# Intensidade amostral (ha por parcela) da ACS que garante ~10% de erro 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
numeroIdealParcelas = function(y, N, errDesired=.10, alpha=.10){
  B  = errDesired * mean(y)
  qt = qt(1 - alpha/2, length(y)-1)
  n  = N*var(y)*qt^2 / (N * B^2 + qt^2 * var(y))
  return(n)
}
IA  <- round(AreaTotal/numeroIdealParcelas(df$VTCC,tamanhoPopulacao),0)

# =====================================================================
# Amostragem Casual Estratificada (ACE): VTCC estimação e inferência
# =====================================================================
# Grupos de estratificação
groups = c("IDINV")

rightAges = parcelasSemGeo$IDINV < 6 & parcelasSemGeo$IDINV > 2
stratPars = foreach(
  fac = groups, .combine = 'rbind') %:% 
  foreach(
    g = parcelasSemGeo[,fac] %>%
      unique %>% as.character %>%
      sort, .combine = 'rbind'
  ) %do% {
  inGroup = parcelasSemGeo[,fac] == g
  popSize = sum( raster::area(talhoesSemGeo[rightAges & !is.na(inGroup) & inGroup,] )) / mean(spatialPlotMetrics$AREA)
  
  inGroup = spatialPlotMetrics@data[,fac] == g
  tempPars = spatialPlotMetrics@data[inGroup,interestVars,drop=F]
  
  inventory = calcPars(tempPars, popSize)
  inventory$factor = fac
  inventory$group  = g
  inventory$n      = nrow(tempPars)
  inventory$N      = popSize
  
  return(inventory)
}






rightAges = talhoes$IDADE_PLAN < 6 & talhoes$IDADE_PLAN > 2
stratPars = foreach(fac = groups, .combine = 'rbind') %:% 
  foreach(g = spatialPlotMetrics@data[,fac] %>% 
            unique %>% as.character %>% 
            sort, .combine = 'rbind') %do% {
  inGroup = talhoes@data[,fac] == g
  popSize = sum( raster::area(talhoes[rightAges & !is.na(inGroup) & inGroup,] )) / mean(spatialPlotMetrics$AREA)
  
  inGroup = spatialPlotMetrics@data[,fac] == g
  tempPars = spatialPlotMetrics@data[inGroup,interestVars,drop=F]
  
  inventory = calcPars(tempPars, popSize)
  inventory$factor = fac
  inventory$group  = g
  inventory$n      = nrow(tempPars)
  inventory$N      = popSize
  
  return(inventory)
}




# Mapa dos talhões com localização das parcelas (EPSG: 31983)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ggplot() +                # plot dos talhões e parcelas (col por idade)
  geom_sf(data =  talhoesComGeo, colour = "black", fill="white") +
  geom_sf(data = parcelasComGeo, aes(colour = IDINV)) +
  coord_sf(datum=st_crs(31983)) +        # Especifica sistema de coord.
  scale_y_continuous(breaks = seq(from=7356500,to=7359000, by=200)) +
  scale_x_continuous(breaks = seq(from=206200, to=207600,  by=200))






# Define os nomes dos tiles LiDAR multitemporais que serão lidos do
# repositório, os respectivos anos e o URL completo do github
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
gitOnde <- "https://github.com/FlorestaR/dados/blob/main/5_LIDARF/Modelo/CLOUDS/"
gitNome <- c("L0004-C0005.laz", "L0004-C0006.laz",
             "L0005-C0005.laz", "L0005-C0006.laz",
             "L0006-C0005.laz", "L0006-C0006.laz")
anoData <- c("A13", "A14")

# Cria diretórios e pastas para onde os tiles LiDAR serão copiados
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
lidarDir <- "C:/LiDAR/";        dir.create(lidarDir, showWarnings = T)
lidarDir <- "C:/LiDAR/Modelo/"; dir.create(lidarDir, showWarnings = T)

# Faz o download dos tiles
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
for (ano in anoData) {
  dirLAZ <- paste0(lidarDir, ano)
  dir.create(dirLAZ, showWarnings = T)
  
  for (nome in gitNome){
    gitOnde <- paste0(gitOnde, ano)
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

dirLAZ1 <- paste0(lidarDir, anoData[1])
ctg1    <- readLAScatalog(dirLAZ1)
print(ctg1@data)
plot(ctg1)

dirLAZ2 <- paste0(lidarDir, anoData[2])
ctg2    <- readLAScatalog(dirLAZ2)
print(ctg2@data)
plot(ctg2)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Este script foi criado em Julho/2022 depois de instalar, na seguinte
# sequência, as mais recentes versões do pacote lidR*:
#
#   devtools::install_github("Jean-Romain/rlas", dependencies=TRUE)
#   devtools::install_github("Jean-Romain/lidR")
#
#   * lidR latest development version
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~