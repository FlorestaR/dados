# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Inventário com dados LiDAR na Fazenda Modelo ~~~~~~~~~~~~~~~~~~~~~~~~
#
# Autor: Luiz Carlos Estraviz Rodriguez
#        Departamento de Ciências Florestais
#        ESALQ/USP - 16/Jul/2022
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
if(!require(kableExtra))                        # Para melhores tabelas
  install.packages("kableExtra")
library(kableExtra)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Download do shape da Fazenda Modelo (2 layers: talhoes e parcelas)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
gitOnde <- "https://github.com/FlorestaR/dados/blob/main/5_LIDARF/Modelo/SHAPES"
gitNome <- "fazmodelo.zip"
gitArqv <- file.path(gitOnde, gitNome) %>% paste0("?raw=true")

tmpd <- tempdir(check = TRUE)                    # diretório temporário
zipf <- file.path(tmpd, "shapes.zip")              # arquivo temporário

if(!file.exists(zipf))  # garante download de dados binários (wb)
  download.file(gitArqv, mode="wb", destfile = zipf) 

unzip(zipf, exdir = tmpd)     # shape é unziped no diretório temporário
unlink(zipf)                                  # deleta o arquivo zipado

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Lê atributos dos talhoes
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
shpArq <- paste0(tmpd, "/Modelo_talhoes.shp")       # shape com talhões
talhoesComGeo <- sf::read_sf(shpArq)                # completo com geom
talhoesSemGeo <- tibble(sf::st_drop_geometry(talhoesComGeo))  # s/ geom

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Lê atributos das parcelas
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
shpArq <- paste0(tmpd, "/Modelo_parcelas.shp")     # shape com parcelas
parcelasComGeo <- sf::read_sf(shpArq)               # completo com geom
parcelasSemGeo <-tibble(sf::st_drop_geometry(parcelasComGeo)) # s/ geom

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Cria coluna IDINV na tabela "talhões", extraída da tabela "parcelas"
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
talhoes <-
parcelasSemGeo %>%
  group_by(SUBTALHAO) %>%
  summarise(IDINV = unique(IDINV)) %>%
  left_join(talhoesSemGeo, join = "SUBTALHAO") %>%
  select("SUBTALHAO", "IDINV", "AREA") %>%
  arrange(SUBTALHAO) %>% as.data.frame

talhoesComGeo <- inner_join(talhoesComGeo, talhoes, by="SUBTALHAO") %>%
  select("OBJECTID", "SUBTALHAO", "IDINV", "geometry")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Reorganiza colunas da tabela "parcelas"
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
parcelas <-
  parcelasSemGeo %>%
  select(SUBTALHAO, CHAVE2, DATAREALIZ, IDINV, AREAPARCEL, MHDOM, VTCC, AB) %>% 
  arrange(SUBTALHAO) %>% as.data.frame

# Imprime tabela de talhões
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
AreaTotal <- talhoes$AREA %>% sum
NotaDeRodape <- paste0(": ", AreaTotal)
talhoes %>%
  kbl(caption = "Talhões da Fazenda Modelo", align = "r") %>%
  kable_classic(full_width = F) %>%
  footnote(general = NotaDeRodape, 
           general_title = "Área total",
           footnote_as_chunk = T)

# Imprime tabela de parcelas
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
parcelas %>%
  kbl(caption = "Parcelas da Fazenda Modelo", align = "r") %>%
  kable_classic(full_width = F)

# Número Total de Unidades Amostrais (N) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
parcelaAreaMedia <- mean(parcelas$AREAPARCEL) / 10000           # em ha
tamanhoPopulacao <- round(AreaTotal / parcelaAreaMedia , 0)

# Mapa dos talhões com localização das parcelas (EPSG: 31983)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ggplot() +                # plot dos talhões e parcelas (col por idade)
  geom_sf(data =  talhoesComGeo, colour = "black", fill="white") +
  geom_sf(data = parcelasComGeo, aes(colour = IDINV)) +
  coord_sf(datum=st_crs(31983)) +        # Especifica sistema de coord.
  scale_y_continuous(breaks = seq(from=7356500,to=7359000, by=200)) +
  scale_x_continuous(breaks = seq(from=206200, to=207600,  by=200))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Amostragem Casual Simples (ACS): VTCC estimação e inferência
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Intervalo de Confiança
calcCI = function(err, n, alpha=.05){
  return(
    qt(1 - alpha/2, n-1) * err #/ sqrt(n)
  )
}

# Parâmetros de Estimação e Inferência
calcPars = function(df, N, alpha=.05){
  means = apply(df, 2, mean)
  vars   = apply(df, 2, function(y){
    (var(y) / length(y)) * ((N - length(y)) / N)
  })
  err = sqrt(vars)
  cis = calcCI(err, nrow(df), alpha)
  err_pc = 100*cis/means
  out = rbind(means, vars, err, cis, err_pc, nrow(df)) %>% as.data.frame
  row.names(out) = c('media', 'var', 'dp', 'ic', 'erro', 'n')
  names(out) = names(df)
  return(out)
}

variaveisDeInteresse <- c("MHDOM", "VTCC", "AB")
df <- parcelas %>% select(all_of(variaveisDeInteresse))

simplePars  <- df %>% calcPars(tamanhoPopulacao)

# Intensidade amostral (ha por parcela) da ACS que garante ~10% de erro 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
tamanhoIdealACS = function(y, N, errDesired=.10, alpha=.10){
  B  = errDesired * mean(y)
  qt = qt(1 - alpha/2, length(y)-1)
  n  = N*var(y)*qt^2 / (N * B^2 + qt^2 * var(y))
  return(n)
}
IA <- round(AreaTotal/tamanhoIdealACS(df$VTCC,tamanhoPopulacao),0)

# Resultados do inventário (estimação + inferência) por ACS
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
IAS <- paste0(" usada: 1 parc/", round(AreaTotal/simplePars$VTCC[6],0), " ha.")
IAI <- paste0("\n Necessárias p/ erro de 10%: 1 parc/", IA, " ha.")
NotaDeRodape <- paste0(IAS, IAI)
simplePars %>%
  kbl(caption = "Amostragem Casual Simples", align = "r") %>%
  kable_classic(full_width = F) %>%
  footnote(general = NotaDeRodape,
           general_title = "Intensidade amostral",
           footnote_as_chunk = T)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Amostragem Casual Estratificada (ACE): VTCC estimação e inferência
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Grupos de estratificação
grupos = c("IDINV")
rightAges = talhoes$IDINV < 6 & talhoes$IDINV > 2

# Box-plot das variáveis de interesse por nível de estratificação
par(mfrow=c( length(grupos), length(variaveisDeInteresse) ))
attach(parcelas)
for(i in grupos){
  for(j in variaveisDeInteresse){
    plot( get(i) %>% factor , get(j), main = j, sub=i)
  }
}
detach(parcelas)

# Estimativa da população para amostragem estratificada
popFromStrata = function(factorStrataList){
  popEstimates = foreach(i = factorStrataList, .combine = 'c') %do% {
    gpMeans = lapply(i, function(x) x[1,,drop=F]) %>% do.call(what = rbind)
    gpVars  = lapply(i, function(x) x[2,,drop=F]) %>% do.call(what = rbind)
    cols = 1:(ncol(gpMeans)-4)
    popMean   = apply(gpMeans[,cols], 2, 
                      function(x) sum(x*gpMeans$N) ) /tamanhoPopulacao
    popVar    = apply(gpVars[,cols], 2, 
                      function(x) sum( x * (gpVars$N/tamanhoPopulacao)^2 ) )
    popStdErr = sqrt(popVar)
    popCI     = calcCI(popStdErr, sum(gpMeans$n))
    popPars = data.frame(
      media = popMean,
      var   = popVar,
      dp    = popStdErr,
      ic    = popCI,
      erro  = 100 * popCI / popMean,
      n     = sum( sapply(i, function(x) mean(x$n)) )
    )
    return(list(popPars))
  }
  names(popEstimates) = names(factorStrataList)
  return(popEstimates)
}

stratPars = foreach(
  grp = grupos, .combine = 'rbind') %:% 
  foreach(
    niv = parcelas[,grp] %>% unique %>% 
      as.character %>% sort, .combine = 'rbind'
  ) %do% {
  inGroup = talhoes[,grp] == niv
  popSize = round(
    sum(talhoes[rightAges & !is.na(inGroup) & inGroup,]$AREA) / 
      parcelaAreaMedia)
  inGroup = parcelas[,grp] == niv
  tempPars = parcelas[inGroup, variaveisDeInteresse, drop=F]
  inventory = calcPars(tempPars, popSize)
  inventory$grupo = grp
  inventory$nivel = niv
  inventory$n     = nrow(tempPars)
  inventory$N     = popSize
  return(inventory)
  }

stratPars %<>% base::split(f = stratPars$grupo) %>% 
  lapply(function(x) split(x, x$nivel))
globalStratPars = popFromStrata(stratPars)

# Intensidade amostral (ha por parcela) da ACE que garante ~10% de erro 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
tamanhoIdealACE = function(y, g, Nh, errDesired=.05){
  vars = by(y, g, stats::var)
  Wh   = by(y, g, length) / length(y)
  Nh = Nh[ names(Nh) %in% g ]
  B = errDesired * mean(y)
  n = sum( Nh^2 * vars / Wh ) / ( (sum(Nh)^2 * B^2)/4 + sum(Nh * vars) )
  }
stratAreas <- round(
  by(st_area(talhoesComGeo), talhoes$IDINV, sum) /
    (parcelaAreaMedia*10000), 0)
IA <- round(AreaTotal/tamanhoIdealACE(parcelas$VTCC,
                                      parcelas$IDINV, stratAreas, .10), 0)

# Resultados do inventário (estimação + inferência) por ACE
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
IAS <- paste0(" usada: 1 parc/", round(AreaTotal/simplePars$VTCC[6],0), " ha.")
IAI <- paste0("\n Necessárias p/ erro de 10%: 1 parc/", IA, " ha.")
NotaDeRodape <- paste0(IAS, IAI)
globalStratPars[[grupos]] %>% t %>%             # transpõe o data frame
  kbl(caption = paste0("Amostragem Casual Estratificada por ", grupos),
      align = "r") %>%
  kable_classic(full_width = F) %>%
  footnote(general = NotaDeRodape,
           general_title = "Intensidade amostral",
           footnote_as_chunk = T)

# Gráfico para análise da precisão dos métodos amostrais
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
errs = 1:20
scsPlotn <- tamanhoIdealACS(parcelas$VTCC, tamanhoPopulacao,
                            errDesired = errs/100)
cssPlotn <- tamanhoIdealACE(parcelas$VTCC, parcelas$IDINV, stratAreas,
                            errDesired = errs/100)
# dsPlotn = dubleSamplePlotNumber(plotMetrics$PC_VOL_TOT,
#                                 plotMetrics$avgCrownHeight,
#                                 cropMetrics$avgCrownHeight, 300, errs/100)

png('nParcelas.png', 15, 10, 'cm', res = 200)
plot( scsPlotn ~ errs, type='l', col='red', lwd=2, 
      ylim=c(0,200), ylab='Número de parcelas', xlab='Erro amostral (%)',
      axes=F)
lines(errs, cssPlotn, col='green', lwd=2)
# lines(errs, dsPlotn, col='blue', lwd=2)
box()
axis(1)
axis(2, at = c(25, 50, 75, 100, 125, 150, 175, 200))
abline(v=10, col='black', lwd=2, lty=2)
lines(c(0,15), rep(100, 2), col='black', lty=2, lwd=2)
# lines(c(0,5), rep(dsPlotn[5], 2), col='blue', lty=2, lwd=2)
# lines(c(0,5), rep(cssPlotn[5], 2), col='green', lty=2, lwd=2)
legend('topright', col=rainbow(3), lwd=2,
       legend = c('ACS', 'ACE'))
       # legend = c('ACS', 'ACE', 'AD'))
dev.off()

# save.image(file='Modelo_inventario.RData')
# load('Modelo_inventario.RData')

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Leitura de dados LidAR
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Define os nomes dos tiles LiDAR multitemporais normalizados que serão
# lidos do repositório, os respectivos anos e o URL completo do github
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
gitOnde <-"https://github.com/FlorestaR/dados/blob/main/5_LIDARF/Modelo/CLOUDS/"
gitNome <-c("L0004-C0005_gcn.laz", "L0004-C0006_gcn.laz",
            "L0005-C0005_gcn.laz", "L0005-C0006_gcn.laz",
            "L0006-C0005_gcn.laz", "L0006-C0006_gcn.laz")
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
# Amostragem Dupla (AD): VTCC estimação e inferência
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Este script foi criado em Julho/2022 depois de instalar, na seguinte
# sequência, as mais recentes versões do pacote lidR*:
#
#   devtools::install_github("Jean-Romain/rlas", dependencies=TRUE)
#   devtools::install_github("Jean-Romain/lidR")
#
#   * lidR latest development version
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~