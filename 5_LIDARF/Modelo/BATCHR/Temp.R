
# Confidence interval
calcCI = function(err, n, alpha=.05){
  return(
    qt(1 - alpha/2, n-1) * err #/ sqrt(n)
  )
}

# Sample parameters
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

# ideal number of plots for simple casual sampling
idealPlotNumber = function(y, N, errDesired=.05, alpha=.05){
  B = errDesired * mean(y)
  qt = qt(1 - alpha/2, length(y)-1)
  
  n = N*var(y)*qt^2 / (N * B^2 + qt^2 * var(y))
  
  return(n)
}

# ideal plots for stratified sample
stratPlotNumber = function(y, g, Nh, errDesired=.05, alpha=.05){
  vars = by(y, g, stats::var)
  Wh   = by(y, g, length) / length(y)
  
  Nh = Nh[ names(Nh) %in% g ]
  
  B = errDesired * mean(y)
  
  n = sum( Nh^2 * vars / Wh ) / ( (sum(Nh)^2 * B^2)/4 + sum(Nh * vars) )
  
}

# ideal number of plots for double sampling
dubleSamplePlotNumber = function(y, x, xLarge, Cpg = 300, errDesired = .05, alpha = .05){
  
  rho = cor(x,y)
  a = var(y) * (1 - rho^2)
  b = var(y) * rho^2
  
  B = errDesired * mean(y)
  qt = qt(1 - alpha/2, length(y)-1)
  
  nG = (sqrt( a*b*Cpg ) + b) / (B^2 / qt^2)
  nP = (sqrt( a*b/Cpg ) + a) / (B^2 / qt^2)
  
  return(nP)
}

# double sampling regression estimations
doubleSampleRegPars = function(y, x, xLarge, alpha=.05){
  n = length(y)
  beta = ( sum(y*x, na.rm=T) - ( sum(x, na.rm=T)*sum(y, na.rm=T) / n ) ) / ( sum(x^2, na.rm=T) - (sum(x, na.rm=T)^2 / n) )
  
  rho = cor(y,x)
  N = length(xLarge)
  
  ydsr = mean(y, na.rm=T) + beta * ( mean(xLarge, na.rm=T) - mean(x, na.rm=T) )
  vardsr = (var(y, na.rm=T)/n)*(1 - (rho^2)*(N-n)/N)
  stderr = sqrt(vardsr)
  ci     = calcCI(stderr, n, alpha)
  
  out = c(mean = ydsr, mean_var = vardsr, std_err = stderr, ci = ci, err_pc = 100*ci/ydsr, n=n, rho=rho)
  
  return(out)
}

# double sampling ratio estimations
doubleSampleRatioPars = function(y, x, xLarge, popSize, alpha=.05){
  
  n = length(y)
  N = length(xLarge)
  
  Rhat = mean(y) / mean(x)
  varLarge = var(xLarge) 
  covXY = cov(x,y)
  k = var(y) + Rhat^2 * varLarge - 2*Rhat*covXY
  l = 2*Rhat*covXY - Rhat^2*varLarge
  m = -(var(y)/popSize)
  
  ydsr   = Rhat * mean(xLarge)
  vardsr = k/n + l/N + m #abs()
  stderr = sqrt(vardsr)
  ci     = calcCI(stderr, n, alpha)
  
  out = c(mean = ydsr, mean_var = vardsr, std_err = stderr, ci = ci, err_pc = 100*ci/ydsr, n=n)
  
  return(out)
}

# population estimation from strata
popFromStrata = function(factorStrataList){
  
  popEstimates = foreach(i = factorStrataList, .combine = 'c') %do% {
    gpMeans = lapply(i, function(x) x[1,,drop=F]) %>% do.call(what = rbind)
    gpVars  = lapply(i, function(x) x[2,,drop=F]) %>% do.call(what = rbind)
    
    cols = 1:(ncol(gpMeans)-4)
    popMean   = apply(gpMeans[,cols], 2, function(x) sum(x*gpMeans$N) ) / populationSize
    popVar    = apply(gpVars[,cols], 2, function(x) sum( x * (gpVars$N/populationSize)^2 ) )
    popStdErr = sqrt(popVar)
    popCI     = calcCI(popStdErr, sum(gpMeans$n))
    
    popPars = data.frame(
      mean     = popMean,
      mean_var = popVar,
      std_err  = popStdErr,
      ci       = popCI,
      err_pc   = 100 * popCI / popMean,
      n        = sum( sapply(i, function(x) mean(x$n)) )
    )
    
    return(list(popPars))
  }
  
  names(popEstimates) = names(factorStrataList)
  return(popEstimates)
}

{
  # get kmeans stratum for each sample plot 
  temp = shapefile('SIG/SHAPES/Parcelas_Centroids.shp')
  spatialPlotMetrics = SpatialPointsDataFrame(plotMetrics[,c('X','Y')], plotMetrics, proj4string = temp@proj4string) %>% spTransform(ctg@crs)
  rm(temp); gc()
  
  metrics = read.csv('full_info.csv')[,-1]
  
  spatialPlotMetrics$kMeans = spatialPlotMetrics@coords %>% apply(1, function(x){
    minDist = sqrt( (x[1] - metrics$X)^2 + (x[2] - metrics$Y)^2 ) %>% order(decreasing = F) %>% head(1)
    return(metrics$kMeans[minDist])
  })
  
  # extract all information of plots' location  
  groups = apply(spatialPlotMetrics@data, 2, function(x) x[!is.na(x)] %>% unique %>% length )
  groups = groups[groups <= 10 & groups > 1] %>% names
  
  # stratification groups
  groups = c('DCR_MATERI', 'CLASS_IDADE', 'DCR_ESPACA', 'kMeans') #, CD_REGIME)
  
  # set variables of interest
  interestVars = c('PC_VOL_TOT', 'PC_DAP', 'PC_HT', 'PC_AB', 'PC_DG', 'PC_H100', 'NFUSTES')
  
  # check stratification classes
  # par(mfrow=c( length(groups), length(interestVars) ))
  # attach(spatialPlotMetrics@data)
  # for(i in groups){ 
  #   for(j in interestVars){
  #     plot( get(i) %>% factor , get(j), main = j, sub=i)
  #   }
  # }
  # detach(spatialPlotMetrics@data)
  
  # get population size
  populationSize = raster::area(talhoes[talhoes$IDADE_PLAN < 6 & talhoes$IDADE_PLAN > 2,]) %>% sum / mean(spatialPlotMetrics$AREA)
  populationSize = raster::area(talhoes) %>% sum / mean(spatialPlotMetrics$AREA)
  
  # get casual sampling info
  simplePars = spatialPlotMetrics@data[,interestVars] %>% calcPars(populationSize)
  
  # get stratified sampling info
  rightAges = talhoes$IDADE_PLAN < 6 & talhoes$IDADE_PLAN > 2
  stratPars = foreach(
    fac = groups, .combine = 'rbind') %:% 
    foreach(
      g = spatialPlotMetrics@data[,fac] %>%
        unique %>% as.character %>%
        sort, .combine = 'rbind'
      ) 
  %do% {
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
  
  stratPars %<>% base::split(f = stratPars$factor) %>% lapply(function(x) split(x, x$group))
  globalStratPars = popFromStrata(stratPars)
  
  # get double sampling parameters
  lims = spatialPlotMetrics@data %>% names %in% c('hMean', 'sdCrownRelief') %>% which
  lidarOnly = spatialPlotMetrics@data[,lims[1]:lims[2]]
  
  corrMat = cor(spatialPlotMetrics@data[,interestVars], lidarOnly)
  corrMat = corrMat[,apply(corrMat, 2, function(x) !any(is.na(x)))]
  
  doubleSamplePars = foreach(i = interestVars, .combine = 'rbind') %do% {
    aux = corrMat[i,]
    pick = which( aux == max(aux) ) %>% names
    XL = cropMetrics@data[cropMetrics@data$IDADE_PLAN > 2 & cropMetrics@data$IDADE_PLAN < 6,pick]
    
    est = doubleSampleRegPars(spatialPlotMetrics@data[,i], spatialPlotMetrics@data[,pick], XL) %>% data.frame()
    # est = doubleSampleRatioPars(spatialPlotMetrics@data[,i], spatialPlotMetrics@data[,pick], XL, populationSize) %>% data.frame()
    names(est) = i
    
    est %<>% t %>% as.data.frame
    est$aux = pick
    
    return(est)
  }
  
  # get stratified double sampling info
  rightAges = talhoes$IDADE_PLAN < 6 & talhoes$IDADE_PLAN > 2
  rightAgesPixels = cropMetrics@data$IDADE_PLAN < 6 & cropMetrics@data$IDADE_PLAN > 2
  stratDoublePars = foreach(fac = groups, .combine = 'rbind') %:% foreach(g = spatialPlotMetrics@data[,fac] %>% unique %>% as.character %>% sort, .combine = 'rbind') %do% {
    inGroup = talhoes@data[,fac] == g
    popSize = sum( raster::area(talhoes[rightAges & !is.na(inGroup) & inGroup,] )) / mean(spatialPlotMetrics$AREA)
    
    inGroup  = spatialPlotMetrics@data[,fac] == g
    tempPars = spatialPlotMetrics@data[inGroup,interestVars,drop=F]
    
    pixelPars = cropMetrics@data[,fac] == g & rightAgesPixels
    
    inventory = sapply(names(tempPars), function(x){
      covar = corrMat[x,] %>% sort %>% tail(1) %>% names
      covPlot = spatialPlotMetrics@data[inGroup,covar]
      dPars = doubleSampleRegPars(tempPars[,x], covPlot, cropMetrics@data[pixelPars,covar])
      return(dPars)
    }) %>% as.data.frame
    
    inventory$factor = fac
    inventory$group  = g
    inventory$n      = nrow(tempPars)
    inventory$N      = popSize
    
    return(inventory)
  }
  
  stratDoublePars %<>% base::split(f = stratDoublePars$factor) %>% lapply(function(x) split(x, x$group))
  globalDoubleStratPars = popFromStrata(stratDoublePars)
  
  # combine all results
  inventoryTables = list(
    simpleCasualSample     = simplePars %>% t,
    stratifiedSample       = globalStratPars,
    doubleSample           = doubleSamplePars,
    stratifiedDoubleSample = globalDoubleStratPars,
    perStratum             = stratPars,
    perStratumDouble       = stratDoublePars
  )
  
  # ideal plot numbers
  errs = 1:20
  scsPlotn = idealPlotNumber(plotMetrics$PC_VOL_TOT, populationSize, errDesired = errs/100)
  
  stratAreas = by(area(talhoes), talhoes$CLASS_IDADE, sum) / mean(parcelas$AREA)
  cssPlotn = stratPlotNumber(plotMetrics$PC_VOL_TOT, plotMetrics$CLASS_IDADE, stratAreas, errs/100)
  
  dsPlotn = dubleSamplePlotNumber(plotMetrics$PC_VOL_TOT, plotMetrics$avgCrownHeight, cropMetrics$avgCrownHeight, 300, errs/100)
  
}



