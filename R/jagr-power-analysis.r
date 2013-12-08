
jagr_power_analysis <- function (model_code, data, niters, inits, 
                                 monitor = NULL,
                                 random = NULL) { 
  
  stopifnot(is.jagr_data(data))
  
  nchains <- opts_jagr("nchains")
  nsims <- opts_jagr("nsims")  
  parallel <- opts_jagr("parallel")
  
  niters <- ceiling(max(niters, nsims * 2 / nchains))

  n.adapt <- 100
  n.burnin <- as.integer(niters / 2)
  n.thin <- max(1, floor(nchains * n.burnin / nsims))
  nsims <- as.integer(niters /2)
  
  ptm <- proc.time()
            
  rngs <- parallel.seeds("base::BaseRNG", nchains)
  
  if (!is.null (inits)) {
    for (i in 1:nchains)
      inits[[i]] <- c(inits[[i]],rngs[[i]])
  } else
    inits <- rngs
  
  file <- tempfile(fileext=".bug")
  cat(model_code, file=file)
  
  if(!parallel || opts_jagr("mode") == "debug" ||
       getDoParWorkers() < opts$nchains) {
    chains <- jags_analysis_internal(inits, data, file = file, 
                                     monitor = monitor,
                                     n.chain = nchains, 
                                     n.adapt = n.adapt, 
                                     n.burnin = n.burnin, 
                                     n.sim = nsims, n.thin = n.thin, 
                                     random = random)
  } else {
    chains <- foreach(i = 1:nchains, .combine=combine_jagr_chains) %dopar% {
      jags_analysis_internal(inits[i], data, file = file, 
                             monitor = monitor,
                             n.adapt = n.adapt, 
                             n.burnin = n.burnin, 
                             n.sim = nsims, n.thin = n.thin, 
                             random = random)
    }
  } 
  object <- list()
  
  class(object) <- c("jagr_power_analysis")
  
  init_values(object) <- inits
  chains(object) <- chains
  niters(object) <- niters
  time_interval(object) <- ((proc.time () - ptm)[3]) / (60 * 60)
  
  return (object)
}
