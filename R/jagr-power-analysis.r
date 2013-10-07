
jagr_power_analysis <- function (model_code, data, niters, inits, monitor = NULL) {      
  nchains <- opts_jagr("nchains")
  nsims <- opts_jagr("nsims")  
  parallelChains <- opts_jagr("parallel_chains")
  quiet <- opts_jagr("quiet")
  
  niters <- ceiling(max(niters, nsims * 2 / nchains))

  n.adapt <- 100
  n.burnin <- as.integer(niters /2)
  n.thin <- max(1, floor(nchains * n.burnin / nsims))
  nsims <- as.integer(niters /2)
  
  ptm <- proc.time()
  
  if (parallelChains) {
          
    rngs<-parallel.seeds("base::BaseRNG", nchains)
    
    if (!is.null (inits)) {
      for (i in 1:nchains)
        inits[[i]] <- c(inits[[i]],rngs[[i]])
    } else
      inits <- rngs
    
    doMC::registerDoMC(cores=nchains)
    
    mcmc <- foreach::foreach(i = 1:nchains, .combine = add_jags_jagr_chains) %dopar% { 
      file <- tempfile(fileext=".bug")
      cat(model_code, file=file)
      
      jags_analysis_internal (
        data = data, file=file, monitor = monitor, 
        inits = inits[i], n.chain = 1, 
        n.adapt = n.adapt, n.burnin = n.burnin, 
        n.sim = nsims, n.thin = n.thin, quiet = quiet
      )
    }
  } else {    
    file <- tempfile(fileext=".bug")
    cat(model_code, file=file)
    
    mcmc <- jags_analysis_internal (
      data = data, file=file, monitor = monitor, 
      inits = inits, n.chain = nchains, 
      n.adapt = n.adapt, n.burnin = n.burnin, 
      n.sim = nsims, n.thin = n.thin, quiet = quiet
    )
  }
  
  object <- list()
  
  class(object) <- c("jagr_power_analysis")
  
  init_values(object) <- inits
  chains(object) <- mcmc
  niters(object) <- niters
  time_interval(object) <- ((proc.time () - ptm)[3]) / (60 * 60)
  
  return (object)
}
