
jags_analysis_internal <- function (data, file, monitor, inits, n.chain, n.adapt, n.burnin, n.sim, n.thin, quiet) {
  stopifnot(is.null(monitor) || is.character(monitor))
  stopifnot(is.list(data))
  stopifnot(is.null(inits) || is.list(inits))
    
  n.adapt <- as.integer(n.adapt)
  n.burnin <- as.integer(n.burnin)
  n.sim <- as.integer(n.sim)
  n.chain <- as.integer(n.chain)
  
  jags_model_internal <- function (..., inits) {
    if (!length(inits)) {
      return (jags.model (...))
    }
    return (jags.model (..., inits = inits))
  }
    
  jags <- jags_model_internal (file = file, data = data, inits = inits, 
                      n.chains = n.chain, n.adapt = n.adapt, quiet = quiet)
  if (n.burnin) 
    update(jags, n.iter = n.burnin)

  if(is.null(monitor)) {
    monitor <- variable.names(jags)
    monitor <- monitor[!monitor %in% names(data)]
    bol <- substr(monitor,1,1) %in% c('d','e','i')
    bol <- bol & substr(monitor,2,2) == toupper(substr(monitor,2,2))
    monitor <- monitor[!bol]
  }
  monitor <- c(monitor, "deviance")
  monitor <- sort(unique(monitor))
  
  mcmc <- jags.samples(
    model = jags, variable.names = monitor, n.iter = n.sim, thin = n.thin
  )

  chains <- jagr_chains(mcmc=mcmc,jags=list(jags))
    
  return (chains)
}
