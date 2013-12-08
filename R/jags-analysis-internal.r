
jags_analysis_internal <- function (inits, data, file, monitor, n.chain = 1, 
                                    n.adapt = 0, n.burnin = 0, n.sim = 1, 
                                    n.thin = 1, random = NULL) {
  stopifnot(is.null(monitor) || is.character(monitor))
  stopifnot(is.jagr_data(data))
  stopifnot(is.null(inits) || is.list(inits))
  
  n.adapt <- as.integer(n.adapt)
  n.burnin <- as.integer(n.burnin)
  n.sim <- as.integer(n.sim)
  n.chain <- as.integer(n.chain)
  
  jags.model_jg <- function (..., inits) {
    quiet <- opts_jagr("mode") != "debug"
    
    if (!length(inits)) {
      return (jags.model (..., quiet = quiet))
    }
    return (jags.model (..., inits = inits, quiet = quiet))
  }
  
  jags <- jags.model_jg (file = file, data = data, inits = inits, 
                               n.chains = n.chain, n.adapt = n.adapt)
  if (n.burnin > 0) 
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
  
  samples <- jags.samples(
    model = jags, variable.names = monitor, n.iter = n.sim, thin = n.thin
  )
  
  object <- list()
  class(object) <- "jagr_chains"
  
  samples(object) <- samples
  jags(object) <- list(jags)
  random(object) <- random
  
  return (object)
}

