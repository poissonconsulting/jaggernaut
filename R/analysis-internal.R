analysis_internal <- function (inits, data, file, monitor, n.chain = 1, 
                                    n.adapt = 0, n.burnin = 0, n.sample = 1, 
                                    n.thin = 1, random = NULL) {
  
  stopifnot(is.character(monitor) && not_empty(monitor))
  stopifnot(is_converted_data(data))
  
  assert_that(is.count(n.chain) && noNA(n.chain))
  assert_that(is.null(inits) || is.list(inits))
  assert_that(is.null(inits) || length(inits) == n.chain)

  n.adapt <- as.integer(n.adapt)
  n.burnin <- as.integer(n.burnin)
  n.sample <- as.integer(n.sample)
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
  
  vars <- variable.names(jags)
  vars <- vars[!vars %in% names(data)]
  
  if(is.string(monitor)) {
    vars <- vars[grepl(monitor, vars, perl = TRUE)]
  } else
    vars <- vars[vars %in% sub("-$", "", monitor)]
  
  monitor <- sort(unique(vars))
  
  samples <- jags.samples(
    model = jags, variable.names = monitor, n.iter = n.sample, thin = n.thin
  )
  
  object <- list()
  class(object) <- "jagr_chains"
  
  samples(object) <- samples
  jags(object) <- list(jags)
  random(object) <- random
  
  return (object)
}

