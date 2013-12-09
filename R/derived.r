
derived <- function (object, parm, data, nworkers) {
  
  stopifnot(is.jags_analysis(object) && is_one_model(object))
  stopifnot(is_character_scalar(parm))
  stopifnot(is.jags_data(data))
  assert_that(is.count(nworkers) && noNA(nworkers))
  
  if (options()$jags.pb != "none") {
    jags.pb <- options()$jags.pb
    options(jags.pb = "none")
    on.exit(options("jags.pb" = jags.pb))
  }  
  
  dat <- translate_data(select(object), data_jags(object), data) 
      
  chains <- zero_random (object, dat)
  
  if (is.function(modify_data_derived(object)))
    dat <- modify_data_derived(object)(dat)
  
  file <- tempfile(fileext=".bug")
  cat(derived_code(object), file=file)
    
  nchains <- nchains (chains)
  nsims <- nsims (chains) / nchains
  
  get_samples <- function (monitor, data, file) {
    # could remove unnecessary data so not need to suppress warning messages...
    warn <- options('warn')
    options(warn = -1)
    
    jags <- jags.model (file = file, data = data, 
                               n.chains = 1, n.adapt = 0, quiet = TRUE
    )
    
    samples <- jags.samples(
      model = jags, variable.names = monitor, n.iter = 1
    )
    options (warn)
    
    return (samples)
  }
  
  list <- list ()
  for (j in 1:nchains) {
    
    list[[j]] <- get_samples (parm,data = c(dat,as.list(subset_jags(chains, sim = 1, chain = j))),file = file)    
    
    if (nsims > 1) {
      for (i in 2:nsims) {
        samples <- get_samples (parm,data = c(dat,as.list(subset_jags(chains, sim = i, chain = j))), file = file)
        
        list[[j]] <- combine(list[[j]], samples, by = "sims")
      }
    }
  }    
  samples <- list [[1]]
    
  if (nchains > 1) {
    for (j in 2:nchains)
      samples <- combine(samples, list[[j]], by = "chains")
  }
  
  newobject <- list()
  class(newobject) <- "jagr_chains"
  
  samples(newobject) <- samples
  jags(newobject) <- list(NULL)
  random(newobject) <- names(random_effects(object))
    
  return (newobject)
}
