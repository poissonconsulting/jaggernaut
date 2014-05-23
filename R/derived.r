
get_samples <- function (sims, chains, data, parm, file) {
  
  fun <- function (parm, data, file) {
    # could remove unnecessary data so not need to suppress warning messages...
    warn <- options('warn')
    options(warn = -1)
    
    jags <- jags.model (file = file, data = data, 
                        n.chains = 1, n.adapt = 0, quiet = TRUE
    )
    
    samples <- jags.samples(
      model = jags, variable.names = parm, n.iter = 1
    )
    options (warn)
    
    return (samples)
  }
  
  nchains <- nchains (chains)
  
  list <- list ()
  for (j in 1:nchains) {
    
    list[[j]] <- fun(parm, data = c(data, 
                                    as.list(subset(chains, 
                                                   sim = sims[1], 
                                                   chain = j))),
                     file = file)    
    
    if (length(sims) > 1) {
      for (i in 2:length(sims)) {
        samples <- fun (parm,
                        data = c(data, 
                                 as.list(subset(chains, 
                                                sim = sims[i], 
                                                chain = j))), 
                        file = file)
        
        list[[j]] <- combine(list[[j]], samples, by = "sims")
      }
    }
  }
  
  samples <- list [[1]]
  
  if (nchains > 1) {
    for (j in 2:nchains)
      samples <- combine(samples, list[[j]], by = "chains")
  }
  return (samples)
}

derived <- function (object, parm, data, nworkers) {
  
  stopifnot(is.jags_analysis(object) && is_one_model(object))
  stopifnot(is_character_scalar(parm))
  stopifnot(is_convertible_data(data))
  assert_that(is.count(nworkers) && noNA(nworkers))
  
  if (options()$jags.pb != "none") {
    jags.pb <- options()$jags.pb
    options(jags.pb = "none")
    on.exit(options("jags.pb" = jags.pb))
  }  
  
  if(is.null(select_derived(object))) {
    data <- translate_data(select(object), dataset(object), data) 
  } else {
    data <- translate_data(select_derived(object), dataset(object), data) 
  }
  
  chains <- zero_random (object, data)
  
  if (is.function(modify_data_derived(object)))
    data <- modify_data_derived(object)(data)
  
  file <- tempfile(fileext=".bug")
  cat(derived_code(object), file=file)
    
  nchains <- nchains (chains)
  nsims <- nsims (chains) / nchains
  
  if(nworkers == 1) {
    samples <- get_samples(sims = 1:nsims, chains = chains, data = data, 
                 parm = parm, file = file)
  } else {
    i <- NULL
    samples <- foreach(i = isplitIndices(n = nsims, chunks = nworkers),
                       .combine = combine_lists_by_sims, 
                       .export = "get_samples") %dopar% {
                         get_samples(i, chains = chains, data = data, 
                                     parm = parm, file = file)
                       }
  }
    
  newobject <- list()
  class(newobject) <- "jagr_chains"
  
  samples(newobject) <- samples
  jags(newobject) <- list(NULL)
  random(newobject) <- names(random_effects(object))
    
  return (newobject)
}
