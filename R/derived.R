get_samples <- function (samples, chains, data, parm, file) {
  
  fun <- function (parm, data, file) {
    # could remove unnecessary data so not need to suppress warning messages...
    warn <- options('warn')
    options(warn = -1)
        
    jags <- jags.model (file = file, data = data, 
                        n.chains = 1, n.adapt = 0, quiet = TRUE
    )
    
    samps <- jags.samples(
      model = jags, variable.names = parm, n.iter = 1
    )
    options (warn)
    
    return (samps)
  }
  
  nchains <- nchains (chains)
  
  list <- list ()
  for (j in 1:nchains) {
    
    list[[j]] <- fun(parm, data = c(data, 
                                    as.list(subset(chains, 
                                                   sample = samples[1], 
                                                   chain = j))),
                     file = file)    
    
    if (length(samples) > 1) {
      for (i in 2:length(samples)) {
        samps <- fun (parm,
                        data = c(data, 
                                 as.list(subset(chains, 
                                                sample = samples[i], 
                                                chain = j))), 
                        file = file)
        
        list[[j]] <- combine(list[[j]], samps, by = "samples")
      }
    }
  }
  
  samps <- list [[1]]
  
  if (nchains > 1) {
    for (j in 2:nchains)
      samps <- combine(samps, list[[j]], by = "chains")
  }
  return (samps)
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
  
  if(is.null(select_data_derived(object))) {
    data <- translate_data(select_data(object), dataset(object), data) 
  } else {
    data <- translate_data(select_data_derived(object), dataset(object), data) 
  }
    
  chains <- zero_random (object, data)
  
  if (is.function(modify_data_derived(object)))
    data <- modify_data_derived(object)(data)
  
  file <- tempfile(fileext=".bug")
  code <- derived_code(object) 
  code <- paste(code,"model { deviance <- 1}")
  
  cat(code, file=file)
    
  nchains <- nchains (chains)
  nsamples <- nsamples (chains) / nchains
  
  if(nworkers == 1) {
    samps <- get_samples(samples = 1:nsamples, chains = chains, data = data, 
                 parm = parm, file = file)
  } else {
    i <- NULL
    samps <- foreach(i = isplitIndices(n = nsamples, chunks = nworkers),
                       .combine = combine_lists_by_samples, 
                       .export = "get_samples") %dopar% {
                         get_samples(i, chains = chains, data = data, 
                                     parm = parm, file = file)
                       }
  }
    
  newobject <- list()
  class(newobject) <- "jagr_chains"
  
  samples(newobject) <- samps
  jags(newobject) <- list(NULL)
    
  return (newobject)
}
