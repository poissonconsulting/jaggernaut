jagr_analysis <- function (model, data, niters, nworkers) {    
  
  assert_that(is.jagr_model(model))
  assert_that(is_convertible_data(data))
  assert_that(is.count(niters) && noNA(niters))
  assert_that(is.count(nworkers) && noNA(nworkers))
  
  resample <- opts_jagr("nresample")
  nchains <- opts_jagr("nchains")
  quiet <- opts_jagr("quiet")
  rhat_threshold <- opts_jagr("rhat")
  
  if(!is.null(monitor(model)))
    monitor(model) <- sort(unique(monitor(model)))
  
  data <- translate_data(select(model), data) 
  
  if (is.function(modify_data(model))) 
    data <- modify_data(model)(data)
  
  assert_that(is_converted_data(data))
  
  if (is.function(gen_inits(model))) {
    inits <- list()
    for (i in 1:nchains) {   
      inits[[i]] <- gen_inits(model)(data)
    }
  } else
    inits <- NULL
  
  assert_that(is_converted_data(data))
  assert_that(is.count(niters) && noNA(niters))
  assert_that(is.count(nworkers) && noNA(nworkers))
  
  nsamples <- opts_jagr("nsamples")  
  
  niters <- ceiling(max(niters, nsamples * 2 / nchains))
  
  n.adapt <- 100
  n.burnin <- as.integer(niters / 2)
  n.thin <- max(1, floor(nchains * n.burnin / nsamples))
  nsamples <- as.integer(niters /2)
  
  ptm <- proc.time()
  
  rngs <- parallel.seeds("base::BaseRNG", nchains)
  
  if (!is.null (inits)) {
    for (i in 1:nchains)
      inits[[i]] <- c(inits[[i]],rngs[[i]])
  } else
    inits <- rngs
  
  file <- tempfile(fileext=".bug")
  cat(model_code(model), file=file)
  
  if(nchains == 1 || nworkers == 1) {
    chains <- jags_analysis_internal(inits, data, file = file, 
                                     monitor = monitor(model),
                                     n.adapt = n.adapt, 
                                     n.burnin = n.burnin, n.chain = nchains, 
                                     n.sample = nsamples, n.thin = n.thin, 
                                     random = names(random_effects(model)))
  } else {
    i <- NULL
    chains <- foreach(i = isplitIndices(n = nchains, chunks = nworkers),
                      .combine = combine_jagr_chains, 
                      .export = "jags_analysis_internal") %dopar% {
                        jags_analysis_internal(inits[i], data, file = file, 
                                               monitor = monitor(model),
                                               n.adapt = n.adapt, 
                                               n.burnin = n.burnin, n.chain = length(i),
                                               n.sample = nsamples, n.thin = n.thin, 
                                               random = names(random_effects(model)))
                      } 
  }
    
  monitor(model) <- monitor(chains)

  object <- model
  
  class(object) <- c("jagr_analysis", "jagr_model")
  
  init_values(object) <- inits
  chains(object) <- chains
  niters(object) <- niters
  time_interval(object) <- ((proc.time () - ptm)[3]) / (60 * 60)
  
  while (!is_converged (object, rhat_threshold = rhat_threshold) && resample > 0)  {
    if(!quiet) {
      cat ("Resampling due to convergence failure")
      cat_convergence (object)  
    }
    
    resample <- resample - 1
    
    object <- update(object, nworkers = nworkers)
  }
  
  if(!quiet) {
    if (is_converged (object, rhat_threshold = rhat_threshold)) {
      cat ('Analysis converged')
      cat_convergence (object)
    }  else {
      cat ('Analysis failed to converge')
      cat_convergence (object)      
    }
  }
  
  return (object)
}

jagr_analysis_list <- function (models, data, niters, nworkers) {
  assert_that(is_list(models))
  
  lapply(models, jagr_analysis, data = data, niters = niters, nworkers = nworkers)
}
