
jagr_analysis <- function (
  model, data, n.iter = 1000, n.chain = 3, resample = 3,
  convergence = 1.1, independence = 0,
  parallelChains = .Platform$OS.type != "windows", 
  quiet = FALSE, n.sim = 1000
)
{  
  stopifnot(is.jags_model(model))
  
  random_variables <- names(random_effects(model))
  
  model <- as.jagr_model(model)
    
  stopifnot(n.iter >= 100)
  stopifnot(n.chain %in% 2:6)
  stopifnot(resample %in% 0:4)
  stopifnot(convergence >= 1.0 && convergence <= 2.0)
  stopifnot(independence %in% 0:100)
  stopifnot(n.sim >= 100 && n.sim <= 2000)
  
  cat_convergence <- function (object) {
    cat (' (Rhat:')
    cat (rhat(object))
    cat (')\n')
  }
  
  n.iter <- ceiling(max(n.iter, n.sim * 2 / n.chain))
    
  if (quiet) {
    options(jags.pb = "none")
  } else {
    options(jags.pb = "text")
  }
  
  if(!is.null(monitor(model))) {
    monitor(model) <- sort(unique(c(monitor(model),"deviance")))
  }
  
    
  data_analysis <- translate_data(select(model), data) 
  
  if (is.function(modify_data(model))) {
    if("analysis" %in% names(formals(modify_data(model)))) {
      data_analysis <- modify_data(model)(data_analysis, analysis = TRUE)
    } else {
      data_analysis <- modify_data(model)(data_analysis)
    }
  }
    
  if (is.function(gen_inits(model))) {
    inits <- list()
    for (i in 1:n.chain) {   
      inits[[i]] <- gen_inits(model)(data_analysis)
    }
  } else {
    inits <- NULL
  }
  
  n.adapt <- 100
  n.burnin <- as.integer(n.iter /2)
  n.thin <- max(1, floor(n.chain * n.burnin / n.sim))
  n.sim <- as.integer(n.iter /2)
  
  ptm <- proc.time()
    
  if (parallelChains) {
      
    doMC::registerDoMC(cores=n.chain)
    rngs<-parallel.seeds("base::BaseRNG", n.chain)
    
    if (!is.null (inits)) {
      for (i in 1:n.chain)
        inits[[i]] <- c(inits[[i]],rngs[[i]])
    } else {
      inits <- rngs
    }
    
      mcmc <- foreach::foreach(i = 1:n.chain, .combine = add_jags_jags_mcmc) %dopar% { 
        file <- tempfile(fileext=".bug")
        cat(model_code(model), file=file)
        
        jags_analysis_internal (
          data = data_analysis, file=file, monitor = monitor(model), 
          inits = inits[i], n.chain = 1, 
          n.adapt = n.adapt, n.burnin = n.burnin, n.sim = n.sim, n.thin = n.thin, 
          quiet = quiet
        )
      }
  } else {    
    file <- tempfile(fileext=".bug")
    cat(model_code(model), file=file)
        
    mcmc <- jags_analysis_internal (
      data = data_analysis, file=file, monitor = monitor(model), 
      inits = inits, n.chain = n.chain, 
      n.adapt = n.adapt, n.burnin = n.burnin, n.sim = n.sim, n.thin = n.thin, 
      quiet = quiet
    )
  }
  if(is.null(monitor(model))) {
    monitor(model) <- names(mcmc$mcmc)
    monitor(model) <- sort(monitor(model))
  }
  
  object <- list(
    model = model, 
    random_variables = random_variables,
    inits = inits, 
    mcmc = mcmc,
    iterations = n.iter,
    time = ((proc.time () - ptm)[3]) / (60 * 60)
    )
  class(object) <- c("jagr_analysis")
    
  while (!is_converged (object, rhat = convergence) && resample > 0) 
  {
    if(!quiet) {
      cat ("Resampling due to convergence failure")
      cat_convergence (object)  
    }
    
    resample <- resample - 1
        
    object <- update_jags(object, quiet = quiet)
  }
  
  if (is_converged (object, rhat = convergence)) {
    if (!quiet) {
      cat ('Analysis converged')
      cat_convergence (object)
    }
    return (object)
  }
  if (quiet) {
    message ("Analysis failed to converge")
  } else {
    cat ('Analysis failed to converge')
    cat_convergence (object)
  }    
  return (object)
}
