
jagr_analysis <- function (
  model, data, n.iter = 1000, n.chain = 3, resample = 3,
  convergence = 1.1, independence = 0,
  parallelChains = .Platform$OS.type != "windows", 
  quiet = FALSE, n.sim = 1000
)
{  
  stopifnot(is.jags_model(model))
  
  model <- model$models[[1]]
  
  stopifnot(n.iter >= 100)
  stopifnot(n.chain %in% 2:6)
  stopifnot(resample %in% 0:4)
  stopifnot(convergence >= 1.0 && convergence <= 2.0)
  stopifnot(independence %in% 0:100)
  stopifnot(n.sim >= 100 && n.sim <= 2000)
  
  cat_convergence <- function (object) {
    cat (' (Rhat:')
    cat (rhat(object$mcmc))
    cat (', ind:')
    cat (ind(object$mcmc))
    cat (')\n')
  }
  
  n.iter <- ceiling(max(n.iter, n.sim * 2 / n.chain))
    
  if (quiet) {
    options(jags.pb = "none")
  } else {
    options(jags.pb = "text")
  }

  if(!is.null(model$monitor)) {
    model$monitor <- sort(unique(c(model$monitor,"deviance")))
  }
    
  data_analysis <- translate_data(model$select, data) 
  
  if (is.function(model$modify_data)) {
    if("analysis" %in% names(formals(model$modify_data))) {
      data_analysis <- model$modify_data (data_analysis, analysis = TRUE)
    } else {
      data_analysis <- model$modify_data (data_analysis)
    }
  }
    
  if (is.function(model$gen_inits)) {
    inits <- list()
    for (i in 1:n.chain) {   
      inits[[i]] <- model$gen_inits(data_analysis)
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
        cat(model$model, file=file)
        
        jags_analysis_internal (
          data = data_analysis, file=file, monitor = model$monitor, 
          inits = inits[i], n.chain = 1, 
          n.adapt = n.adapt, n.burnin = n.burnin, n.sim = n.sim, n.thin = n.thin, 
          quiet = quiet
        )
      }
  } else {    
    file <- tempfile(fileext=".bug")
    cat(model$model, file=file)
        
    mcmc <- jags_analysis_internal (
      data = data_analysis, file=file, monitor = model$monitor, 
      inits = inits, n.chain = n.chain, 
      n.adapt = n.adapt, n.burnin = n.burnin, n.sim = n.sim, n.thin = n.thin, 
      quiet = quiet
    )
  }
  if(is.null(model$monitor)) {
    model$monitor <- names(mcmc$mcmc)
    model$monitor <- sort(model$monitor)
  }
  
  object <- list(
    model = model, 
    data = data, 
    inits = inits, 
    mcmc = mcmc,
    iterations = n.iter,
    time = ((proc.time () - ptm)[3]) / (60 * 60),
    convergence = convergence,
    independence = independence
    )
  class(object) <- c("jagr_analysis")
  
  is_converged_jagr_analysis <- function (object, ...)
  {     
    return (rhat(object$mcmc) <= object$convergence)
  }
        
  while (!is_converged_jagr_analysis (object) && resample > 0) 
  {
    if(!quiet) {
      cat ("Resampling due to convergence failure")
      cat_convergence (object)  
    }
    
    resample <- resample - 1
        
    object <- update_jags(object)
  }
  
  if (is_converged_jagr_analysis (object)) {
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
