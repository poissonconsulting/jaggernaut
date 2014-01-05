jagr_analysis <- function (model, data, niters, nworkers) {    
  
  assert_that(is.jagr_analysis_model(model))
  assert_that(is_convertible_data(data))
  assert_that(is.count(niters) && noNA(niters))
  assert_that(is.count(nworkers) && noNA(nworkers))
  
  resample <- opts_jagr("nresample")
  nchains <- opts_jagr("nchains")
  quiet <- opts_jagr("quiet")
  rhat_threshold <- opts_jagr("rhat")
  
  if(!is.null(monitor(model)))
    monitor(model) <- sort(unique(c(monitor(model),"deviance")))
  
  data <- translate_data(select(model), data) 
  
  if (is.function(modify_data(model))) 
    data <- modify_data(model)(data)
    
  if (is.function(gen_inits(model))) {
    inits <- list()
    for (i in 1:nchains) {   
      inits[[i]] <- gen_inits(model)(data)
    }
  } else
    inits <- NULL
  
  analysis <- jagr_power_analysis(model_code = model_code(model), 
                                  data = data, 
                                  niters = niters,
                                  inits = inits,
                                  nworkers = nworkers,
                                  monitor = monitor(model),
                                  random = names(random_effects(model)))
    
  monitor(model) <- monitor(analysis) 
  
  object <- c(model,analysis)
  
  class(object) <- c("jagr_analysis","jagr_analysis_model",
                     "jagr_model","jagr_power_analysis")
    
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
