
calc_derived<- function (object, ...) {
  UseMethod("calc_derived", object)
}

calc_derived.gsmcmc <- function (object, model, monitor, calc_estimates = FALSE) {
  if (!inherits(object,"gsmcmc"))
    stop ("object should be class gsmcmc")
  if (length(model) != 1 || !inherits(model,"character")) 
    stop ("model must be a character vector of length 1")
  if (!inherits(monitor,"character") || length(monitor) < 1) 
    stop ("monitor must be a character vector of length 1 or more")
      
  model <- jmodel (model, monitor)
  options(jags.pb = "none")

  file <- tempfile(fileext=".bug")
  cat(model$model, file=file)

  nchain <- nchain (object)
  niter <- niter (object)
  
  list <- list ()
  for (j in 1:nchain) {
    list[[j]] <- get_samples (model,data = as.list(get_subset_gsmcmc(object, iter = 1, chain = j)), file = file)
    if (niter > 1) {
      for (i in 2:niter) {
        samples <- get_samples (model,data = as.list(get_subset_gsmcmc(object, iter = i, chain = j)), file = file)
        list[[j]] <- add_iterations (list[[j]], samples)
      }
    }
  }
  samples <- list [[1]]
  
  if (nchain > 1) {
    for (j in 2:nchain)
      samples <- add_chains (samples, list[[j]])
  }
  mcmc <- gsmcmc(samples, jags = list(NULL))
  
  if(calc_estimates)
    return (calc_estimates (mcmc))
  
  return (mcmc)
}

calc_derived.jagr_analysis <- function (object, model, monitor, 
  data = NULL, calc_estimates = FALSE) {
  if (!is.jagr_analysis(object))
    stop ("object should be class jagr_analysis")
  if (length(model) != 1 || !inherits(model,"character")) 
    stop ("model must be a character vector of length 1")
  if (!inherits(monitor,"character") || length(monitor) < 1) 
    stop ("monitor must be a character vector of length 1 or more")
  if (!is.null(data) & !is.character(data) & !is.data.frame(data))
    stop ("data must be a data frame, a character or NULL")
    
  if (is.null(data)) {
    data <- as.data.frame (object)
  } else if (is.character (data)) {
    data <- generate_data (object, range = data)
  } 
  dat <- translate_data(object$model,object$data, dat = data)  
  model <- jmodel (model= model, monitor = monitor)
  options(jags.pb = "none")
  file <- tempfile(fileext=".bug")
  cat(model$model, file=file)
  
  object <- zero_random (object,data)
  
  nchain <- nchain (object)
  niter <- niter (object)

  list <- list ()
  for (j in 1:nchain) {
    
    data <- c(dat,as.list(get_subset_gsmcmc(object$mcmc, iter = 1, chain = j)))
    list[[j]] <- get_samples (model,data = c(dat,as.list(get_subset_gsmcmc(object$mcmc, iter = 1, chain = j))),file = file)    
    if (niter > 1) {
      for (i in 2:niter) {
        samples <- get_samples (model,data = c(dat,as.list(get_subset_gsmcmc(object$mcmc, iter = i, chain = j))), file = file)
        list[[j]] <- add_iterations (list[[j]], samples)
      }
    }
  }    
  samples <- list [[1]]
  
  if (nchain > 1) {
    for (j in 2:nchain)
      samples <- add_chains (samples, list[[j]])
  }
  mcmc <- gsmcmc(samples, jags = list(NULL))
  
  if(calc_estimates) {
    return (calc_estimates (mcmc))
  }
  return (mcmc)
}

calc_derived.janalysis <- function (object, model = NULL, monitor, 
                                    data = NULL, calc_estimates = FALSE) {
  
  if (!is.janalysis(object))
    stop ("analyses should be class janalysis")
  
  analysis <- top_model(object)
  if(is.null(model))
    model <- analysis$model$derived
  
  return (calc_derived(top_model(object), model = model, monitor = monitor, 
                        data = data, calc_estimates = calc_estimates))
}

