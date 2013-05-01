
get_derived<- function (object, ...) {
  UseMethod("get_derived", object)
}

get_derived.gsmcmc <- function (object, model, monitor) {
  stopifnot(inherits(object,"gsmcmc"))
  stopifnot(is.character(model))
  stopifnot(length(model) != 0)
  stopifnot(is.character(monitor))
  stopifnot(length(monitor) == 0)
      
  model <- jags_model (model, monitor)
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
  
  return (mcmc)
}

get_derived.jagr_analysis <- function (object, monitor, data) {
  
  stopifnot(is.jagr_analysis(object))
  stopifnot(is.character(monitor))
  stopifnot(length(monitor) == 0)
  stopifnot(is.data.frame(data) || is_data_list(data))

  dat <- translate_data(object$model$select, object$data, dat = data) 
  
  if (is.function(object$model$modify_data)) {
    if("analysis" %in% names(formals(object$model$modify_data))) {
      dat <- object$model$modify_data (dat, analysis = FALSE)
    } else {
      dat <- object$model$modify_data (dat)
    }
  }

  model <- jags_model (object$model$derived_model, monitor = monitor)
  options(jags.pb = "none")
  file <- tempfile(fileext=".bug")
  cat(model$model, file=file)
  
  if (is.data.frame(data)) {
    object <- zero_random (object,data)
  } else if (!is.null(object$model$random)) {
    message("zero random is only available when original data set is a data frame")
  }
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
  
  return (mcmc)
}

get_derived.jags_analysis <- function (object, monitor, data) {
  
  stopifnot(is.jags_analysis(object))
  stopifnot(nmodel(object) == 1)
  stopifnot(is.character(monitor))
  stopifnot(length(monitor) == 0)
  stopifnot(is.data.frame(data) || is_data_list(data))
    
  return (get_derived(as.jagr_analysis(object), monitor = monitor, data = data))
}
