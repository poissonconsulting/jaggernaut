
get_derived<- function (object, ...) {
  UseMethod("get_derived", object)
}

get_derived.jags_mcmc <- function (object, model, monitor) {

  stopifnot(is.character(model))
  stopifnot(length(model) == 1)
  stopifnot(is.character(monitor))
  stopifnot(length(monitor) > 0)
      
  model <- jags_model (model, monitor)
  options(jags.pb = "none")

  file <- tempfile(fileext=".bug")
  cat(model$model, file=file)

  nchain <- nchain (object)
  niter <- niter (object)
  
  list <- list ()
  for (j in 1:nchain) {
    list[[j]] <- get_samples (model,data = as.list(get_subset_jags_mcmc(object, iter = 1, chain = j)), file = file)
    if (niter > 1) {
      for (i in 2:niter) {
        samples <- get_samples (model,data = as.list(get_subset_jags_mcmc(object, iter = i, chain = j)), file = file)
        list[[j]] <- add_jags (list[[j]], samples, by = "iterations")
      }
    }
  }
  samples <- list [[1]]
  
  if (nchain > 1) {
    for (j in 2:nchain)
      samples <- add_jags (samples, list[[j]], by = "chains")
  }
  mcmc <- jags_mcmc(samples, jags = list(NULL))
  
  return (mcmc)
}

get_derived.jagr_analysis <- function (object, monitor, data, object_data) {
  
  stopifnot(is.character(monitor))
  stopifnot(length(monitor) > 0)
  stopifnot(is.data.frame(data) || is_data_list(data))

  dat <- translate_data(select(object), object_data, dat = data) 
  
  if (is.function(modify_data(object))) {
    if("analysis" %in% names(formals(modify_data(object)))) {
      dat <- modify_data(object)(dat, analysis = FALSE)
    } else {
      dat <- modify_data(object)(dat)
    }
  }

  model <- jags_model (derived_code(object), monitor = monitor)
  options(jags.pb = "none")
  file <- tempfile(fileext=".bug")
  cat(model_code(model), file=file)
  
  if (is.data.frame(data)) {
    object <- zero_random (object,object_data,data)
  } else if (!is.null(random_effects(object))) {
    message("zero random is only available when original data set is a data frame")
  }
  nchain <- nchain (object)
  niter <- niter (object)

  list <- list ()
  for (j in 1:nchain) {
    
    data <- c(dat,as.list(get_subset_jags_mcmc(object$mcmc, iter = 1, chain = j)))
    list[[j]] <- get_samples (model,data = c(dat,as.list(get_subset_jags_mcmc(object$mcmc, iter = 1, chain = j))),file = file)    
    if (niter > 1) {
      for (i in 2:niter) {
        samples <- get_samples (model,data = c(dat,as.list(get_subset_jags_mcmc(object$mcmc, iter = i, chain = j))), file = file)
        list[[j]] <- add_jags (list[[j]], samples, by = "iterations")
      }
    }
  }    
  samples <- list [[1]]
    
  if (nchain > 1) {
    for (j in 2:nchain)
      samples <- add_jags (samples, list[[j]], by = "chains")
  }
  
  mcmc <- jags_mcmc(samples, jags = list(NULL))
  
  return (mcmc)
}
