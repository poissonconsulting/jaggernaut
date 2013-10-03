
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

  nchains <- nchains (object)
  niters <- niters (object)
  
  list <- list ()
  for (j in 1:nchains) {
    list[[j]] <- get_samples (model,data = as.list(get_subset_jags_mcmc(object, iter = 1, chain = j)), file = file)
    if (niters > 1) {
      for (i in 2:niters) {
        samples <- get_samples (model,data = as.list(get_subset_jags_mcmc(object, iter = i, chain = j)), file = file)
        list[[j]] <- add_jags (list[[j]], samples, by = "iterations")
      }
    }
  }
  samples <- list [[1]]
  
  if (nchains > 1) {
    for (j in 2:nchains)
      samples <- add_jags (samples, list[[j]], by = "chains")
  }
  mcmc <- jags_mcmc(samples, jags = list(NULL))
  
  return (mcmc)
}

get_derived.jags_analysis <- function (object, monitor, data, object_data) {
  
  stopifnot(nmodels(object) == 1)
  stopifnot(is.character(monitor))
  stopifnot(length(monitor) > 0)
  stopifnot(is.data.frame(data) || is_data_list(data))
  
  derived_code <- derived_code(object)
  random_effects <- random_effects(object)
  
  object <- as.jagr_analysis(object)
  
  dat <- translate_data(select(object), object_data, dat = data) 
  
  if (is.function(modify_data(object))) {
    if("analysis" %in% names(formals(modify_data(object)))) {
      dat <- modify_data(object)(dat, analysis = FALSE)
    } else {
      dat <- modify_data(object)(dat)
    }
  }

  model <- jags_model (derived_code, monitor = monitor)
  options(jags.pb = "none")
  file <- tempfile(fileext=".bug")
  cat(model_code(model), file=file)
  
  if (is.data.frame(data)) {
    object <- zero_random (object,object_data,data,random = random_effects)
  } else if (!is.null(random_effects)) {
    message("zero random is only available when original data set is a data frame")
  }
  
  
  nchains <- nchains (object)
  niters <- niters (object)

  list <- list ()
  for (j in 1:nchains) {
    
    data <- c(dat,as.list(get_subset_jags_mcmc(object$mcmc, iter = 1, chain = j)))
    list[[j]] <- get_samples (model,data = c(dat,as.list(get_subset_jags_mcmc(object$mcmc, iter = 1, chain = j))),file = file)    
    if (niters > 1) {
      for (i in 2:niters) {
        samples <- get_samples (model,data = c(dat,as.list(get_subset_jags_mcmc(object$mcmc, iter = i, chain = j))), file = file)
        list[[j]] <- add_jags (list[[j]], samples, by = "iterations")
      }
    }
  }    
  samples <- list [[1]]
    
  if (nchains > 1) {
    for (j in 2:nchains)
      samples <- add_jags (samples, list[[j]], by = "chains")
  }
  
  mcmc <- jags_mcmc(samples, jags = list(NULL))
  
  return (mcmc)
}
