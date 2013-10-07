
#' @export
monitor <- function (object, ...) {
  UseMethod("monitor", object)
}

#' @export
"monitor<-" <- function (object, value, ...) {
  UseMethod("monitor<-", object)
}

monitor.jagr_chains <- function (object, ...) {
  return (names(object$mcmc))
}

#' @method monitor jagr_model
#' @export
monitor.jagr_model <- function (object, ...) {
  return (object$monitor)
}

monitor_jagr_model <- function (object, ...) {
  stopifnot(is.jagr_model(object))
  return (monitor(object, ...))
}

#' @method monitor jags_model
#' @export
monitor.jags_model <- function (object, ...) {
  if(is_one_model(object))
    return (monitor(model(object),...))
  
  models <- models(object)
  models <- lapply(models, monitor_jagr_model, ...)
  models <- name_object(models, "Model")
  return (models)   
}

monitor.jagr_power_analysis <- function (object, ...) {
  return (monitor(as.jagr_chains(object, ...)))
}

#' @method monitor jags_analysis
#' @export
monitor.jags_analysis <- function (object, ...) {
  return (monitor(as.jags_model(object), ...))
}

#' @method monitor<- jagr_model
#' @export
"monitor<-.jagr_model" <- function (object, value, ...) {
  
  if(!is.null(value))
    value <- sort(unique(value))
  
  if (!is.null(value)) {
    if (!is.character(value)) {
      stop ("monitor must be NULL or class character")
    }
    if (!length(value)) {
      stop ("monitor must be NULL or define at least one parameter to monitor")
    } 
  }
  
  object$monitor <- value
  
  return (object)
}

#' @method monitor<- jags_model
#' @export
"monitor<-.jags_model" <- function (object, value, ...) {
  
  if(is.list(value) && length(value) != nmodels(object))
    stop("if value is a list it must be the same length as the number of models in object")
  
  if(is.list(value))
    names(value) <- NULL
  
  models <- models(object)
  
  for (i in 1:length(models)) {
    if(!is.list(value)) {
      monitor(models[[i]]) <- value
    } else
      monitor(models[[i]]) <- value[[i]]
  }
  
  models(object) <- models
  return (object)
}

#' @method monitor<- jagr_analysis
#' @export
"monitor<-.jagr_analysis" <- function (object, value, ...)
  stop("cannot replace monitor in a jagr_analysis object")
