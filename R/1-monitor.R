
#' @title Get monitor
#'
#' @description
#' Get the monitor component of a JAGS object.  
#' 
#' @param object a JAGS object.
#' @param ... further arguments passed to or from other methods.
#' @return The monitor component of a JAGS object.
#' @seealso \code{\link{jaggernaut}}  
#' @export
monitor <- function (object, ...) {
  UseMethod("monitor", object)
}

#' @title Set monitor
#'
#' @description
#' Set the monitor component of a JAGS object.  
#' 
#' @usage
#' monitor(object) <- value
#' @param object a JAGS object.
#' @param value a character vector of the parameters to monitor.
#' @seealso \code{\link{jaggernaut}}  
#' @export
"monitor<-" <- function (object, value) {
  UseMethod("monitor<-", object)
}

monitor.jagr_chains <- function (object, ...) {
  return (names(samples(object)))
}

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
    return (monitor(model(object), ...))
  
  models <- models(object)
  models <- lapply(models, monitor_jagr_model, ...)
  models <- name_object(models, "Model")
  return (models)   
}

monitor.jagr_analysis <- function (object, ...) {
  return (monitor(as.jagr_chains(object), ...))
}

#' @method monitor jags_analysis
#' @export
monitor.jags_analysis <- function (object, ...) {
  return (monitor(as.jags_model(object), ...))
}

"monitor<-.jagr_model" <- function (object, value) {
  
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
"monitor<-.jags_model" <- function (object, value) {
  
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

"monitor<-.jagr_analysis" <- function (object, value) {
  stop()
}
