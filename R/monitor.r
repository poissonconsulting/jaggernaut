
monitor <- function (object, ...) {
  UseMethod("monitor", object)
}

"monitor<-" <- function (object, value, ...) {
  UseMethod("monitor<-", object)
}

monitor.jagr_model <- function (object, ...) {
  return (object$monitor)
}

monitor_jagr_model <- function (object, ...) {
  stopifnot(is.jagr_model(object))
  return (monitor(object, ...))
}

monitor.jags_model <- function (object, ...) {
  
  object <- as.jagr_model(object)
  
  if(is.jagr_model(object))
    return (monitor(object, ...))
  
  object <- lapply(object, monitor_jagr_model, ...)
  
  object <- delist(object)
  if (length(object) == 0)
    return (NULL)
  return (object)  
}

monitor.jags_data_model <- function (object, ...) {
  return (monitor(as.jags_model(object), ...))
}

monitor.jagr_analysis <- function (object, ...) {
  return (monitor(as.jagr_model(object, ...)))
}

monitor_jagr_analysis <- function (object, ...) {
  stopifnot(is.jagr_analysis(object))
  return (monitor(object, ...))
}

monitor.jags_analysis <- function (object, ...) {
  return (monitor(as.jags_model(object), ...))
}

"monitor<-.jagr_model" <- function (object, value, ...) {
  
  if (!is.null(value)) {
    if (!is.character(value)) {
      stop ("monitor must be NULL or class character")
    }
    if (!length(value)) {
      stop ("monitor must be NULL or define at least one parameter to monitor")
    } 
    if (any(duplicated(value))) {
      stop ("parameters to monitor must be unique")
    }
  }
  
  object$monitor <- value
  
  return (object)
}

"monitor<-.jags_model" <- function (object, value, ...) {

  for (i in 1:nmodels(object))
    monitor(object$models[[i]], ...) <- value
  return (object)
}

"monitor<-.jags_data_model" <- function (object, value, ...) {
  
  for (i in 1:nmodels(object))
    monitor(object$models[[i]], ...) <- value
  return (object)
}
