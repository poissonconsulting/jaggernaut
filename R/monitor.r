
monitor <- function (object, ...) {
  UseMethod("monitor", object)
}

"monitor<-" <- function (object, value, ...) {
  UseMethod("monitor<-", object)
}

monitor.jags_model <- function (object, ...) {
  x <- list()
  for (i in 1:length(object$models))
    x[[i]] <- object$models[[i]]$monitor
  x <- delist(x)
  if (length(x) == 0)
    return (NULL)
  return (x)
}

monitor.jagr_analysis <- function (object, ...) {
  return (monitor(as.jags_model(object, ...)))
}

monitor_jagr_analysis <- function (object, ...) {
  stopifnot(is.jagr_analysis(object))
  return (monitor(as.jags_model(object, ...)))
}

monitor.jags_analysis <- function (object, ...) {
  object <- as.jagr_analysis(object)
  if (is.jagr_analysis(object))
    return (monitor(object, ...))
  
  object <- lapply(object, monitor_jagr_analysis, ...)
  
  return (object)
}

"monitor<-.jags_model" <- function (object, value, ...) {

  if(is.null(value))
    return (object)
  
  if (!is.character(value)) {
    stop ("monitor must be NULL or class character")
  }
  if (!length(value)) {
    stop ("monitor must be NULL or define at least one parameter to monitor")
  } 
  if (any(duplicated(value))) {
    stop ("parameters to monitor must be unique")
  }
  
  for (i in 1:length(object$models))
    object$models[[i]]$monitor <- value
  return (object)
}
