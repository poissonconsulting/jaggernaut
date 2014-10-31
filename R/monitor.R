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
#' @param value a character vector of the parameters to monitor or a 
#' Perl regular expression as a string of the parameters to monitor.
#'  
#' @details
#' #' Parameter names that have a '-' suffix are monitored
#' but not included in the calculation of convergence. 
#' This can be useful when, for example, a parameter is Bernoulli distributed.
#' 
#' Duplicates are removed and the parameters sorted. If a parameter
#' is specified with and without a '-' suffix then it is assumed 
#' that it is to be included in the calculation of convergence.
#' @seealso \code{\link{jaggernaut}}  
#' @export
"monitor<-" <- function (object, value) {
  UseMethod("monitor<-", object)
}

monitor.jagr_chains <- function (object, ...) {
  names(samples(object))
}

monitor.jagr_model <- function (object, trim_suffix = FALSE, drop_suffixed = FALSE, ...) {
  assert_that(is.flag(trim_suffix) && noNA(trim_suffix))
  assert_that(is.flag(drop_suffixed) && noNA(trim_suffix))
  
  monitor <- object$monitor
  if(trim_suffix)
    monitor <- sub("-$", "", monitor)
  if(drop_suffixed)
    monitor <- monitor[!grepl("-$", object$monitor)]
  monitor
}

monitor_jagr_model <- function (object, trim_suffix = FALSE, drop_suffixed = FALSE, ...) {
  stopifnot(is.jagr_model(object))
  monitor(object, trim_suffix = trim_suffix, drop_suffixed = drop_suffixed, ...)
}

#' @method monitor jags_model
#' @export
monitor.jags_model <- function (object, ...) {
  if(is_one_model(object))
    return (monitor(model(object), ...))
  
  lapply(models(object), monitor_jagr_model, ...)
}

#' @method monitor jags_analysis
#' @export
monitor.jags_analysis <- function (object, ...) {
  monitor(as.jags_model(object), ...)
}

"monitor<-.jagr_model" <- function (object, value) {
  
  assert_that(is.character(value) && not_empty(value))
  
  value <- sort(unique(value))
  
  sub <- sub("-$", "", object$monitor)
  sub <- unique(sub[duplicated(sub)])
  sub <- paste0(sub, "-")
  
  object$monitor <- value[!value %in% sub]
  object
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
  object
}
