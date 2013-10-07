
#' @export
rhat_threshold <- function (object, ...) {
  UseMethod("rhat_threshold", object)
}

"rhat_threshold<-" <- function (object, ...) {
  UseMethod("rhat_threshold<-", object)
}

#' @method rhat_threshold jags_analysis
#' @export
rhat_threshold.jags_analysis <- function (object, ...) {
  return (object$rhat_threshold)
}

#' @method rhat_threshold jags_power_analysis
#' @export
rhat_threshold.jags_power_analysis <- function (object, ...) {
  return (object$rhat_threshold)
}

"rhat_threshold<-.jags_analysis" <- function (object, value, ...) {
  stopifnot(is.numeric(value) && is_scalar(value) && is_no_missing(value))
  stopifnot(value >= 1.0 && value <= 2.0)
  
  object$rhat_threshold <- value
  
  return (object)
}

"rhat_threshold<-.jags_power_analysis" <- function (object, value, ...) {
  stopifnot(is.numeric(value) && is_scalar(value) && is_no_missing(value))
  stopifnot(value >= 1.0 && value <= 2.0)
  
  object$rhat_threshold <- value
  
  return (object)
}
