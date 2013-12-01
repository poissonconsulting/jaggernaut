
#' @title R-hat threshold
#'
#' @description
#' Gets the R-hat threshold of a JAGS object.  
#' 
#' @param object a JAGS object.
#' @param ... further arguments passed to or from other methods.
#' @return The R-hat threshold.
#' @seealso \code{\link{jaggernaut}}  
#' @export
rhat_threshold <- function (object, ...) {
  UseMethod("rhat_threshold", object)
}

"rhat_threshold<-" <- function (object, value) {
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

"rhat_threshold<-.jags_analysis" <- function (object, value) {
  stopifnot(is_informative_numeric_scalar(value))
  stopifnot(is_bounded(value, 1.0, 2.0))
  
  object$rhat_threshold <- value
  
  return (object)
}

"rhat_threshold<-.jags_power_analysis" <- function (object, value) {
  stopifnot(is_informative_numeric_scalar(value))
  stopifnot(is_bounded(value, 1.0, 2.0))
  
  object$rhat_threshold <- value
  
  return (object)
}
