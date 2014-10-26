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
convergence_threshold <- function (object, ...) {
  UseMethod("convergence_threshold", object)
}

"convergence_threshold<-" <- function (object, value) {
  UseMethod("convergence_threshold<-", object)
}

#' @method convergence_threshold jags_analysis
#' @export
convergence_threshold.jags_analysis <- function (object, ...) {
  return (object$convergence_threshold)
}

"convergence_threshold<-.jags_analysis" <- function (object, value) {
  stopifnot(is_numeric_scalar(value))
  stopifnot(is_bounded(value, 1.0, 2.0))
  
  object$convergence_threshold <- value
  
  return (object)
}
