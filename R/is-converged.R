#' @title Test convergence
#'
#' @description
#' Tests the convergence of a JAGS object.  
#' 
#' @param object a JAGS object.
#' @param ... further arguments passed to or from other methods.
#' @return A logical element, vector or list indicating whether or not the object
#' has converged.
#' @seealso \code{\link{jaggernaut}}  
#' @export
is_converged <- function (object, ...) {
  UseMethod("is_converged", object)
}

is_converged.default <- function (object, convergence_threshold, ...) {
  stopifnot(is_scalar(convergence_threshold))  
  return (convergence(object, parm = "all", combine = TRUE) <= convergence_threshold) 
}

#' @title Test convergence
#'
#' @description
#' Tests the convergence of a JAGS analysis.  
#' 
#' @param object a \code{jags_analysis} object.
#' @param ... further arguments passed to or from other methods.
#' @return A logical element or vector whether or not analysis or analyses have
#' converged.
#' @seealso \code{\link{convergence_threshold}} and \code{\link{jaggernaut}}  
#' @method is_converged jags_analysis
#' @export 
is_converged.jags_analysis <- function (object, ...) {
  return (convergence (object, parm = "all", combine = TRUE) <= convergence_threshold(object))
}
