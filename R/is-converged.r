
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

is_converged.default <- function (object, rhat_threshold, ...) {
  stopifnot(is_scalar(rhat_threshold))  
  return (rhat(object, parm = "all", combine = TRUE) <= rhat_threshold) 
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
#' @seealso \code{\link{rhat_threshold}} and \code{\link{jaggernaut}}  
#' @method is_converged jags_analysis
#' @export 
is_converged.jags_analysis <- function (object, ...) {
  return (rhat (object, parm = "all", combine = TRUE) <= rhat_threshold(object))
}

#' @title Test convergence
#'
#' @description
#' Tests the convergence of a JAGS power analysis.  
#' 
#' @param object a \code{jags_power_analysis} object.
#' @param combine a logical element indicating whether or not to calculate the 
#' percent convergence by \code{jags_power_analysis} values.
#' @param ... further arguments passed to or from other methods.
#' @return A logical matrix indicating or not each analysis has converged 
#' or a data.frame indicating the percent convergence rate.
#' @seealso \code{\link{rhat_threshold}}, \code{\link{jags_power_analysis}} 
#' and \code{\link{jaggernaut}}  
#' @method is_converged jags_power_analysis
#' @export 
is_converged.jags_power_analysis <- function (object, combine = TRUE, ...) { 
  rhat <- rhat(object, parm = "all", combine = TRUE)
  rhat <- rhat <= rhat_threshold(object)
  if(!combine) {
    return (rhat)
  }
  
  percent_fun <- function (x) {
    return (round(length(x[x]) / length(x) * 100))
  }
  
  rhat <- apply(rhat, 2, percent_fun)
  return (rhat)
}
