
#' @export
is_converged <- function (object, ...) {
  UseMethod("is_converged", object)
}

is_converged.default <- function (object, rhat_threshold, ...) {
  stopifnot(is_scalar(rhat_threshold))  
  return (rhat(object, parm = "all", combine = TRUE) <= rhat_threshold) 
}

#' @method is_converged jags_analysis
#' @export 
is_converged.jags_analysis <- function (object, ...) {
  return (rhat (object, parm = "all", combine = TRUE) <= rhat_threshold(object))
}

#' @method is_converged jags_power_analysis
#' @export 
is_converged.jags_power_analysis <- function (object, combine = FALSE, ...) { 
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
