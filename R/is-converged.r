
#' @export
is_converged <- function (object, ...)
  UseMethod("is_converged", object)

is_converged.jagr_chains <- function (object, rhat_threshold, ...) {
  stopifnot(is_scalar(rhat_threshold))
  rhat(object, parm = "all", combine = TRUE) <= rhat_threshold
}

is_converged.jagr_power_analysis <- function (object, rhat_threshold, ...)
  is_converged(as.jagr_chains(object), rhat_threshold = rhat_threshold, ...)

is_converged_jagr_power_analysis <- function (object, rhat_threshold, ...) {
  stopifnot(is.jagr_power_analysis(object))
  return (is_converged(object, rhat_threshold = rhat_threshold, ...))
}

#' @method is_converged jags_analysis
#' @export 
is_converged.jags_analysis <- function (object, ...) {
  return (rhat (object, ...) <= rhat_threshold(object))
}

#' @method is_converged jags_power_analysis
#' @export 
is_converged.jags_power_analysis <- function (object, percent = FALSE, ...) { 
  rhat <- rhat(object, ...)
  rhat <- rhat <= rhat_threshold(object)
  if(!percent) {
    return (rhat)
  }
  
  percent_fun <- function (x) {
    return (round(length(x[x]) / length(x) * 100))
  }
  
  rhat <- apply(rhat, 2, percent_fun)
  return (rhat)
}
