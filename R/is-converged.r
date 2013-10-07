
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
  is_converged(object, rhat_threshold = rhat_threshold, ...)
}

#' @method rhat jags_analysis
#' @export 
is_converged.jags_analysis <- function (object, ...) {
  rhat_threshold <- rhat_threshold(object)
  
  if(is_one_model(object))
    return (is_converged(analysis(object), rhat_threshold = rhat_threshold, ...))
  
  analyses <- analyses(object)
  analyses <- sapply(analyses, is_converged_jagr_power_analysis,  
                     rhat_threshold = rhat_threshold, ...)
  analyses <- name_object(analyses, "Model")
  
  return (analyses)  
}

#' @method rhat jags_power_analysis
#' @export 
is_converged.jags_power_analysis <- function (object, ...)
{
  stop("not yet implemented")
  
  return (object)
}
