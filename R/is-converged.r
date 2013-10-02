
#' @export
is_converged <- function (object, ...) {
  UseMethod("is_converged", object)
}

#' @method rhat jags_mcmc
is_converged.jags_mcmc <- function (object, rhat, ...)
{ 
  stopifnot(is.numeric(rhat))
  stopifnot(is_scalar(rhat))
  stopifnot(is_defined(rhat))
  
  if(rhat < 1.0 || rhat > 2.0)
    stop("rhat must be between 1.0 and 2.0")
  
  return (rhat(object) <= rhat)
}

#' @method rhat jagr_analysis
is_converged.jagr_analysis <- function (object, rhat, ...)
{ 
  
  return (is_converged(as.jags_mcmc(object), rhat = rhat, ...))
}

#' @method rhat jags_analysis
#' @export 
is_converged.jags_analysis <- function (object, ...)
{
  return (sapply(object$analyses, is_converged, rhat = object$rhat))
}

#' @method rhat jags_power_analysis
#' @export 
is_converged.jags_power_analysis <- function (object, ...)
{
  stop("not yet implemented")
  
  return (object)
}
