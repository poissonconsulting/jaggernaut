
#' @export
is_converged <- function (object, ...) {
  UseMethod("is_converged", object)
}

#' @method rhat jags_mcmc
is_converged.jags_mcmc <- function (object, rhat, ...)
{ 
  return (rhat(object) <= rhat)
}

#' @method rhat jags_analysis
#' @export 
is_converged.jags_analysis <- function (object, model = NULL, rhat = NULL, ...)
{
  stop("not yet implemented")
  
  return (object)
}

#' @method rhat jags_power_analysis
#' @export 
is_converged.jags_power_analysis <- function (object, model = NULL, value = NULL, rep = NULL, rhat = NULL, ...)
{
  stop("not yet implemented")
  
  return (object)
}
