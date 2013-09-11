
#' @export
ind <- function (object, ...) {
  UseMethod("ind", object)
}

#' @method rhat jags_mcmc
ind.jags_mcmc <- function (object, parm = NULL, ...)
{
  if (!is.null(parm))
    stop("not yet implemented")
  
  return (min(object$ind[,1]))
}

#' @method rhat jags_analysis
#' @export 
ind.jags_analysis <- function (object, model = NULL, parm = NULL, ...)
{
  stop("not yet implemented")
  
  return (object)
}

#' @method rhat jags_power_analysis
#' @export 
ind.jags_power_analysis <- function (object, model = NULL, value = NULL, rep = NULL, parm = NULL, ...)
{
  stop("not yet implemented")
  
  return (object)
}
