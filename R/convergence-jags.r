
#' @export
convergence_jags <- function (object, ...) {
  UseMethod("convergence_jags", object)
}

#' @method convergence_jags jags_analysis
#' @export 
convergence_jags.jags_analysis <- function (object, ...)
{
  if(!is.jags_analysis(object))
    stop("object should be of class jags_analysis")
  
  
  
  return (object)
}

#' @method convergence_jags jags_analysis
#' @export 
convergence_jags.jags_power <- function (object, ...)
{
  if(!is.jags_analysis(object))
    stop("object should be of class jags_analysis")
  
  
  
  return (object)
}
