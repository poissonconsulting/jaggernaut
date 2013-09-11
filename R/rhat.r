
#' @title Get R-hat value(s)
#'
#' @description
#' Get R-hat value or values for JAGS objects
#' 
#' @param object a JAGS object
#' @param ... passed to and from other functions
#' @return a vector, matrix or array of rhat values
#' @seealso \code{\link{rhat.jags_analysis}} and \code{\link{rhat.jags_power_analysis}}
#' @export
rhat <- function (object, ...) {
  UseMethod("rhat", object)
}

#' @method rhat jags_mcmc
rhat.jags_mcmc <- function (object, parm = NULL, ...)
{
  if (!is.null(parm))
    stop("not yet implemented")
  
  return (min(object$rhat[,1]))
}

#' @method rhat jags_analysis
#' @export 
rhat.jags_analysis <- function (object, model = NULL, parm = NULL, ...)
{
  stop("not yet implemented")
  
  return (object)
}

#' @method rhat jags_power_analysis
#' @export 
rhat.jags_power_analysis <- function (object, model = NULL, value = NULL, rep = NULL, parm = NULL, ...)
{
  stop("not yet implemented")
  
  return (object)
}
