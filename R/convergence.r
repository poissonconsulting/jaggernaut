
#' @title Get convergence value(s)
#'
#' @description
#' Get convergence values for JAGS objects
#' 
#' @param object a JAGS object
#' @param ... passed to and from other functions
#' @return a vector, matrix or array of convergence values
#' @seealso \code{\link{convergence.jags_analysis}} and \code{\link{convergence.jags_power_analysis}}
#' @export
convergence <- function (object, ...) {
  UseMethod("convergence", object)
}

#' @method convergence jags_mcmc
convergence.jags_mcmc <- function (object, ...) { 
  convergence <- rhat(object, parm = "all", combine = TRUE)
  names(convergence) <- "rhat"
  return (convergence)
}

#' @method convergence jagr_analysis
convergence.jagr_analysis <- function (object, ...) {
  return (convergence(as.jags_mcmc(object, ...)))
}

convergence_jagr_analysis <- function (object, ...) {
  stopifnot(is.jagr_analysis(object))
  return (convergence(object, ...))
}

#' @title Get R-hat value(s)
#'
#' @description
#' Get R-hat value or values for JAGS analysis
#' 
#' @param object a JAGS analysis
#' @param character element
#' @param ... passed to and from other functions
#' @return a vector, matrix or array of convergence values
#' 
#' model <- jags_model("
#' model { 
#'  bLambda ~ dlnorm(0,10^-2) 
#'  for (i in 1:nrow) { 
#'    x[i]~dpois(bLambda) 
#'  } 
#'}")
#'
#' data <- data.frame(x = rpois(100,1))
#' 
#' analysis <- jags_analysis (model, data, mode = "demo")
#' convergence(analysis)
#' 
#' @method convergence jags_analysis
#' @export 
convergence.jags_analysis <- function (object, ...)
{ 
  convergence <- lapply(object$analyses, FUN = convergence_jagr_analysis, ...)
  
  convergence <- delist (convergence)
  return (convergence)
}

#' @method convergence jags_power_analysis
#' @export 
convergence.jags_power_analysis <- function (object, model = NULL, value = NULL, rep = NULL, parm = NULL, ...)
{
  stop("not yet implemented")
  
  return (object)
}
