
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
rhat.jags_mcmc <- function (object, parm = "all", combine = TRUE, ...)
{ 
  stopifnot(is.character(parm) && is_length(parm) && is_defined(parm))
  stopifnot(is_indicator(combine))
  
  parm <- unique(parm)
  
  if("all" %in% parm) {
    rhat <- object$rhat
  } else {
    stopifnot(all(parm %in% object$svars))
    rhat <- object$rhat[object$svars %in% parm]
    vars <- object$vars[object$svars %in% parm]
  }
  
  if (combine)
    return (max(rhat))
  
  rhat <- data.frame(rhat = rhat, row.names = vars)
  return (rhat)
}

#' @method rhat jagr_analysis
rhat.jagr_analysis <- function (object, parm = "all", combine = TRUE, ...)
{
  stopifnot(is.character(parm) && is_length(parm) && is_defined(parm))
  stopifnot(is_indicator(combine))
  
  parm <- expand_parm(object, parm = parm)
  
  return (rhat(as.jags_mcmc(object), parm = parm, combine = combine,...))
}

rhat_jagr_analysis <- function (object, ...) {
  stopifnot(is.jagr_analysis(object))
  return (rhat(object, ...))
}

#' @title Get R-hat value(s)
#'
#' @description
#' Get R-hat value or values for JAGS analysis
#' 
#' @param object a JAGS analysis
#' @param character element
#' @param ... passed to and from other functions
#' @return a vector, matrix or array of rhat values
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
#' rhat(analysis)
#' 
#' @method rhat jags_analysis
#' @export 
rhat.jags_analysis <- function (object, parm = "all", combine = TRUE, ...)
{
  if(!is.character(parm))
    stop("parm must a character vector")

  if(!is_length(parm))
    stop("parm must be length one or more")
  
  if(!is_defined(parm))
    stop("parm must not contain missing values")
  
  if(!is_indicator(combine))
    stop("combine must be TRUE or FALSE")
  
  rhat <- lapply(object$analyses, FUN = rhat_jagr_analysis, parm = parm, combine = combine)
  
  rhat <- delist (rhat)
  return (rhat)
}

#' @method rhat jags_power_analysis
#' @export 
rhat.jags_power_analysis <- function (object, model = NULL, value = NULL, rep = NULL, parm = NULL, ...)
{
  stop("not yet implemented")
  
  return (object)
}
