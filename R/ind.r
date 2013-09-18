
#' @title Get R-hat value(s)
#'
#' @description
#' Get R-hat value or values for JAGS objects
#' 
#' @param object a JAGS object
#' @param ... passed to and from other functions
#' @return a vector, matrix or array of ind values
#' @seealso \code{\link{ind.jags_analysis}} and \code{\link{ind.jags_power_analysis}}
#' @export
ind <- function (object, ...) {
  UseMethod("ind", object)
}

#' @method ind jags_mcmc
ind.jags_mcmc <- function (object, parm = "all", combine = TRUE, ...)
{ 
  stopifnot(is.character(parm) && is_length(parm) && is_defined(parm))
  stopifnot(is_indicator(combine))
  
  parm <- unique(parm)
  
  if("all" %in% parm) {
    ind <- object$ind
  } else {
    stopifnot(all(parm %in% object$svars))
    ind <- object$ind[object$svars %in% parm]
    vars <- object$vars[object$svars %in% parm]
  }
  
  if (combine)
    return (min(ind))
  
  ind <- data.frame(ind = ind, row.names = vars)
  return (ind)
}

#' @method ind jagr_analysis
ind.jagr_analysis <- function (object, parm = "all", combine = TRUE, ...)
{
  stopifnot(is.character(parm) && is_length(parm) && is_defined(parm))
  stopifnot(is_indicator(combine))
  
  parm <- expand_parm(object, parm = parm)
  
  return (ind(as.jags_mcmc(object), parm = parm, combine = combine,...))
}

ind_jagr_analysis <- function (object, ...) {
  stopifnot(is.jagr_analysis(object))
  return (ind(object, ...))
}

#' @title Get R-hat value(s)
#'
#' @description
#' Get R-hat value or values for JAGS analysis
#' 
#' @param object a JAGS analysis
#' @param character element
#' @param ... passed to and from other functions
#' @return a vector, matrix or array of ind values
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
#' ind(analysis)
#' 
#' @method ind jags_analysis
#' @export 
ind.jags_analysis <- function (object, parm = "all", combine = TRUE, ...)
{
  if(!is.character(parm))
    stop("parm must a character vector")
  
  if(!is_length(parm))
    stop("parm must be length one or more")
  
  if(!is_defined(parm))
    stop("parm must not contain missing values")
  
  if(!is_indicator(combine))
    stop("combine must be TRUE or FALSE")
  
  ind <- lapply(object$analyses, FUN = ind_jagr_analysis, parm = parm, combine = combine)
  
  ind <- delist (ind)
  return (ind)
}

#' @method ind jags_power_analysis
#' @export 
ind.jags_power_analysis <- function (object, model = NULL, value = NULL, rep = NULL, parm = NULL, ...)
{
  stop("not yet implemented")
  
  return (object)
}
