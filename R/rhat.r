
#' @title Get R-hat value(s)
#'
#' @description
#' Get R-hat value(s) for JAGS objects
#' 
#' @param object a JAGS object
#' @param ... passed to and from other functions
#' @return a vector, matrix or array of rhat values
#' @export
rhat <- function (object, ...) {
  UseMethod("rhat", object)
}

#' @method rhat jagr_chains
rhat.jagr_chains <- function (object, parm = "all", combine = TRUE, ...)
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
    return (max(rhat, na.rm = TRUE))
  
  rhat <- data.frame(rhat = rhat, row.names = vars)
  return (rhat)
}

rhat.jagr_power_analysis <- function (object, parm = "all", combine = TRUE, ...) {
  stopifnot(is.character(parm) && is_length(parm) && is_defined(parm))
  stopifnot(is_indicator(combine))
  
  parm <- expand_parm(object, parm = parm)
  
  return (rhat(as.jagr_chains(object), parm = parm, combine = combine,...))
}

rhat_jagr_power_analysis <- function (object, ...) {
  stopifnot(is.jagr_power_analysis(object))
  return (rhat(object, ...))
}

rhat.jagr_power_analysis <- function (object, parm = "all", combine = TRUE, ...) {
  stopifnot(is.character(parm) && is_length(parm) && is_defined(parm))
  stopifnot(is_indicator(combine))
  
  parm <- expand_parm(object, parm = parm)

  return (rhat(as.jagr_chains(object), parm = parm, combine = combine,...))
}

rhat_jagr_power_analysis <- function (object, ...) {
  stopifnot(is.jagr_power_analysis(object))
  return (rhat(object, ...))
}

#' @method rhat jags_analysis
#' @export 
rhat.jags_analysis <- function (object, parm = "all", combine = TRUE, ...) {
  if(!is.character(parm))
    stop("parm must a character vector")

  if(!is_length(parm))
    stop("parm must be length one or more")
  
  if(!is_defined(parm))
    stop("parm must not contain missing values")
  
  if(!is_indicator(combine))
    stop("combine must be TRUE or FALSE")
  
  rhat <- lapply(object$analyses, FUN = rhat_jagr_power_analysis, parm = parm, combine = combine)
  
  rhat <- delist (rhat)
  return (rhat)
}

#' @method rhat jags_power_analysis
#' @export 
rhat.jags_power_analysis <- function (object, parm = "all", combine = TRUE, ...)
{
  lapply_rhat_jagr_power_analysis <- function (object, 
                                         parm = parm, 
                                         combine = combine, ...) {    
    return (lapply(object, rhat_jagr_power_analysis, 
                   parm = parm, combine = combine, ...))
  }
  
  analyses <- analyses(object)
  
  rhat <- lapply(analyses, lapply_rhat_jagr_power_analysis, parm = parm, combine = combine, ...)

  if(combine) {
    rhat <- arrayicise(rhat)
    rhat <- name_object(t(rhat),c("replicate","value"))
  } else
    rhat <- name_object(rhat,c("value","replicate"))
  return (rhat)
}
