
#' @title Get R-hat value(s)
#'
#' @description
#' Get R-hat value(s) for JAGS objects
#' 
#' @param object a JAGS object
#' @param parm a character vector indicating the parameters for which to calculate
#' the R-hat values
#' @param combine a logical element indicating whether or not to summarise by 
#' the maximum R-hat value.
#' @param ... passed to and from other functions
#' @return a vector, matrix or array of rhat values
#' @export
rhat <- function (object, parm, combine, ...) {
  UseMethod("rhat", object)
}

rhat.jagr_chains <- function (object, parm = "all", combine = TRUE, ...) {
  
  stopifnot(is.character(parm) && is_length(parm) && is_defined(parm))
  stopifnot(is_indicator(combine))
  
  parm <- unique(parm)
  
  mcmc <- as.mcmc.list (object)
  
  vars <- coda::varnames(mcmc)
  
  vars <- sort(vars)
  
  if (is.null(object$rhat)) {
    if(nchains(object) > 1) {
      rhat <- numeric()
      for (i in seq(along = vars)) {
        rhat[i] <- round(coda::gelman.diag(mcmc[,vars[i]])$psrf[1],2)
      }
    } else {
      rhat <- rep(NA,length(vars))
    }
    rhat <- data.frame(rhat = rhat, row.names = vars)
    rhat$rhat <- rhat
  } else
    rhat <- object$rhat
  
  parm <- expand_parm(object, parm)
  
  rhat <- rhat[row.names(rhat) %in% parm,,drop = FALSE]
    
  if (combine)
    return (max(rhat$rhat, na.rm = TRUE))
  
  return (rhat)
}

rhat.jagr_power_analysis <- function (object, parm = "all", combine = TRUE, ...) {
  return (rhat(as.jagr_chains(object), parm = parm, combine = combine, ...))
}

rhat_jagr_power_analysis <- function (object, parm = "all", combine = TRUE, ...) {
  stopifnot(is.jagr_power_analysis(object))
  return (rhat(object, parm = parm, combine = combine, ...))
}

rhat_jagr_analysis <- function (object, parm = "all", combine = TRUE, ...) {
  stopifnot(is.jagr_analysis(object))
  return (rhat(object, parm = parm, combine = combine, ...))
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
  
  if(is_one_model(object))
    return (rhat(analysis(object), 
                 parm = parm, 
                 combine = combine, ...))
  
  analyses <- analyses(object)
  analyses <- lapply(analyses, 
                     rhat_jagr_analysis, 
                     parm = parm, 
                     combine = combine, ...)  
  analyses <- name_object(analyses, "model")
  return (analyses) 
}

#' @method rhat jags_power_analysis
#' @export 
rhat.jags_power_analysis <- function (object, parm = "all", combine = TRUE, ...) {

  lapply_rhat_jagr_power_analysis <- function (object, parm, combine) {    
    return (lapply(object, rhat_jagr_power_analysis, 
                   parm = parm, 
                   combine = combine, ...))
  }
    
  analyses <- analyses(object)
    
  rhat <- lapply(analyses, lapply_rhat_jagr_power_analysis, 
                 parm = parm, 
                 combine = combine, ...)

  if(combine) {
    rhat <- matrixise(rhat)
    rhat <- name_object(t(rhat),c("replicate","value"))
  } else
    rhat <- name_object(rhat,c("value","replicate"))
  return (rhat)
}
