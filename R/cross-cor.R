#' @title Get cross-correlation values
#'
#' @description
#' Get cross-correlation value for JAGS objects
#' 
#' @param object a JAGS object
#' @param parm a character vector indicating the parameters for which to calculate
#' the cross_corr values. Either list the parmeters or use all, fixed or random.
#' @param ... passed to and from other functions
#' @return a vector, matrix or array of autocorr values
#' @seealso \code{coda::crosscorr}
#' @export
cross_corr <- function (object, parm = "all", ...) {
  UseMethod("cross_corr", object)
}  

cross_corr.jagr_chains <- function (object, parm, ...) {
  
  assert_that(is.character(parm) && noNA(parm) && not_empty(parm))  
    
  crosscorr <- crosscorr(as.mcmc.list(object))
    
  crosscorr <- crosscorr[rownames(crosscorr) %in% parm,
                         colnames(crosscorr) %in% parm,
                         drop = FALSE]
  
  crosscorr  
}

cross_corr.jagr_analysis <- function (object, parm, ...) {
  
  parm <- unique(parm)
  
  parm <- expand_parm(object, parm)
  
  cross_corr(as.jagr_chains(object), parm = parm, ...)
}

cross_corr_jagr_analysis <- function (object, parm, ...) {
  stopifnot(is.jagr_analysis(object))
  cross_corr(object, parm = parm, ...)
}

#' @method cross_corr jags_analysis
#' @export 
cross_corr.jags_analysis <- function (object, parm = "all", ...) {
  
  if(is_one_model(object)) {
    return (cross_corr(analysis(object), parm = parm, ...))
  }
  lapply(analyses(object), cross_corr_jagr_analysis, parm = parm, ...)  
}

#' @method cross_corr jags_sample
#' @export 
cross_corr.jags_sample <- function (object, parm = "all", ...) {
  
  assert_that(is.string(parm) && noNA(parm))  
  
  if(any(c("fixed", "random") %in% parm)) {
    warning("fixed or random not defined for jags_sample - replacing with all")
    parm <- "all"
  }
  
  cross_corr(as.jagr_chains(object), parm = parm, ...)  
}
