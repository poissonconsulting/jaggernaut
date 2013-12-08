
#' @title Number of MCMC chains in a JAGS object
#'
#' @description 
#' Gets the number of MCMC chains in a JAGS object
#'   
#' @param object a JAGS object
#' @return an integer element indicating the number of MCMC chains in object
#' @aliases nchains
#' @export
nchains <- function (object) {
  UseMethod("nchains", object)
}

nchains.mcarray <- function (object) {
  dim <- dim (object)
  return (as.integer(dim[length(dim)]))
}

nchains.mcmc.list <- function (object) {
  return (nchain (object))
}

nchains.jagr_chains <- function (object) {
  return (nchains (samples(object)[[1]]))
}

nchains.jagr_power_analysis <- function (object) {
  return (nchains (as.jagr_chains(object)))
}

nchains_jagr_power_analysis <- function (object) {
  stopifnot(is.jagr_power_analysis(object))
  return (nchains (object))
}

#' @method nchains jags_analysis
#' @export
nchains.jags_analysis <- function (object) {
  if(is_one_model(object))
    return (nchains(analysis(object)))
  
  analyses <- analyses(object)
  analyses <- lapply(analyses, nchains_jagr_power_analysis)
  analyses <- name_object(analyses, "Model")
  return (analyses) 
}

nchains_jags_analysis <- function (object) {
  stopifnot(is.jags_analysis(object))
  return (nchains (object))
}

#' @method nchains jags_power_analysis
#' @export
nchains.jags_power_analysis <- function (object) {
  
  lapply_nchains_jagr_power_analysis <- function (object) {    
    return (lapply(object, nchains_jagr_power_analysis))
  }
  
  analyses <- analyses(object)
    
  nchains <- lapply(analyses, lapply_nchains_jagr_power_analysis)
    
  nchains <- matrixise(nchains)
  
  stopifnot(all(nchains == nchains[1,1]))
  
  return (nchains[1,1])
}
