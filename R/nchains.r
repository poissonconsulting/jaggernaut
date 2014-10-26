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

nchains.jagr_analysis <- function (object) {
  return (nchains (as.jagr_chains(object)))
}

nchains_jagr_analysis <- function (object) {
  stopifnot(is.jagr_analysis(object))
  return (nchains (object))
}

#' @method nchains jags_analysis
#' @export
nchains.jags_analysis <- function (object) {
  if(is_one_model(object))
    return (nchains(analysis(object)))
  
  analyses <- analyses(object)
  nchains <- sapply(analyses, nchains_jagr_analysis)
  stopifnot(all(nchains == nchains[1]))
  return (nchains[1]) 
}

nchains_jags_analysis <- function (object) {
  stopifnot(is.jags_analysis(object))
  return (nchains (object))
}
