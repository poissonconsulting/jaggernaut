
#' @title Number of MCMC samples in a JAGS object
#'
#' @description 
#' Gets the number of MCMC samples in a JAGS object
#'   
#' @param object a JAGS object
#' @return an integer element indicating the number of MCMC samples in object
#' @export
nsamples <- function (object) {
  UseMethod("nsamples", object)
}

nsamples.mcarray <- function (object) {
  
  dim <- dim (object)
  niter <- as.integer(dim[length(dim)-1])
  
  return (nchains (object) * niter)
}

nsamples.mcmc.list <- function (object) {
  return (nchains (object) * niter(object))
}

nsamples.jagr_chains <- function (object) {
  return (nsamples(samples(object)[[1]]))
}

nsamples.jags_samples <- function (object) {
  return (ncol(object))
}

nsamples.jagr_analysis <- function (object) {
  return (nsamples(chains(object)))
}

nsamples_jagr_analysis <- function (object) {
  stopifnot(is.jagr_analysis(object))
  return (nsamples (object))
}

#' @method nsamples jags_analysis
#' @export
nsamples.jags_analysis <- function (object) {
  if(is_one_model(object))
    return (nsamples(analysis(object)))
  
  lapply(analyses(object), nsamples_jagr_analysis)
}

nsamples_jags_analysis <- function (object) {
  stopifnot(is.jags_analysis(object))
  return (nsamples(object))
}
