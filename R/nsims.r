
#' @title Number of MCMC simulations in a JAGS object
#'
#' @description 
#' Gets the number of MCMC simulations in a JAGS object
#'   
#' @param object a JAGS object
#' @return an integer element indicating the number of MCMC simulations in object
#' @export
nsims <- function (object) {
  UseMethod("nsims", object)
}

nsims.mcarray <- function (object) {
  
  dim <- dim (object)
  niter <- as.integer(dim[length(dim)-1])
  
  return (nchains (object) * niter)
}

nsims.mcmc.list <- function (object) {
  return (nchains (object) * coda::niter(object))
}

nsims.jagr_chains <- function (object) {
  return (nsims(samples(object)[[1]]))
}

nsims.jagr_power_analysis <- function (object) {
  return (nsims(chains(object)))
}

nsims_jagr_power_analysis <- function (object) {
  stopifnot(is.jagr_power_analysis(object))
  return (nsims (object))
}

#' @method nsims jags_analysis
#' @export
nsims.jags_analysis <- function (object) {
  if(is_one_model(object))
    return (nsims(analysis(object)))
  
  analyses <- analyses(object)
  analyses <- lapply(analyses, nsims_jagr_power_analysis)
  analyses <- name_object(analyses, "Model")
  return (analyses)  
}

nsims_jags_analysis <- function (object) {
  stopifnot(is.jags_analysis(object))
  return (nsims(object))
}

#' @method nsims jags_power_analysis
#' @export
nsims.jags_power_analysis <- function (object) {
  
  lapply_nsims_jagr_power_analysis <- function (object) {    
    return (lapply(object, nsims_jagr_power_analysis))
  }
  
  analyses <- analyses(object)
  
  nsims <- lapply(analyses, lapply_nsims_jagr_power_analysis)
  
  nsims <- matrixise(nsims)
  nsims <- name_object(t(nsims),c("replicate","value"))
  return (nsims)
}
