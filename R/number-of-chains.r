
#' @title Number of MCMC chains in a JAGS object
#'
#' @description 
#' Gets the number of MCMC chains in a JAGS object
#'   
#' @param object a JAGS object
#' @return an integer element indicating the number of MCMC chains in object
#' @aliases nchains
#' @seealso \code{\link{number_of_chains.jags_analysis}} 
#' and \code{\link{number_of_chains.jags_power_analysis}}
#' @export
number_of_chains <- function (object, ...) {
  UseMethod("number_of_chains", object)
}

#' @export
nchains <- function (object, ...) {
  UseMethod("number_of_chains", object)
}

number_of_chains.mcarray <- function (object) {
  dim <- dim (object)
  return (as.integer(dim[length(dim)]))
}

number_of_chains.mcmc.list <- function (object) {
  return (coda::nchain (object))
}

number_of_chains.jags_mcmc <- function (object) {
  return (number_of_chains (object$mcmc[[1]]))
}

number_of_chains.jagr_analysis <- function (object) {
  return (number_of_chains (object$mcmc))
}

number_of_chains_jagr_analysis <- function (object, ...) {
  stopifnot(is.jagr_analysis(object))
  return (number_of_chains (object, ...))
}

#' @title Number of MCMC chains in a JAGS analysis
#'
#' @description 
#' Gets the number of MCMC chains in a JAGS analysis object.
#'   
#' @param object a jags_analysis object
#' @param ... other arguments passed to generic function.
#' @seealso \code{\link{number_of_chains}} and \code{\link{jags_analysis}} 
#' @method number_of_chains jags_analysis
#' @export
number_of_chains.jags_analysis <- function (object, ...) {
  
  nchains <- lapply(object$analyses, number_of_chains_jagr_analysis, ...)
  nchains <- delist(nchains)
  return (nchains)
}

number_of_chains_jags_analysis <- function (object, ...) {
  stopifnot(is.jags_analysis(object))
  return (number_of_chains (object, ...))
}


#' @title Number of MCMC chains in a JAGS power analysis
#'
#' @description 
#' Gets the number of MCMC chains in a JAGS power analysis object.
#'   
#' @param object a jags_power_analysis object
#' @param ... other arguments passed to generic function.
#' @seealso \code{\link{number_of_chains}} and \code{\link{jags_power_analysis}} 
#' @method number_of_chains jags_power_analysis
#' @export
number_of_chains.jags_power_analysis <- function (object, ...) {
  
  lapply_number_of_chains_jags_analysis <- function (object, ...) {    
    return (lapply(object, number_of_chains_jags_analysis, ...))
  }
  
  nchains <- lapply(object$analyses, lapply_number_of_chains_jags_analysis, ...)
  nchains <- delist(nchains)
  nchains <- arrayicise(nchains)
  rownames(nchains) <- paste0("Value",1:nrow(nchains))
  colnames(nchains) <- paste0("Replicate",1:ncol(nchains))
  return (nchains)
}
