
#' @title Number of MCMC simulations in a JAGS object
#'
#' @description 
#' Gets the number of MCMC simulations in a JAGS object
#'   
#' @param object a JAGS object
#' @return an integer element indicating the number of MCMC simulations in object
#' @aliases nsim
#' @seealso \code{\link{number_of_simulations.jags_analysis}} 
#' and \code{\link{number_of_simulations.jags_power_analysis}}
#' @export
number_of_simulations <- function (object, ...) {
  UseMethod("number_of_simulations", object)
}

#' @export
nsim<- function (object, ...) {
  UseMethod("number_of_simulations", object)
}

number_of_simulations.mcarray <- function (object) {
  return (nchains (object) * niters(object))
}

number_of_simulations.mcmc.list <- function (object) {
  return (nchains (object) * niters(object))
}

number_of_simulations.jags_mcmc <- function (object) {
  return (number_of_simulations (object$mcmc[[1]]))
}

number_of_simulations.jagr_analysis <- function (object) {
  return (number_of_simulations (object$mcmc))
}

number_of_simulations_jagr_analysis <- function (object, ...) {
  stopifnot(is.jagr_analysis(object))
  return (number_of_simulations (object, ...))
}

#' @title Number of MCMC simulations in a JAGS analysis
#'
#' @description 
#' Gets the number of MCMC simulations in a JAGS analysis object.
#'   
#' @param object a jags_analysis object
#' @param ... other arguments passed to generic function.
#' @seealso \code{\link{number_of_simulations}} and \code{\link{jags_analysis}} 
#' @method number_of_simulations jags_analysis
#' @export
number_of_simulations.jags_analysis <- function (object, ...) {
  
  nsims <- lapply(object$analyses, number_of_simulations_jagr_analysis, ...)
  nsims <- delist(nsims)
  return (nsims)
}

number_of_simulations_jags_analysis <- function (object, ...) {
  stopifnot(is.jags_analysis(object))
  return (number_of_simulations (object, ...))
}

#' @title Number of MCMC simulations in a JAGS power analysis
#'
#' @description 
#' Gets the number of MCMC simulations in a JAGS power analysis object.
#'   
#' @param object a jags_power_analysis object
#' @param ... other arguments passed to generic function.
#' @seealso \code{\link{number_of_simulations}} and \code{\link{jags_power_analysis}} 
#' @method number_of_simulations jags_power_analysis
#' @export
number_of_simulations.jags_power_analysis <- function (object, ...) {
  
  lapply_number_of_simulations_jags_analysis <- function (object, ...) {    
    return (lapply(object, number_of_simulations_jags_analysis, ...))
  }
  
  nsim <- lapply(object$analyses, lapply_number_of_simulations_analysis, ...)
  nsim <- delist(nsim)
  nsim <- arrayicise(nsim)
  rownames(nsim) <- paste0("Value",1:nrow(nsim))
  colnames(nsim) <- paste0("Replicate",1:ncol(nsim))
  return (nsim)
}
