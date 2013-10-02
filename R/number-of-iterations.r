
#' @title Number of MCMC iterations in a JAGS object
#'
#' @description 
#' Gets the number of MCMC iterations in a JAGS object
#'   
#' @param object a JAGS object
#' @return an integer element indicating the number of MCMC iterations in object
#' @aliases niter
#' @seealso \code{\link{number_of_iteration.jags_analysis}} 
#' and \code{\link{number_of_iterations.jags_power_analysis}}
#' @export
number_of_iterations <- function (object, ...) {
  UseMethod("number_of_iterations", object)
}

#' @export
niter <- function (object, ...) {
  UseMethod("number_of_iterations", object)
}

number_of_iterations.mcarray <- function (object, ...) {
  dim <- dim (object)
  return (as.integer(dim[length(dim)-1]))
}

number_of_iterations.mcmc.list <- function (object, ...) {
  return (coda::niter (object))
}

number_of_iterations.jags_mcmc <- function (object, ...) {
  return (number_of_iterations (object$mcmc[[1]], ...))
}

number_of_iterations.jagr_analysis <- function (object, ...) {
  return (number_of_iterations (object$mcmc, ...))
}

number_of_iterations_jagr_analysis <- function (object, ...) {
  stopifnot(is.jagr_analysis(object))
  return (number_of_iterations (object, ...))
}

#' @title Number of iterations in a JAGS analysis
#'
#' @description 
#' Gets the number of iterations in a JAGS analysis object.
#'   
#' @param object a jags_analysis object
#' @param ... other arguments passed to generic function.
#' @seealso \code{\link{number_of_iterations}} and \code{\link{jags_analysis}} 
#' @method number_of_iterations jags_analysis
#' @export
number_of_iterations.jags_analysis <- function (object, ...) {
  
  niter <- lapply(object$analyses, number_of_iterations_jagr_analysis, ...)
  niter <- delist(niter)
  return (niter)
}

number_of_iterations_jags_analysis <- function (object, ...) {
  stopifnot(is.jags_analysis(object))
  return (number_of_iterations (object, ...))
}

#' @title Number of iterations in a JAGS power analysis
#'
#' @description 
#' Gets the number of iterations in a JAGS power analysis object.
#'   
#' @param object a jags_power_analysis object
#' @param ... other arguments passed to generic function.
#' @seealso \code{\link{number_of_iterations}} and \code{\link{jags_power_analysis}} 
#' @method number_of_iterations jags_power_analysis
#' @export
number_of_iterations.jags_power_analysis <- function (object, ...) {
  
  lapply_number_of_iterations_jags_analysis <- function (object, ...) {    
    return (lapply(object, number_of_iterations_jags_analysis, ...))
  }
  
  niter <- lapply(object$analyses, lapply_number_of_iterations_jags_analysis, ...)
  niter <- delist(niter)
  niter <- arrayicise(niter)
  rownames(niter) <- paste0("Value",1:nrow(niter))
  colnames(niter) <- paste0("Replicate",1:ncol(niter))
  return (niter)
}
