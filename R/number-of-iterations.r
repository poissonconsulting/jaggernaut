
#' @title Number of MCMC iterations in a JAGS object
#'
#' @description 
#' Gets the number of MCMC iterations in a JAGS object
#'   
#' @param object a JAGS object
#' @return an integer element indicating the number of MCMC iterations in object
#' @aliases niters
#' @seealso \code{\link{number_of_iteration.jags_analysis}} 
#' and \code{\link{number_of_iterations.jags_power_analysis}}
#' @export
number_of_iterations <- function (object, ...) {
  UseMethod("number_of_iterations", object)
}

#' @export
niters <- function (object, ...) {
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
  
  niters <- lapply(object$analyses, number_of_iterations_jagr_analysis, ...)
  niters <- delist(niters)
  return (niters)
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
  
  niters <- lapply(object$analyses, lapply_number_of_iterations_jags_analysis, ...)
  niters <- delist(niters)
  niters <- arrayicise(niters)
  rownames(niters) <- paste0("Value",1:nrow(niters))
  colnames(niters) <- paste0("Replicate",1:ncol(niters))
  return (niters)
}
