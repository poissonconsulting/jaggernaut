
#' @title Number of replicates in a JAGS object
#'
#' @description 
#' Gets the number of replicates in a JAGS object
#'   
#' @param object a JAGS object
#' @return an integer element indicating the number of replicates in object
#' @aliases nrep
#' @seealso \code{\link{number_of_replicates.jags_simulation}} and \code{\link{number_of_replicates.jags_power}}
#' @export
number_of_replicates <- function (object, ...) {
  UseMethod("number_of_replicates", object)
}

#' @export
nrep <- function (object, ...) {
  UseMethod("number_of_replicates", object)
}

#' @title Number of replicates in a JAGS simulation
#'
#' @description 
#' Gets the number of replicates in a JAGS simulation object
#'   
#' @param object a jags_simulation object
#' @param ... other arguments passed to generic function.
#' @seealso \code{\link{number_of_replicates}} and \code{\link{jags_simulation}} 
#' @method number_of_replicates jags_simulation
#' @export
number_of_replicates.jags_simulation <- function (object, ...) {
  return (as.integer(length(object$data[[1]])))
}

#' @title Number of replicates in a JAGS power analysis
#'
#' @description 
#' Gets the number of replicates in a JAGS power analysis object
#'   
#' @param object a jags_power_analysis object
#' @param ... other arguments passed to generic function.
#' @seealso \code{\link{number_of_replicates}} 
#' and \code{\link{jags_power_analysis}} 
#' @method number_of_replicates jags_power_analysis
#' @export
number_of_replicates.jags_power_analysis <- function (object, ...) {
  return (as.integer(length(object$analyses[[1]])))
}
