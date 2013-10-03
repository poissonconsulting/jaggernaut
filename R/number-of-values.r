
#' @title Number of simulation values in a JAGS object
#'
#' @description 
#' Gets the number of simulation values in a JAGS object
#'   
#' @param object a JAGS object
#' @return an integer element indicating the number of simulation values in object
#' @aliases nvalues
#' @seealso \code{\link{number_of_values.jags_simulation}} and \code{\link{number_of_values.jags_power}}
#' @export
number_of_values <- function (object, ...) {
  UseMethod("number_of_values", object)
}

#' @export
nvalues <- function (object, ...) {
  UseMethod("number_of_values", object)
}

#' @title Number of values in a JAGS simulation
#'
#' @description 
#' Gets the number of values in a JAGS simulation object
#'   
#' @param object a jags_simulation object
#' @param ... other arguments passed to generic function.
#' @seealso \code{\link{number_of_values}} and \code{\link{jags_simulation}} 
#' @method number_of_values jags_simulation
#' @export
number_of_values.jags_simulation <- function (object, ...) {
  return (as.integer(nrow(object$values)))
}

#' @title Number of values in a JAGS power analysis
#'
#' @description 
#' Gets the number of values in a JAGS power analysis object
#'   
#' @param object a jags_power_analysis object
#' @param ... other arguments passed to generic function.
#' @seealso \code{\link{number_of_values}} 
#' and \code{\link{jags_power_analysis}} 
#' @method number_of_values jags_power_analysis
#' @export
number_of_values.jags_power_analysis <- function (object, ...) {
  return (as.integer(nrow(object$values)))
}
