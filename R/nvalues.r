
#' @title Number of simulation values in a JAGS object
#'
#' @description 
#' Gets the number of simulation values in a JAGS object
#'   
#' @param object a JAGS object
#' @return an integer element indicating the number of simulation values in object
#' @export
nvalues <- function (object) {
  UseMethod("nvalues", object)
}
 
#' @method nvalues jags_simulation
#' @export
nvalues.jags_simulation <- function (object) {
  return (as.integer(nrow(object$values)))
}
