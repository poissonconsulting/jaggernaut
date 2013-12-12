
#' @title Number of replicates in a JAGS object
#'
#' @description 
#' Gets the number of replicates in a JAGS object
#'   
#' @param object a JAGS object
#' @return an integer element indicating the number of replicates in object
#' @export
nreps <- function (object) {
  UseMethod("nreps", object)
}

#' @method nreps jags_simulation
#' @export
nreps.jags_simulation <- function (object) {
  return (as.integer(length(dataset(object)[[1]])))
}
