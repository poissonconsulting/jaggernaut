#' @title Test parallel
#'
#' @description
#' Tests JAGS object has parallel chains.  
#' 
#' @param object a JAGS object.
#' @return A flag indicating whether or not the object
#' has parallel chains.
#' @seealso \code{\link{jaggernaut}}  
#' @export
is_parallel <- function (object) {
  UseMethod("is_parallel", object)
}

is_parallel.jagr_chains <- function (object) {
  length(jags(object)) > 1
}

is_parallel.jagr_analysis <- function (object) {  
  is_parallel(chains(object))
}

#' @method is_parallel jags_analysis
#' @export 
is_parallel.jags_analysis <- function (object) {
  is_parallel(analyses(object)[[1]])
}
