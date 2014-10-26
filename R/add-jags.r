#' @title Add JAGS objects
#'
#' @description
#' Adds two or more JAGS object of the same class.  
#' 
#' @param object a JAGS object.
#' @param ... additional JAGS objects to add to object.
#' @return a JAGS object of the original class
#' @export
add_jags <- function (object, ...) {
  warning("add_jags is deprecated use combine instead", call. = FALSE)
  UseMethod("combine", object)
}
