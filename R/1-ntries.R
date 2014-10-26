#' @title Get number of tries
#'
#' @description
#' Get the ntries of a JAGS object.  
#' 
#' @param object a JAGS object.
#' @param ... further arguments passed to or from other methods.
#' @return The ntries component of a JAGS object.
#' @seealso \code{\link{jaggernaut}}  
#' @export
ntries <- function (object, ...) {
  UseMethod("ntries", object)
}

"ntries<-" <- function (object, value) {
  UseMethod("ntries<-", object)
}
 
ntries.default <- function (object, ...) {
  return (attr(object, "ntries"))
}

ntries_default <- function (object, ...) {
  return (ntries(object, ...))
}
