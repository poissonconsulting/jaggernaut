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

#' @title Get number of tries
#'
#' @description
#' Get the ntries of a JAGS object.  
#' 
#' @param object a JAGS object.
#' @param combine a logical element indicating whether or not to calculate the 
#' maximum number of tries convergence by \code{jags_power_analysis} values.
#' @param ... further arguments passed to or from other methods.
#' @return A matrix of the ntries component of a JAGS object.
#' @seealso \code{\link{jaggernaut}}  
#' @method ntries jags_simulation
#' @export
ntries.jags_simulation <- function (object, combine = FALSE, ...) {
  
  lapply_ntries_default <- function (object, ...) {    
    return (lapply(object, ntries_default, ...))
  }
  
  data <- dataset(object)
  
  ntries <- lapply(data, lapply_ntries_default, ...)
  
  ntries <- matrixise(ntries)
  ntries <- name_object(t(ntries),c("replicate","value"))
  
  if(!combine)
    return (ntries)
  return (apply(ntries, MARGIN = 2, FUN = mean))
}

"ntries<-.default" <- function (object, value) {
  stopifnot(is_integer_scalar(value))
  stopifnot(is_bounded(value, 1, 10))
  
  value <- as.integer(value)
  
  attr(object, "ntries") <- value
  
  return (object)
}
