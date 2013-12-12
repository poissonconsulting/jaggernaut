
#' @title Get dataset(s) from a JAGS object
#'
#' @description
#' Gets the dataset(s) from a JAGS object.  
#' 
#' @param object a JAGS object.
#' @param ... further arguments passed to or from other methods.
#' @return a data.frame or list(s) of the data
#' @seealso \code{\link{dataset}}  
#' @export
data_jags <- function (object, ...) {
  warning("data_jags is deprecated - use dataset instead")
  UseMethod("dataset", object)
}
