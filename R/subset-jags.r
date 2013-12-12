
#' @title Subset a JAGS object
#'
#' @description
#' Subsets a JAGS object.  
#' 
#' @param object a JAGS object.
#' @param ... other arguments.
#' @return an object of the same JAGS class.
#' @seealso \code{\link{jaggernaut}}
#' @export
subset_jags <- function (object, ...) {
  warning("subset_jags is deprecated - use subset instead", call. = FALSE)
  UseMethod("subset", object)
}
