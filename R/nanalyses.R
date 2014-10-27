#' @title Number of analyses in a JAGS object
#'
#' @description 
#' Gets the number of analyses in a JAGS object
#'   
#' @param object a JAGS object
#' @param ... further arguments passed to or from other methods.
#' @return an integer element indicating the number of analyses in object
#' @export
nanalyses <- function (object, ...) {
  UseMethod("nmodels", object)
}

#' @method nanalyses jagr_analysis
#' @export
nanalyses.jagr_analysis <- function (object, ...) {
  as.integer(1)
}

#' @method nanalyses jags_analysis
#' @export
nanalyses.jags_analysis <- function (object, ...) {
  nmodels(object)
}
