
#' @title Number of models in a JAGS object
#'
#' @description 
#' Gets the number of models in a JAGS object
#'   
#' @param object a JAGS object
#' @return an integer element indicating the number of models in object
#' @aliases nmodel
#' @export
nmodels <- function (object) {
  UseMethod("nmodels", object)
}

#' @method nmodels jagr_model
#' @export
nmodels.jagr_model <- function (object) {
  return (as.integer(1))
}

#' @method nmodels jags_model
#' @export
nmodels.jags_model <- function (object) {
  return (as.integer(length(models(object))))
}
 
#' @method nmodels jags_analysis
#' @export
nmodels.jags_analysis <- function (object) {
  return (as.integer(length(object$analyses)))
}
