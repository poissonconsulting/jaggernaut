#' @title Number of models in a JAGS object
#'
#' @description 
#' Gets the number of models in a JAGS object
#'   
#' @param object a JAGS object
#' @param ... further arguments passed to or from other methods.
#' @return an integer element indicating the number of models in object
#' @aliases nmodel
#' @export
nmodels <- function (object, ...) {
  UseMethod("nmodels", object)
}

#' @method nmodels jagr_model
#' @export
nmodels.jagr_model <- function (object, ...) {
  as.integer(1)
}

#' @method nmodels jags_model
#' @export
nmodels.jags_model <- function (object, ...) {
  as.integer(length(models(object)))
}
 
#' @method nmodels jags_analysis
#' @export
nmodels.jags_analysis <- function (object, ...) {
  as.integer(length(analyses(object)))
}
