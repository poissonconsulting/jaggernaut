
#' @title Get model
#'
#' @description
#' Get the model component of a JAGS object.  
#' 
#' @param object a JAGS object.
#' @param ... further arguments passed to or from other methods.
#' @return The model component of a JAGS object.
#' @seealso \code{\link{jaggernaut}}  
#' @export
model <- function (object, ...) {
  UseMethod("model", object)
}

model.jags_model <- function (object, ...) {
  stopifnot(is_one_model(object))
  return (models(object)[[1]])
}
