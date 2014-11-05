#' @title Get default model id
#'
#' @description
#' Get the default_model_id component of a JAGS object.  
#' 
#' @param object a JAGS object.
#' @param ... further arguments passed to or from other methods.
#' @return The default_model_id component of a JAGS object.
#' @seealso \code{\link{jaggernaut}}  
#' @export
default_model_id <- function (object, ...) {
  UseMethod("default_model_id", object)
}

#' @title Set model name(s)
#'
#' @description
#' Set the default_model_id component of a JAGS object.  
#' 
#' @usage
#' default_model_id(object) <- value
#' @param object a JAGS object.
#' @param value a string or number defining the default model id to use in predictions etc
#' of the BUGS language.
#' @seealso \code{\link{jaggernaut}}  
#' @export
"default_model_id<-" <- function (object, value) {
  UseMethod("default_model_id<-", object)
}

default_model_id.jagr_model <- function (object, ...) {
  object$default_model_id  
}

#' @method default_model_id jags_analysis
#' @export
default_model_id.jags_analysis <- function (object, ...) {
  object$default_model_id
}

#' @method default_model_id<- jags_analysis
#' @export
"default_model_id<-.jags_analysis" <- function (object, value) {
  assert_that(is.string(value) && noNA(value))
  
  assert_that(value %in% model_id(object, reference = TRUE))

  object$default_model_id <- value
  object
}
