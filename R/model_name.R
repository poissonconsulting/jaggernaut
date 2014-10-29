#' @title Get model name
#'
#' @description
#' Get the model_name component of a JAGS object.  
#' 
#' @param object a JAGS object.
#' @param reference flag indicating whether to return the assigned unique reference
#' names or the original names (the default)
#' @param ... further arguments passed to or from other methods.
#' @return The model_name component of a JAGS object.
#' @seealso \code{\link{jaggernaut}}  
#' @export
model_name <- function (object, reference = FALSE, ...) {
  UseMethod("model_name", object)
}

#' @title Set model name(s)
#'
#' @description
#' Set the model_name component of a JAGS object.  
#' 
#' @usage
#' model_name(object) <- value
#' @param object a JAGS object.
#' @param value a string defining the model name in the JAGS dialect
#' of the BUGS language.
#' @seealso \code{\link{jaggernaut}}  
#' @export
"model_name<-" <- function (object, value) {
  UseMethod("model_name<-", object)
}

model_name.jagr_model <- function (object, ...) {
  object$model_name  
}

model_name_jagr_model <- function (object, ...) {
  stopifnot(is.jagr_model(object))
  model_name(object, ...)
}

#' @method model_name jags_model
#' @export
model_name.jags_model <- function (object, reference = FALSE, ...) {
  
  assert_that(is.flag(reference) && noNA(reference))
  
  if(reference)
    return (names(models(object)))
  
  if(is_one_model(object))
    return (model_name(model(object), ...))
  
  lapply(models(object), model_name_jagr_model, ...)
}

#' @method model_name jags_analysis
#' @export
model_name.jags_analysis <- function (object, reference = FALSE, ...) {
  model_name(as.jags_model(object), reference = reference, ...)
}

"model_name<-.jagr_model" <- function (object, value) {
  
  assert_that(is.string(value) || (!is.null(value) && is.na(value)))
  
  if(grepl("^Model\\d", value, perl = TRUE)) {
    stop("`Model` immediately followed by 1-9 is a reserved model name")
  }
  
  object$model_name <- value
  
  object
}

#' @method model_name<- jags_model
#' @export
"model_name<-.jags_model" <- function (object, value) {
  
  if(is.list(value) && length(value) != nmodels(object))
    stop("if value is a list it must be the same length as the number of models in object")
  
  if(is.list(value))
    names(value) <- NULL
  
  models <- models(object)
  
  for (i in 1:length(models)) {
    if(!is.list(value)) {
      model_name(models[[i]]) <- value
    } else
      model_name(models[[i]]) <- value[[i]]
  }
  
  models(object) <- models
  rename_models(object)
}
