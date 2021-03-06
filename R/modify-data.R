
#' @title Get modify data
#'
#' @description
#' Get the modify_data component of a JAGS object.  
#' 
#' @param object a JAGS object.
#' @param ... further arguments passed to or from other methods.
#' @return The modify_data component of a JAGS object.
#' @seealso \code{\link{jaggernaut}}  
#' @export
modify_data <- function (object, ...) {
  UseMethod("modify_data", object)
}

#' @title Set modify data
#'
#' @description
#' Set the modify_data component of a JAGS object.  
#' 
#' @usage
#' modify_data(object) <- value
#' @param object a JAGS object.
#' @param value a function or NULL to replace the modify data fnction.
#' @seealso \code{\link{jaggernaut}}  
#' @export
"modify_data<-" <- function (object, value) {
  UseMethod("modify_data<-", object)
}

modify_data.jagr_model <- function (object, ...) {
  return (object$modify_data)
}

modify_data_jagr_model <- function (object, ...) {
  stopifnot(is.jagr_model(object))
  return (modify_data(object, ...))
}

#' @method modify_data jags_model
#' @export
modify_data.jags_model <- function (object, ...) {
  
  if(is_one_model(object))
    return (modify_data(model(object), ...))
  
  lapply(models(object), modify_data_jagr_model, ...)
}

modify_data.jagr_analysis <- function (object, ...) {
  return (modify_data(as.jagr_model(object), ...))
}

modify_data_jagr_analysis <- function (object, ...) {
  stopifnot(is.jagr_analysis(object))
  return (modify_data(object, ...))
}

#' @method modify_data jags_analysis
#' @export
modify_data.jags_analysis <- function (object, ...) {
  return (modify_data(as.jags_model(object), ...))
}

"modify_data<-.jagr_model" <- function (object, value) {
  
  if(!is.null(value)) {
    if (!is.function(value)) {
      stop ("modify_data must be NULL or a function")
    }
    args <- names(formals(value))
    if (!identical(args,c("data"))) {
      stop ("modify_data argument(s) must be named data")
    }
  }
  
  object$modify_data <- value
  
  return (object)
}

#' @method modify_data<- jags_model
#' @export
"modify_data<-.jags_model" <- function (object, value) {
  
  if(is.list(value) && length(value) != nmodels(object))
    stop("if value is a list it must be the same length as the number of models in object")
  
  if(is.list(value))
    names(value) <- NULL
  
  models <- models(object)
  
  for (i in 1:length(models)) {
    if(!is.list(value)) {
      modify_data(models[[i]]) <- value
    } else
      modify_data(models[[i]]) <- value[[i]]
  }
  models(object) <- models
  object
}
