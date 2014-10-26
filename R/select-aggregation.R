#' @title Get select derived
#'
#' @description
#' Get the select_aggregation component of a JAGS object.  
#' 
#' @param object a JAGS object.
#' @param ... further arguments passed to or from other methods.
#' @return The select_aggregation component of a JAGS object.
#' @seealso \code{\link{jaggernaut}}  
#' @export
select_aggregation <- function (object, ...) {
  UseMethod("select_aggregation", object)
}

#' @title Set modify data derived
#'
#' @description
#' Set the select_aggregation component of a JAGS object.  
#' 
#' @usage
#' select_aggregation(object) <- value
#' @param object a JAGS object.
#' @param value a function or NULL to replace the modify data derived fnction.
#' @seealso \code{\link{jaggernaut}}  
#' @export
"select_aggregation<-" <- function (object, value) {
  UseMethod("select_aggregation<-", object)
}

select_aggregation.jagr_model <- function (object, ...) {
  return (object$select_aggregation)
}

select_aggregation_jagr_model <- function (object, ...) {
  stopifnot(is.jagr_model(object))
  return (select_aggregation(object, ...))
}

#' @method select_aggregation jags_model
#' @export
select_aggregation.jags_model <- function (object, ...) {
  
  if(is_one_model(object))
    return (select_aggregation(model(object), ...))
  
  models <- models(object)
  models <- lapply(models, select_aggregation_jagr_model, ...)
  models <- name_object(models, "Model")
  return (models) 
}

#' @method select_aggregation jags_analysis
#' @export
select_aggregation.jags_analysis <- function (object, ...) {
  return (select_aggregation(as.jags_model(object), ...))
}  

"select_aggregation<-.jagr_model" <- function (object, value) {
  
  if(!is.null(value)) {
    if (!is.character(value)) {
      stop ("value must be NULL or a character vector")
    }
  }
  
  object$select_aggregation <- value
  
  return (object)
}

#' @method select_aggregation<- jags_model
#' @export
"select_aggregation<-.jags_model" <- function (object, value) {
  
  if(is.list(value) && length(value) != nmodels(object))
    stop("if value is a list it must be the same length as the number of models in object")
  
  if(is.list(value))
    names(value) <- NULL
  
  models <- models(object)
  
  for (i in 1:length(models)) {
    if(!is.list(value)) {
      select_aggregation(models[[i]]) <- value
    } else
      select_aggregation(models[[i]]) <- value[[i]]
  }
  
  models(object) <- models
  return (object)
}

#' @method select_aggregation<- jags_analysis
#' @export
"select_aggregation<-.jags_analysis" <- function (object, value) {
  
  if (!is.null (value)) {
    if(!is.character(value)) {
      stop("select_aggregation must be NULL or a character")
    }
  }
  
  if(is.null(value))
    is.na(value) <- TRUE
  
  for (i in 1:nmodels(object))
    select_aggregation(object$analyses[[i]]) <- value
  
  return (object)
}
