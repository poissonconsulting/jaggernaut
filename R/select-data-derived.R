#' @title Get select derived
#'
#' @description
#' Get the select_data_derived component of a JAGS object.  
#' 
#' @param object a JAGS object.
#' @param ... further arguments passed to or from other methods.
#' @return The select_data_derived component of a JAGS object.
#' @seealso \code{\link{jaggernaut}}  
#' @export
select_data_derived <- function (object, ...) {
  UseMethod("select_data_derived", object)
}

#' @title Set modify data derived
#'
#' @description
#' Set the select_data_derived component of a JAGS object.  
#' 
#' @usage
#' select_data_derived(object) <- value
#' @param object a JAGS object.
#' @param value a function or NULL to replace the modify data derived fnction.
#' @seealso \code{\link{jaggernaut}}  
#' @export
"select_data_derived<-" <- function (object, value) {
  UseMethod("select_data_derived<-", object)
}

select_data_derived.jagr_model <- function (object, ...) {
  return (object$select_data_derived)
}

select_data_derived_jagr_model <- function (object, ...) {
  stopifnot(is.jagr_model(object))
  return (select_data_derived(object, ...))
}

#' @method select_data_derived jags_model
#' @export
select_data_derived.jags_model <- function (object, ...) {
  
  if(is_one_model(object))
    return (select_data_derived(model(object), ...))
  
  models <- models(object)
  models <- lapply(models, select_data_derived_jagr_model, ...)
  models <- name_object(models, "Model")
  return (models) 
}

#' @method select_data_derived jags_analysis
#' @export
select_data_derived.jags_analysis <- function (object, ...) {
  return (select_data_derived(as.jags_model(object), ...))
}  

"select_data_derived<-.jagr_model" <- function (object, value) {
  
  if(!is.null(value)) {
    if (!is.character(value)) {
      stop ("value must be NULL or a character vector")
    }
  }
  
  object$select_data_derived <- value
  
  return (object)
}

#' @method select_data_derived<- jags_model
#' @export
"select_data_derived<-.jags_model" <- function (object, value) {
  
  if(is.list(value) && length(value) != nmodels(object))
    stop("if value is a list it must be the same length as the number of models in object")
  
  if(is.list(value))
    names(value) <- NULL
  
  models <- models(object)
  
  for (i in 1:length(models)) {
    if(!is.list(value)) {
      select_data_derived(models[[i]]) <- value
    } else
      select_data_derived(models[[i]]) <- value[[i]]
  }
  
  models(object) <- models
  return (object)
}

#' @method select_data_derived<- jags_analysis
#' @export
"select_data_derived<-.jags_analysis" <- function (object, value) {
  
  if (!is.null (value)) {
    if(!is.character(value)) {
      stop("select_data_derived must be NULL or a character")
    }
  }
  
  if(is.null(value))
    is.na(value) <- TRUE
  
  for (i in 1:nmodels(object))
    select_data_derived(object$analyses[[i]]) <- value
  
  return (object)
}
