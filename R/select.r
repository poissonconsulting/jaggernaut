
#' @title Get select
#'
#' @description
#' Get the select component of a JAGS object.  
#' 
#' @param object a JAGS object.
#' @param ... further arguments passed to or from other methods.
#' @return The select component of a JAGS object.
#' @seealso \code{\link{jaggernaut}}  
#' @export
select <- function (object, ...) {
  UseMethod("select", object)
}

#' @title Set select
#'
#' @description
#' Set the select component of a JAGS object.  
#' 
#' @usage
#' select(object) <- value
#' @param object a JAGS object.
#' @param value a character vector of the variables to select or NULL.
#' @seealso \code{\link{jaggernaut}}  
#' @export
"select<-" <- function (object, value) {
  UseMethod("select<-", object)
}

select.jagr_model <- function (object, ...) {
  return (object$select)
}

select_jagr_model <- function (object, ...) {
  stopifnot(is.jagr_model(object))
  return (select(object, ...))
}

#' @method select jags_model
#' @export
select.jags_model <- function (object, ...) {
  
  if(is_one_model(object))
    return (select(model(object, ...)))
  
  models <- models(object)
  models <- lapply(models, select_jagr_model, ...)
  models <- name_object(models, "Model")
  return (models)   
}

select.jagr_analysis <- function (object, ...) {
  return (select(as.jagr_model(object), ...))
}

select_jagr_analysis <- function (object, ...) {
  stopifnot(is.jagr_analysis(object))
  return (select(object, ...))
}

#' @method select jags_analysis
#' @export
select.jags_analysis <- function (object, ...) {
  return (select(as.jags_model(object), ...))
}

"select<-.jagr_model" <- function (object, value) {
  
  if (!is.null(value)) {
    if (!is.character(value)) {
      stop ("select must be NULL or class character")
    }
    if (!length(value)) {
      stop ("select must be NULL or define at least one variable to include")
    }
    names <- names_select(value)
    if (any(duplicated(names))) {
      stop ("variables to select must be unique")
    }
  }
  
  object$select <- value
  
  return (object)
}

#' @method select<- jags_model
#' @export
"select<-.jags_model" <- function (object, value) {
  
  if(is.list(value) && length(value) != nmodels(object))
    stop("if value is a list it must be the same length as the number of models in object")
  
  if(is.list(value))
    names(value) <- NULL
  
  models <- models(object)
  
  for (i in 1:length(models)) {
    if(!is.list(value)) {
      select(models[[i]]) <- value
    } else
      select(models[[i]]) <- value[[i]]
  }
  
  models(object) <- models
  return (object)
}

"select<-.jagr_analysis" <- function (object, value) {
  stop()
}
