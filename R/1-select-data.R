#' @title Get select data
#'
#' @description
#' Get the select_data component of a JAGS object.  
#' 
#' @param object a JAGS object.
#' @param ... further arguments passed to or from other methods.
#' @return The select_data component of a JAGS object.
#' @seealso \code{\link{jaggernaut}}  
#' @export
select_data <- function (object, ...) {
  UseMethod("select_data", object)
}

#' @title Set select_data
#'
#' @description
#' Set the select_data component of a JAGS object.  
#' 
#' @usage
#' select_data(object) <- value
#' @param object a JAGS object.
#' @param value a character vector of the variables to select or NULL.
#' @seealso \code{\link{jaggernaut}}  
#' @export
"select_data<-" <- function (object, value) {
  UseMethod("select_data<-", object)
}

select_data.jagr_model <- function (object, ...) {
  return (object$select_data)
}

select_data_jagr_model <- function (object, ...) {
  stopifnot(is.jagr_model(object))
  return (select_data(object, ...))
}

#' @method select_data jags_model
#' @export
select_data.jags_model <- function (object, ...) {
  
  if(is_one_model(object))
    return (select_data(model(object, ...)))
  
  models <- models(object)
  models <- lapply(models, select_data_jagr_model, ...)
  models <- name_object(models, "Model")
  return (models)   
}

select_data.jagr_analysis <- function (object, ...) {
  return (select_data(as.jagr_model(object), ...))
}

select_data_jagr_analysis <- function (object, ...) {
  stopifnot(is.jagr_analysis(object))
  return (select_data(object, ...))
}

#' @method select_data jags_analysis
#' @export
select_data.jags_analysis <- function (object, ...) {
  return (select_data(as.jags_model(object), ...))
}

"select_data<-.jagr_model" <- function (object, value) {
  
  if (!is.null(value)) {
    if (!is.character(value)) {
      stop ("select_data must be NULL or class character")
    }
    if (!length(value)) {
      stop ("select_data must be NULL or define at least one variable to include")
    }
    names <- names_select(value)
    if (any(duplicated(names))) {
      stop ("variables to select_data must be unique")
    }
  }
  
  object$select_data <- value
  
  return (object)
}

#' @method select_data<- jags_model
#' @export
"select_data<-.jags_model" <- function (object, value) {
  
  if(is.list(value) && length(value) != nmodels(object))
    stop("if value is a list it must be the same length as the number of models in object")
  
  if(is.list(value))
    names(value) <- NULL
  
  models <- models(object)
  
  for (i in 1:length(models)) {
    if(!is.list(value)) {
      select_data(models[[i]]) <- value
    } else
      select_data(models[[i]]) <- value[[i]]
  }
  
  models(object) <- models
  return (object)
}

