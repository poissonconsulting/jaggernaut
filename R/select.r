
#' @export
select <- function (object) {
  UseMethod("select", object)
}

#' @export
"select<-" <- function (object, value) {
  UseMethod("select<-", object)
}

#' @method select jagr_model
#' @export
select.jagr_model <- function (object) {
  return (object$select)
}

select_jagr_model <- function (object) {
  stopifnot(is.jagr_model(object))
  return (select(object))
}

#' @method select jags_model
#' @export
select.jags_model <- function (object) {
  
  if(is_one_model(object))
    return (select(model(object)))
  
  models <- models(object)
  models <- lapply(models, select_jagr_model)
  models <- name_object(models, "Model")
  return (models)   
}

select.jagr_analysis <- function (object) {
  return (select(as.jagr_model(object)))
}

select_jagr_analysis <- function (object) {
  stopifnot(is.jagr_analysis(object))
  return (select(object))
}

#' @method select jags_analysis
#' @export
select.jags_analysis <- function (object) {
  return (select(as.jags_model(object)))
}

#' @method select<- jagr_model
#' @export
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

#' @method select<- jagr_analysis
#' @export
"select<-.jagr_analysis" <- function (object, value)
  stop("cannot replace select in a jagr_analysis object")

