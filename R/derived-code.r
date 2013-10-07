
#' @export
derived_code <- function (object, ...) {
  UseMethod("derived_code", object)
}

#' @export
"derived_code<-" <- function (object, value, ...) {
  UseMethod("derived_code<-", object)
}

derived_code.jagr_analysis_model <- function (object, ...) {
  return (object$derived_code)
}

derived_code_jagr_analysis_model <- function (object, ...) {
  stopifnot(is.jagr_analysis_model(object))
  return (derived_code(object, ...))
}

#' @method derived_code jags_model
#' @export
derived_code.jags_model <- function (object, ...) {
  
  if(is_one_model(object))
    return (derived_code(model(object),...))
  
  models <- models(object)
  models <- lapply(models, derived_code_jagr_analysis_model, ...)
  models <- name_object(models, "Model")
  return (models) 
}

#' @method derived_code jags_analysis
#' @export
derived_code.jags_analysis <- function (object, ...) {
  return (derived_code(as.jags_model(object), ...))
}  

"derived_code<-.jagr_analysis_model" <- function (object, value, ...) {
  
  if (!is.null (value)) {
    if(!is.character(value)) {
      stop("derived_code must be NULL or a character")
    }
    if (length(value) != 1) {
      stop ("derived_code must be define a single model block")
    }
  }
  
  object$derived_code <- value
  
  return (object)
}

#' @method derived_code<- jags_model
#' @export
"derived_code<-.jags_model" <- function (object, value, ...) {
  
  if(is.list(value) && length(value) != nmodels(object))
    stop("if value is a list it must be the same length as the number of models in object")
  
  if(is.list(value))
    names(value) <- NULL
  
  models <- models(object)
  
  for (i in 1:length(models)) {
    if(!is.list(value)) {
      derived_code(models[[i]]) <- value
    } else
      derived_code(models[[i]]) <- value[[i]]
  }
  
  models(object) <- models
  return (object)
}

#' @method derived_code<- jags_analysis
#' @export
"derived_code<-.jags_analysis" <- function (object, value, ...) {
  
  if (!is.null (value)) {
    if(!is.character(value)) {
      stop("derived_code must be NULL or a character")
    }
    if (length(value) != 1) {
      stop ("derived_code must be define a single model block")
    }
  }
  
  if(is.null(value))
    is.na(value) <- TRUE
  
  for (i in 1:nmodels(object))
    object$derived_code[[i]] <- value
  
  return (object)
}
