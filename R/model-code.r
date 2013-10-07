
#' @export
model_code <- function (object, ...) {
  UseMethod("model_code", object)
}

#' @export
"model_code<-" <- function (object, value, ...) {
  UseMethod("model_code<-", object)
}

#' @method model_code jagr_model
#' @export
model_code.jagr_model <- function (object, ...) {
  return (object$model_code)
}

model_code_jagr_model <- function (object, ...) {
  stopifnot(is.jagr_model(object))
  return (model_code(object, ...))
}

#' @method model_code jags_model
#' @export
model_code.jags_model <- function (object, ...) {
  
  if(is_one_model(object))
    return (model_code(model(object),...))
  
  models <- models(object)
  models <- lapply(models, model_code_jagr_model, ...)
  models <- name_object(models, "Model")
  return (models)  
}

model_code.jagr_analysis <- function (object, ...) {
  return (model_code(as.jagr_model(object), ...))
}

model_code_jagr_analysis <- function (object, ...) {
  stopifnot(is.jagr_analysis(object))
  return (model_code(object, ...))
}

#' @method model_code jags_analysis
#' @export
model_code.jags_analysis <- function (object, ...) {
  return (model_code(as.jags_model(object), ...))
}

#' @method model_code<- jagr_model
#' @export
"model_code<-.jagr_model" <- function (object, value, ...) {
  
  if (is.character (value)) {
    if (length(value) != 1) {
      stop ("model_code must define a single model - for multiple models use add_jags to add multiple jags_models")
    }
  } else {
    stop ("model_code must be class character")
  }
  
  object$model_code <- value
  
  return (object)
}

#' @method model_code<- jags_model
#' @export
"model_code<-.jags_model" <- function (object, value, ...) {
  
  if(is.list(value) && length(value) != nmodels(object))
    stop("if value is a list it must be the same length as the number of models in object")
  
  if(is.list(value))
    names(value) <- NULL
  
  models <- models(object)
  
  for (i in 1:length(models)) {
    if(!is.list(value)) {
      model_code(models[[i]]) <- value
    } else
      model_code(models[[i]]) <- value[[i]]
  }
  
  models(object) <- models
  return (object)
}

#' @method model_code<- jagr_analysis
#' @export
"model_code<-.jagr_analysis" <- function (object, value, ...)
  stop("cannot replace model code in a jagr_analysis object")
