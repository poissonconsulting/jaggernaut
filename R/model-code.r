
model_code <- function (object, ...) {
  UseMethod("model_code", object)
}

"model_code<-" <- function (object, value, ...) {
  UseMethod("model_code<-", object)
}

model_code.jags_model <- function (object, ...) {
  
  x <- character()
  for (i in 1:length(object$models))
    x[[i]] <- object$models[[i]]$model_code
  return (x)
}

model_code.jagr_analysis <- function (object, ...) {
  return (model_code(as.jags_model(object), ...))
}

model_code.jags_analysis <- function (object, ...) {
  return (model_code(as.jags_model(object), ...))
}

"model_code<-.jags_model" <- function (object, value, ...) {
  
  if(is.null(value))
    return (object)
  
  if (is.character (value)) {
    if (length(value) != 1) {
      stop ("model_code must be define a single model - for multiple models use add_jags to add multiple jags_models")
    }
  } else {
    stop ("model_code must be class character")
  }
  
  for (i in 1:length(object$models))
    object$models[[i]]$model_code <- value
  return (object)
}
