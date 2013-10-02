
model_code <- function (object, ...) {
  UseMethod("model_code", object)
}

"model_code<-" <- function (object, value, ...) {
  UseMethod("model_code<-", object)
}

model_code.jagr_model <- function (object, ...) {
  return (object$model_code)
}

model_code_jagr_model <- function (object, ...) {
  stopifnot(is.jagr_model(object))
  return (model_code(object, ...))
}

model_code.jags_model <- function (object, ...) {
  
  object <- as.jagr_model(object)
  
  if(is.jagr_model(object))
    return (model_code(object, ...))
  
  object <- lapply(object, model_code_jagr_model, ...)
  
  object <- delist(object)
  return (object)  
}

model_code.jags_data_model <- function (object, ...) {
  return (model_code(as.jags_model(object), ...))
}

model_code.jagr_analysis <- function (object, ...) {
  return (model_code(as.jagr_model(object), ...))
}

model_code_jagr_analysis <- function (object, ...) {
  stopifnot(is.jagr_analysis(object))
  return (model_code(object, ...))
}

model_code.jags_analysis <- function (object, ...) {
  return (model_code(as.jags_model(object), ...))
}

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

"model_code<-.jags_model" <- function (object, value, ...) {
  
  for (i in 1:nmodel(object))
    model_code(object$models[[i]], ...) <- value
  
  return (object)
}

"model_code<-.jags_data_model" <- function (object, value, ...) {
  
  for (i in 1:nmodel(object))
    model_code(object$models[[i]], ...) <- value
  
  return (object)
}
