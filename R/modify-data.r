
modify_data <- function (object, ...) {
  UseMethod("modify_data", object)
}

"modify_data<-" <- function (object, value, ...) {
  UseMethod("modify_data<-", object)
}

modify_data.jagr_model <- function (object, ...) {
  return (object$modify_data)
}

modify_data_jagr_model <- function (object, ...) {
  stopifnot(is.jagr_model(object))
  return (modify_data(object, ...))
}

modify_data.jags_model <- function (object, ...) {
  
  object <- as.jagr_model(object)
  
  if(is.jagr_model(object))
    return (modify_data(object, ...))
  
  object <- lapply(object, modify_data, ...)
  
  object <- delist(object)
  if (length(object) == 0)
    return (NULL)
  return (object)  
}

modify_data.jags_data_model <- function (object, ...) {
  return (modify_data(as.jags_model(object), ...))
}

modify_data.jagr_analysis <- function (object, ...) {
  return (modify_data(as.jagr_model(object, ...)))
}

modify_data_jagr_analysis <- function (object, ...) {
  stopifnot(is.jagr_analysis(object))
  return (modify_data(object, ...))
}

modify_data.jags_analysis <- function (object, ...) {
  return (modify_data(as.jags_model(object), ...))
}

"modify_data<-.jagr_model" <- function (object, value, ...) {
  
  if(!is.null(value)) {
    if (!is.function(value)) {
      stop ("modify_data must be NULL or a function")
    }
    args <- names(formals(value))
    if (!identical(args,c("data")) && !identical(args,c("data","analysis"))) {
      stop ("modify_data argument(s) must be named data (and analysis)")
    }
  }
  
  object$modify_data <- value
  
  return (object)
}

"modify_data<-.jags_model" <- function (object, value, ...) {
  
  for (i in 1:nmodels(object))
    modify_data(object$models[[i]], ...) <- value
  
  return (object)
}

"modify_data<-.jags_data_model" <- function (object, value, ...) {
  
  for (i in 1:nmodels(object))
    modify_data(object$models[[i]], ...) <- value
  
  return (object)
}
