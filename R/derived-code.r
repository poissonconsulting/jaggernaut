
derived_code <- function (object, ...) {
  UseMethod("derived_code", object)
}

"derived_code<-" <- function (object, value, ...) {
  UseMethod("derived_code<-", object)
}

derived_code.jags_model <- function (object, ...) {
  
  x <- list()
  for (i in 1:length(object$models))
    x[[i]] <- object$models[[i]]$derived_model
  x <- delist(x)

  if (length(x) == 0)
    return (NULL)
  
  return (x)
}

derived_code.jagr_analysis <- function (object, ...) {
  return (derived_code(as.jags_model(object), ...))
}

derived_code_jagr_analysis <- function (object, ...) {
  stopifnot(is.jagr_analysis(object))
  return (derived_code(object, ...))
}
  
derived_code.jags_analysis <- function (object, ...) {
  object <- as.jagr_analysis(object)
  if (is.jagr_analysis(object))
    return (derived_code(object, ...))
  
  object <- lapply(object, derived_code_jagr_analysis, ...)
  
  return (object)
}  

"derived_code<-.jags_model" <- function (object, value, ...) {
  
  if (!is.null (value)) {
    if(!is.character(value)) {
      stop("derived_code must be a character")
    }
    if (length(value) != 1) {
      stop ("derived_code must be define a single model block")
    }
  }
  
  for (i in 1:length(object$models))
    object$models[[i]]$derived_model <- value
  return (object)
}

"derived_code<-.jagr_analysis" <- function (object, value, ...) {
  derived_code (object$model, ...) <- value
  return (object)
}

"derived_code<-.jags_analysis" <- function (object, value, ...) {
  for (i in 1:length(object$analyses)) {
    derived_code (object$analyses[[1]], ...) <- value
  }
  return (object)
}
