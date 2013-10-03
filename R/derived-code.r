
derived_code <- function (object, ...) {
  UseMethod("derived_code", object)
}

"derived_code<-" <- function (object, value, ...) {
  UseMethod("derived_code<-", object)
}

derived_code.jags_model <- function (object, ...) {
  
  object <- object$derived_code
  
  object <- delist(object)
  
  if(length(object) == 1 && is.na(object))
    return (NULL)
  
  return (object)
}
  
derived_code.jags_analysis <- function (object, ...) {
  return (derived_code(as.jags_model(object), ...))
}  

"derived_code<-.jags_model" <- function (object, value, ...) {
  
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
