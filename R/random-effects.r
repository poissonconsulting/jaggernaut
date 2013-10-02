
random_effects <- function (object, ...) {
  UseMethod("random_effects", object)
}

"random_effects<-" <- function (object, value, ...) {
  UseMethod("random_effects<-", object)
}

random_effects.jags_model <- function (object, ...) {
  
  object <- object$random_effects
  
  object <- delist(object)
  
  if(length(object) == 1 && is.na(object))
    return (NULL)
  
  return (object)
}

random_effects.jags_analysis <- function (object, ...) {
  return (random_effects(as.jags_model(object), ...))
}

"random_effects<-.jags_model" <- function (object, value, ...) {
  
  if (!is.null(value)) {
    if (!is.list(value)) {
      stop ("random_effects must be NULL or a list")
    }
    names <- names(value)
    if (is.null(names)) {
      stop("random effects must be a named list")
    }
    if (any(duplicated(names))) {
      stop ("random effects must be unique")
    }
  }
  
  if(is.null(value))
    is.na(value) <- TRUE
  
  for (i in 1:nmodel(object))
    object$random_effects[[i]] <- value
  
  return (object)
}

"random_effects<-.jags_analysis" <- function (object, value, ...) {
  
  if (!is.null(value)) {
    if (!is.list(value)) {
      stop ("random_effects must be NULL or a list")
    }
    names <- names(value)
    if (is.null(names)) {
      stop("random effects must be a named list")
    }
    if (any(duplicated(names))) {
      stop ("random effects must be unique")
    }
  }
  
  if(is.null(value))
    is.na(value) <- TRUE
  
  for (i in 1:nmodel(object))
    object$random_effects[[i]] <- value
  
  return (object)
}
