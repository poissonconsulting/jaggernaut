
random_effects <- function (object, ...) {
  UseMethod("random_effects", object)
}

"random_effects<-" <- function (object, value, ...) {
  UseMethod("random_effects<-", object)
}

random_effects.jags_model <- function (object, ...) {
  x <- list()
  for (i in 1:length(object$models))
    x[[i]] <- object$models[[i]]$random_effects
  if (length(x) == 0)
    return (NULL)
  if(length(x) == 1 & is.list(x[[1]]))
    return (x[[1]])
  return (x)
}

random_effects.jagr_analysis <- function (object, ...) {
  return (random_effects(as.jags_model(object, ...)))
}

random_effects_jagr_analysis <- function (object, ...) {
  stopifnot(is.jagr_analysis(object))
  return (random_effects(object, ...))
}

random_effects.jags_analysis <- function (object, ...) {
  object <- as.jagr_analysis(object)
  if (is.jagr_analysis(object))
    return (random_effects(object, ...))
  
  object <- lapply(object, random_effects_jagr_analysis, ...)
  
  return (object)
}

"random_effects<-.jags_model" <- function (object, value, ...) {
  
  if(is.null(value))
    return (object)
  
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
  
  for (i in 1:length(object$models))
    object$models[[i]]$random_effects <- value
  return (object)
}

"random_effects<-.jagr_analysis" <- function (object, value, ...) {
  
  if(is.null(value))
    return (object)
  
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

  random_effects(object$model) <- value
  
  return (object)
}

"random_effects<-.jags_analysis" <- function (object, value, ...) {
  for (i in 1:length(object$analyses)) {
    random_effects (object$analyses[[1]], ...) <- value
  }
  return (object)
}
