#' @title Get random effects
#'
#' @description
#' Get the random_effects component of a JAGS object.  
#' 
#' @param object a JAGS object.
#' @param ... further arguments passed to or from other methods.
#' @return The random_effects component of a JAGS object.
#' @seealso \code{\link{jaggernaut}}  
#' @export
random_effects <- function (object, ...) {
  UseMethod("random_effects", object)
}

#' @title Set random effects
#'
#' @description
#' Set the random_effects component of a JAGS object.  
#' 
#' @usage
#' random_effects(object) <- value
#' @param object a JAGS object.
#' @param value a named list vector of the random effects or NULL.
#' @seealso \code{\link{jaggernaut}}  
#' @export
"random_effects<-" <- function (object, value) {
  UseMethod("random_effects<-", object)
}

random_effects.jagr_model <- function (object, names = FALSE, ...) {
  assert_that(is.flag(names) && noNA(names))
  
  if(names)
    return(names(object$random_effects))
  object$random_effects
}

random_effects_jagr_model <- function (object, names = FALSE, ...) {
  stopifnot(is.jagr_model(object))
  return (random_effects(object, names = names, ...))
}

#' @method random_effects jags_model
#' @export
random_effects.jags_model <- function (object, names = FALSE, ...) {
  
  if(is_one_model(object))
    return (random_effects(model(object), names = names, ...))
  
  lapply(models(object), random_effects_jagr_model, names = names, ...)
}

#' @method random_effects jags_analysis
#' @export
random_effects.jags_analysis <- function (object, names = FALSE, ...) {
  random_effects(as.jags_model(object), names = names, ...)
}

"random_effects<-.jagr_model" <- function (object, value) {
  
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
  object$random_effects <- value
  
  object
}

#' @method random_effects<- jags_model
#' @export
"random_effects<-.jags_model" <- function (object, value) {
  
  if(!is.list(value) && !is.null(value))
    stop ("value must be NULL or a named list")      
  
  if(is.list(value) && is.list(value[[1]]) && length(value) != nmodels(object))
    stop("if value is a list of random_effects it must be the same length as the number of models in object")
  
  if(is.list(value[[1]]))
    names(value) <- NULL
  
  models <- models(object)
  
  for (i in 1:length(models)) {
    if(!is.list(value[[1]])) {
      random_effects(models[[i]]) <- value
    } else
      random_effects(models[[i]]) <- value[[i]]
  }
  
  models(object) <- models
  object
}

"random_effects<-.jagr_analysis" <- function (object, value) {

  random_effects(object$model) <- value
  
  object
}

#' @method random_effects<- jags_analysis
#' @export
"random_effects<-.jags_analysis" <- function (object, value) {
  
  for (i in 1:nmodels(object))
    random_effects(object$analyses[[i]]) <- value
  
  object
}
