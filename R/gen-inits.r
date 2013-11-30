
#' @export
gen_inits <- function (object) {
  UseMethod("gen_inits", object)
}

#' @export
"gen_inits<-" <- function (object, value) {
  UseMethod("gen_inits<-", object)
}

#' @method gen_inits jagr_model
#' @export
gen_inits.jagr_model <- function (object) {
  return (object$gen_inits)
}

gen_inits_jagr_model <- function (object) {
  stopifnot(is.jagr_model(object))
  return (gen_inits(object))
}

#' @method gen_inits jags_model
#' @export
gen_inits.jags_model <- function (object) {
  
  if(is_one_model(object))
    return (gen_inits(model(object)))
  
  models <- models(object)
  models <- lapply(models, gen_inits_jagr_model)
  models <- name_object(models, "Model")
  return (models)  
}

#' @method gen_inits jags_analysis
#' @export
gen_inits.jags_analysis <- function (object) {
  return (gen_inits(as.jags_model(object)))
}

#' @method gen_inits<- jagr_model
#' @export
"gen_inits<-.jagr_model" <- function (object, value) {
  
  if (!is.null(value)) {
    if (!is.function(value)) {
      stop ("gen_inits must be NULL or a function")
    }
    args <- names(formals(value))
    if (!identical(args,c("data"))) {
      stop ("gen_inits argument must be named data")
    }
  }
  
  object$gen_inits <- value
  
  return (object)
}

#' @method gen_inits<- jags_model
#' @export
"gen_inits<-.jags_model" <- function (object, value) {
  
  if(is.list(value) && length(value) != nmodels(object))
    stop("if value is a list it must be the same length as the number of models in object")
  
  if(is.list(value))
    names(value) <- NULL
  
  models <- models(object)
  
  for (i in 1:length(models)) {
    if(!is.list(value)) {
      gen_inits(models[[i]]) <- value
    } else
      gen_inits(models[[i]]) <- value[[i]]
  }
  
  models(object) <- models
  return (object)
}

#' @method gen_inits<- jagr_analysis
#' @export
"gen_inits<-.jagr_analysis" <- function (object, value) {
  stop("cannot replace gen_inits in a jagr_analysis object")
}
