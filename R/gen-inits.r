
gen_inits <- function (object, ...) {
  UseMethod("gen_inits", object)
}

"gen_inits<-" <- function (object, value, ...) {
  UseMethod("gen_inits<-", object)
}

gen_inits.jagr_model <- function (object, ...) {
  return (object$gen_inits)
}

gen_inits_jagr_model <- function (object, ...) {
  stopifnot(is.jagr_model(object))
  return (gen_inits(object, ...))
}

gen_inits.jags_model <- function (object, ...) {
  
  object <- as.jagr_model(object)
  
  if(is.jagr_model(object))
    return (gen_inits(object, ...))
  
  object <- lapply(object, gen_inits_jagr_model, ...)
  
  object <- delist(object)
  if (length(object) == 0)
    return (NULL)
  return (object)  
}

gen_inits.jags_data_model <- function (object, ...) {
  return (gen_inits(as.jags_model(object), ...))
}

gen_inits.jagr_analysis <- function (object, ...) {
  return (gen_inits(as.jagr_model(object, ...)))
}

gen_inits_jagr_analysis <- function (object, ...) {
  stopifnot(is.jagr_analysis(object))
  return (gen_inits(object, ...))
}

gen_inits.jags_analysis <- function (object, ...) {
  return (gen_inits(as.jags_model(object), ...))
}

"gen_inits<-.jagr_model" <- function (object, value, ...) {
  
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

"gen_inits<-.jags_model" <- function (object, value, ...) {
  
  for (i in 1:nmodels(object))
    gen_inits(object$models[[i]], ...) <- value
  
  return (object)
}

"gen_inits<-.jags_data_model" <- function (object, value, ...) {
  
  for (i in 1:nmodels(object))
    gen_inits(object$models[[i]], ...) <- value
  
  return (object)
}
