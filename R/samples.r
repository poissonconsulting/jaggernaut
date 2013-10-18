
samples <- function (object) {
  UseMethod("samples", object)
}

"samples<-" <- function (object, value) {
  UseMethod("samples<-", object)
}

samples.jagr_chains <- function (object) {
  return (object$samples)
}

"samples<-.jagr_chains" <- function (object, value) {
  
  stopifnot(is.list_mcarray(value))
  
  rhat(object) <- NULL  
  object$samples <- value
  
  return (object)
}
