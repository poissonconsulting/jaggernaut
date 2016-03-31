
samples <- function (object) {
  UseMethod("samples", object)
}

"samples<-" <- function (object, value) {
  UseMethod("samples<-", object)
}

samples.jagr_chains <- function (object) {
  return (object$samples)
}

samples.jags_sample <- function (object) {
  return (as.data.frame(object[,grep("^[[:digit:]]+$", colnames(object))]))
}

"samples<-.jagr_chains" <- function (object, value) {
  
  stopifnot(is_list_mcarray(value))
  
  object$convergence <- NULL  
  object$samples <- value
  
  return (object)
}
