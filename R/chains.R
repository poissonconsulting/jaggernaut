
chains <- function (object) {
  UseMethod("chains", object)
}

"chains<-" <- function (object, value) {
  UseMethod("chains<-", object)
}

chains.jagr_analysis <- function (object) {
  return (object$chains)
}

chains_jagr_analysis <- function (object) {
  stopifnot(is.jagr_analysis(object))
  return (chains(object))
}

"chains<-.jagr_analysis" <- function (object, value) {
  
  stopifnot(is.jagr_chains(value))
  
  object$chains <- value
  
  return (object)
}
