
init_values <- function (object, ...) {
  UseMethod("init_values", object)
}

"init_values<-" <- function (object, value) {
  UseMethod("init_values<-", object)
}

init_values.jagr_analysis <- function (object, ...) {
  return (object$init_values)
}

init_values_jagr_analysis <- function (object, ...) {
  stopifnot(is.jagr_analysis(object))
  return (init_values(object, ...))
}

"init_values<-.jagr_analysis" <- function (object, value) {
  
  object$init_values <- value
  
  return (object)
}
