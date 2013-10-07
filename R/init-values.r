
#' @export
init_values <- function (object, ...) {
  UseMethod("init_values", object)
}

"init_values<-" <- function (object, value, ...) {
  UseMethod("init_values<-", object)
}

init_values.jagr_power_analysis <- function (object, ...) {
  return (object$init_values)
}

init_values_jagr_power_analysis <- function (object, ...) {
  stopifnot(is.jagr_power_analysis(object))
  return (init_values(object, ...))
}

"init_values<-.jagr_power_analysis" <- function (object, value, ...) {
  
  object$init_values <- value
  
  return (object)
}
