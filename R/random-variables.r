
random_variables <- function (object, ...) {
  UseMethod("random_variables", object)
}

random_variables.jagr_analysis <- function (object, ...) {
  return (object$random_variables)
}

random_variables_jagr_analysis <- function (object, ...) {
  stopifnot(is.jagr_analysis(object))
  return (random_variables(object, ...))
}
