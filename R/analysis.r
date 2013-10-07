
analysis <- function (object, ...) {
  UseMethod("analysis", object)
}

"analysis<-" <- function (object, ...) {
  UseMethod("analysis<-", object)
}

analysis.jags_analysis <- function (object, ...) {
  stopifnot(is_one_model(object))
  return (analyses(object)[[1]])
}
