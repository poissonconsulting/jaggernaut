
powers_level <- function (object, ...) {
  UseMethod("powers_level", object)
}

"powers_level<-" <- function (object, value) {
  UseMethod("powers_level<-", object)
}

powers_level.jags_powers_analysis <- function (object, ...) {
  return (object$powers_level)
}

"powers_level<-.jags_powers_analysis" <- function (object, value) {
  stopifnot(is.numeric(value) && is_scalar(value))
  stopifnot(value >= 0.50 && value <= 0.99)
  
  object$powers_level <- value
  return (object)
}
