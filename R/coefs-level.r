
coefs_level <- function (object, ...) {
  UseMethod("coefs_level", object)
}

"coefs_level<-" <- function (object, ...) {
  UseMethod("coefs_level<-", object)
}

coefs_level.jags_power_analysis <- function (object, ...) {
  return (object$coefs_level)
}

"coefs_level<-.jags_power_analysis" <- function (object, value, ...) {
  stopifnot(is.numeric(value) && is_scalar(value))
  stopifnot(value >= 0.75 && value <= 0.99)

  object$coefs_level <- value
  return (object)
}
