
coefs <- function (object, ...) {
  UseMethod("coefs", object)
}

"coefs<-" <- function (object, ...) {
  UseMethod("coefs<-", object)
}

coefs.jags_power_analysis <- function (object, ...) {
  return (object$coefs)  
}

"coefs<-.jags_power_analysis" <- function (object, value, ...) {
  stopifnot(is.data.frame(value))
  object$coefs <- value
  return (object)
}
