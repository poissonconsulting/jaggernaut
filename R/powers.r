
powers <- function (object, ...) {
  UseMethod("powers", object)
}

"powers<-" <- function (object, value) {
  UseMethod("powers<-", object)
}

powers.jags_power_analysis <- function (object, ...) {
  return (object$powers)  
}

"powers<-.jags_power_analysis" <- function (object, value) {
  stopifnot(is.data.frame(value))
  object$powers <- value
  return (object)
}
