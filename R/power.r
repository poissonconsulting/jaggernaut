
power <- function (object, ...) {
  UseMethod("power", object)
}

"power<-" <- function (object, value, ...) {
  UseMethod("power<-", object)
}

"power.jags_power_analysis" <- function (object, ...) {
  return (object$power)
}

"power<-.jags_power_analysis" <- function (object, value, ...) {
  
  object$power <- value
  return (object)
}
