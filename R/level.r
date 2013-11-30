
level <- function (object, ...) {
  UseMethod("level", object)
}

"level<-" <- function (object, value) {
  UseMethod("level<-", object)
}

"level.jags_power_analysis" <- function (object, ...) {
  return (object$level)
}

"level<-.jags_power_analysis" <- function (object, value) {
  stopifnot(is_numeric(value) && is_scalar(value))
  stopifnot(value >= 0.5 && value <= 0.99) 
  
  object$level <- value
  return (object)
}
