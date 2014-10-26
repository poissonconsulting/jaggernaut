
time_interval <- function (object, ...) {
  UseMethod("time_interval", object)
}

"time_interval<-" <- function (object, value) {
  UseMethod("time_interval<-", object)
}

time_interval.jagr_analysis <- function (object, ...) {
  return (object$time_interval)
}

time_interval_jagr_analysis <- function (object, ...) {
  stopifnot(is.jagr_analysis(object))
  return (time_interval(object, ...))
}

"time_interval<-.jagr_analysis" <- function (object, value) {
    
  object$time_interval <- value
  
  return (object)
}
