
get_mean <- function(object) {
  UseMethod("get_mean", object)
}

get_mean.dvariable <- function(object) {

  return (object$mean)
}

get_mean.dfactor <- function(object) {
  
  return (object$min)
}