
get_mean <- function(object) {
  UseMethod("get_mean", object)
}

get_mean.dvariable <- function(object) {

  return (object$mean)
}

get_mean.vlogical<- function(object) {
  
  return (object$min)
}

get_mean.vfactor <- function(object) {
  
  return (object$min)
}

get_mean.mlogical<- function(object) {
  
  return (object$min)
}

get_mean.alogical<- function(object) {
  
  return (object$min)
}
