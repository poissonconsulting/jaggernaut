
get_base_value <- function(object) {
  UseMethod("get_base_value", object)
}

get_base_value.dvariable <- function(object) {

  return (object$mean)
}

get_base_value.vlogical<- function(object) {
  
  return (object$min)
}

get_base_value.vfactor <- function(object) {
  
  return (object$min)
}

get_base_value.mlogical<- function(object) {
  
  return (object$min)
}

get_base_value.alogical<- function(object) {
  
  return (object$min)
}
