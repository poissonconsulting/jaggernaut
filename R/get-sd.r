get_sd <- function(x) {  
  UseMethod("get_sd", x)
}

get_sd.continuous <- function(object) {
  return (object$sd)
}

get_sd.categorical <- function(object) {  
  return (NA)
}
