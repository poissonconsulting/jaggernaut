get_mean <- function(x, observed = FALSE) {
  assert_that(is.flag(observed))
  
  UseMethod("get_mean", x)
}

get_mean.continuous <- function(object, observed = FALSE) {
  if(observed)
    return (object$obs[which.min(abs(object$obs - object$mean))])
  
  return (object$mean)
}

get_mean.categorical <- function(object, observed = FALSE) {  
  return (NA)
}
