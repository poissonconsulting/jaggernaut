get_max <- function(x, observed = FALSE) {
  assert_that(is.flag(observed))
  
  UseMethod("get_max", x)
}

get_max.continuous <- function(object, observed = FALSE) {
  return (object$obs[length(object$obs)])
}

get_max.categorical <- function(object, observed = FALSE) {
  if(observed)
    return (object$obs[length(object$obs)])
  
  return (object$levels[length(object$levels)])
}
