get_min <- function(x, observed = FALSE) {
  assert_that(is.flag(observed))

  UseMethod("get_min", x)
}

get_min.continuous <- function(object, observed = FALSE) {
  return (object$obs[1])
}

get_min.categorical <- function(object, observed = FALSE) {
  if(observed)
    return (object$obs[1])
  
  return (object$levels[1])
}
