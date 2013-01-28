
set_random<- function (object, ...) {
  UseMethod("set_random", object)
}

set_random.gsmodel <- function (object, random) {
  
  if (!(is.null(random) || is.list(random)))
    stop("random must be NULL or a list")
  
  object$random <- random
  
  return (object)
}

set_random.gsanalysis <- function (object, random) {
  
  object$model <- set_random(object$model, random)
  
  return (object)
}