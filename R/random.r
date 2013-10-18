
random <- function (object) {
  UseMethod("random", object)
}

"random<-" <- function (object, value) {
  UseMethod("random<-", object)
}

random.jagr_chains <- function (object) {
  return (object$random)
}

"random<-.jagr_chains" <- function (object, value) {
  
  stopifnot(is.null(value) || (is.character(value) && length(value) > 0))
    
  object$random <- value
  
  return (object)
}
