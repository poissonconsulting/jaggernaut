
set_derived<- function (object, ...) {
  UseMethod("set_derived", object)
}

set_derived.gsmodel <- function (object, derived) {  
  if (!is.character(derived))
    stop("derived must be of class character")
  
  object$derived <- derived
  
  return (object)
}

set_derived.gsanalysis <- function (object, derived) {  
  if (!is.character(derived))
    stop("derived must be of class character")
  
  object$model <- set_derived (object$model, derived = derived)
  
  return (object)
}


