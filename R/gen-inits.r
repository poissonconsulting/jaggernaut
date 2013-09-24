
gen_inits <- function (object, ...) {
  UseMethod("gen_inits", object)
}

"gen_inits<-" <- function (object, value, ...) {
  UseMethod("gen_inits<-", object)
}

gen_inits.jags_model <- function (object, ...) {
  x <- list()
  for (i in 1:length(object$models))
    x[i] <- object$models[[i]]$gen_inits
  x <- delist(x)
  if (length(x) == 0)
    return (NULL)
  return (x)
}

"gen_inits<-.jags_model" <- function (object, value, ...) {

  if (!is.null(value)) {
    if (!is.function(value)) {
      stop ("gen_inits must be NULL or a function")
    }
    args <- names(formals(value))
    if (!identical(args,c("data"))) {
      stop ("gen_inits argument must be named data")
    }
  }
  
  for (i in 1:length(object$models))
    object$models[[i]]$gen_inits <- value
  return (object)
}
