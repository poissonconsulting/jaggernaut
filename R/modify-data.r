
modify_data <- function (object, ...) {
  UseMethod("modify_data", object)
}

"modify_data<-" <- function (object, value, ...) {
  UseMethod("modify_data<-", object)
}

modify_data.jags_model <- function (object, ...) {
  x <- list()
  for (i in 1:length(object$models))
    x[i] <- object$models[[i]]$modify_data
  x <- delist(x)
  if (length(x) == 0)
    return (NULL)
  return (x)
}

modify_data.jagr_analysis <- function (object, ...) {
  return (modify_data(as.jags_model(object, ...)))
}

modify_data_jagr_analysis <- function (object, ...) {
  stopifnot(is.jagr_analysis(object))
  return (modify_data(object, ...))
}

"modify_data<-.jags_model" <- function (object, value, ...) {
  
  if(is.null(value))
    return (object)
  
  if (!is.function(value)) {
    stop ("modify_data must be NULL or a function")
  }
  args <- names(formals(value))
  if (!identical(args,c("data")) && !identical(args,c("data","analysis"))) {
    stop ("modify_data argument(s) must be named data (and analysis)")
  }
  
  for (i in 1:length(object$models))
    object$models[[i]]$modify_data <- value
  return (object)
}
