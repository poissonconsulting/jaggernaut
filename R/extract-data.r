
#' @export
extract_data <- function (object) {
  UseMethod("extract_data", object)
}

#' @export
"extract_data<-" <- function (object, value) {
  UseMethod("extract_data<-", object)
}

#' @method extract_data jags_data_model
#' @export
extract_data.jags_data_model <- function (object) {
  return (object$extract_data)
}

#' @method extract_data<- jags_data_model
#' @export
"extract_data<-.jags_data_model" <- function (object, value) {
  
  if(!is.null(value)) {
    if (!is.function(value)) {
      stop ("extract_data must be NULL or a function")
    }
    args <- names(formals(value))
    if (!identical(args,c("data"))) {
      stop ("extract_data argument must be named data")
    }
  }
  
  object$extract_data <- value
  return (object)
}
