
#' @title Get extract data
#'
#' @description
#' Get the extract_data component of a JAGS object.  
#' 
#' @param object a JAGS object.
#' @param ... further arguments passed to or from other methods.
#' @return The extract_data component of a JAGS object which is either 
#' a function or NULL.
#' @seealso \code{\link{jaggernaut}}  
#' @export
extract_data <- function (object, ...) {
  UseMethod("extract_data", object)
}

#' @title Set extract data
#'
#' @description
#' Sets the extract_data component of a JAGS object.  
#' 
#' @usage
#' extract_data(object) <- value
#' @param object a JAGS object.
#' @param value a function or NULL.
#' @return The replacement method changes the extract_data component of the object.
#' @seealso \code{\link{extract_data}} and \code{\link{jaggernaut}}  
#' @export
"extract_data<-" <- function (object, value) {
  UseMethod("extract_data<-", object)
}

#' @method extract_data jags_data_model
#' @export
extract_data.jags_data_model <- function (object, ...) {
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
