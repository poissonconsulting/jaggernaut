
#' @title Get values
#'
#' @description
#' Get the values component of a JAGS object.  
#' 
#' @param object a JAGS object.
#' @param ... further arguments passed to or from other methods.
#' @return The values component of a JAGS object.
#' @seealso \code{\link{jaggernaut}}  
#' @export
values <- function (object, ...) {
  UseMethod("values", object)
}

"values<-" <- function (object, value) {
  UseMethod("values<-", object)
}

#' @method values jags_simulation
#' @export
values.jags_simulation <- function (object, ...) {
  values <- object$values
  rownames(values) <- paste0("value",1:nrow(values))
  return (values)
}

"values<-.jags_simulation" <- function (object, value) {
  
  if(!is.data.frame(value))
    stop ("value must be a data frame")
  
  if(nrow(value) == 0)
    stop ("value must have at least one row of data")
  
  if(ncol(value) == 0)
    stop ("value must have at least one column of data")
  
  rownames(value) <- NULL
  object$values <- value
  return (object)
}
