
#' @title Get derived code from a JAGS object
#'
#' @description
#' Gets the derived_code component of a JAGS object.  
#' 
#' @param object a JAGS object.
#' @param ... further arguments passed to or from other methods.
#' @return The derived_code component of object.
#' @seealso \code{\link{jaggernaut}}  
#' @export
derived_code <- function (object, ...) {
  UseMethod("derived_code", object)
}

#' @title Set derived code in a JAGS object
#'
#' @description
#' Sets the derived_code component of a JAGS object.  
#' 
#' @usage
#' derived_code(object) <- value
#' @param object a JAGS object.
#' @param value a character element or NULL.
#' @return The replacement method changes the derived_code component of the object.
#' @seealso \code{\link{derived_code}} and \code{\link{jaggernaut}}  
#' @export
"derived_code<-" <- function (object, value) {
  UseMethod("derived_code<-", object)
}

derived_code.jagr_model <- function (object, ...) {
  return (object$derived_code)
}

derived_code_jagr_model <- function (object, ...) {
  stopifnot(is.jagr_model(object))
  return (derived_code(object, ...))
}

#' @method derived_code jags_model
#' @export
derived_code.jags_model <- function (object, ...) {
  
  if(is_one_model(object))
    return (derived_code(model(object), ...))
  
  lapply(models(object), derived_code_jagr_model, ...)
}
  
#' @method derived_code jags_analysis
#' @export
derived_code.jags_analysis <- function (object, ...) {
  return (derived_code(as.jags_model(object), ...))
}  

"derived_code<-.jagr_model" <- function (object, value) {
  
  if(!is.null(value)) {
    
    if(jg_nblock(value) != 1)
      stop ("derived code must define a single model block")
    
    if(!identical(jg_block_names(value),"data")) {
      message("derived code converted to data block")
      jg_block_names(value) <- "data"
    }
  }
  
  object$derived_code <- value
  
  return (object)
}

#' @method derived_code<- jags_model
#' @export
"derived_code<-.jags_model" <- function (object, value) {
  
  if(is.list(value) && length(value) != nmodels(object))
    stop("if value is a list it must be the same length as the number of models in object")
  
  if(is.list(value))
    names(value) <- NULL
  
  models <- models(object)
  
  for (i in 1:length(models)) {
    if(!is.list(value)) {
      derived_code(models[[i]]) <- value
    } else
      derived_code(models[[i]]) <- value[[i]]
  }
  
  models(object) <- models
  return (object)
}

#' @method derived_code<- jags_analysis
#' @export
"derived_code<-.jags_analysis" <- function (object, value) {
  
  if (!is.null (value)) {
    if(!is.character(value)) {
      stop("derived_code must be NULL or a character")
    }
    if (length(value) != 1) {
      stop ("derived_code must be define a single model block")
    }
  }
  
  if(is.null(value))
    is.na(value) <- TRUE
  
  for (i in 1:nmodels(object))
    derived_code(object$analyses[[i]]) <- value
  
  return (object)
}
