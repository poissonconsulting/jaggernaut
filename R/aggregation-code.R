#' @title Get aggregation code from a JAGS object
#'
#' @description
#' Gets the aggregation_code component of a JAGS object.  
#' aggregation code tracks scalar parameters.
#' 
#' @param object a JAGS object.
#' @param ... further arguments passed to or from other methods.
#' @return The aggregation_code component of object.
#' @seealso \code{\link{jaggernaut}}  
#' @export
aggregation_code <- function (object, ...) {
  UseMethod("aggregation_code", object)
}

#' @title Set aggregation code in a JAGS object
#'
#' @description
#' Sets the aggregation_code component of a JAGS object.  
#' 
#' @usage
#' aggregation_code(object) <- value
#' @param object a JAGS object.
#' @param value a character element or NULL.
#' @return The replacement method changes the aggregation_code component of the object.
#' @seealso \code{\link{aggregation_code}} and \code{\link{jaggernaut}} 
#' @importFrom juggler jg_check jg_nblock jg_block_names "jg_block_names<-"
#' @export
"aggregation_code<-" <- function (object, value) {
  UseMethod("aggregation_code<-", object)
}

aggregation_code.jagr_model <- function (object, ...) {
  return (object$aggregation_code)
}

aggregation_code_jagr_model <- function (object, ...) {
  stopifnot(is.jagr_model(object))
  return (aggregation_code(object, ...))
}

#' @method aggregation_code jags_model
#' @export
aggregation_code.jags_model <- function (object, ...) {
  
  if(is_one_model(object))
    return (aggregation_code(model(object), ...))
  
  lapply(models(object), aggregation_code_jagr_model, ...)
}

#' @method aggregation_code jags_analysis
#' @export
aggregation_code.jags_analysis <- function (object, ...) {
  return (aggregation_code(as.jags_model(object), ...))
}  

"aggregation_code<-.jagr_model" <- function (object, value) {
  
  if(!is.null(value)) {
        
    if(jg_nblock(value) != 1)
      stop ("aggregation code must define a single model block")
    
    if(!identical(jg_block_names(value), "data")) {
      message("aggregation code converted to data block")
      jg_block_names(value) <- "data"
    }
  }
  object$aggregation_code <- value
  
  return (object)
}

#' @method aggregation_code<- jags_model
#' @export
"aggregation_code<-.jags_model" <- function (object, value) {
  
  if(is.list(value) && length(value) != nmodels(object))
    stop("if value is a list it must be the same length as the number of models in object")
  
  if(is.list(value))
    names(value) <- NULL
  
  models <- models(object)
  
  for (i in 1:length(models)) {
    if(!is.list(value)) {
      aggregation_code(models[[i]]) <- value
    } else
      aggregation_code(models[[i]]) <- value[[i]]
  }
  
  models(object) <- models
  return (object)
}

#' @method aggregation_code<- jags_analysis
#' @export
"aggregation_code<-.jags_analysis" <- function (object, value) {
  
  for (i in 1:nmodels(object))
    aggregation_code(object$analyses[[i]]) <- value
  
  return (object)
}
