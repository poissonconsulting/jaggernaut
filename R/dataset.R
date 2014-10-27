#' @title Get dataset(s) from a JAGS object
#'
#' @description
#' Gets the dataset(s) from a JAGS object.  
#' 
#' @param object a JAGS object.
#' @param ... further arguments passed to or from other methods.
#' @return a data.frame or list(s) of the data
#' @export
dataset <- function (object, ...) {
  UseMethod("dataset", object)
}

"dataset<-" <- function (object, value) {
  UseMethod("dataset<-", object)
}

#' @title Get dataset
#'
#' @description
#' Gets the dataset from a \code{jags_sample} object.  
#' 
#' @param object a \code{jags_sample} object.
#' @param ... further arguments passed to or from other methods.
#' @return The dataset.
#' @method dataset jags_sample
#' @export
dataset.jags_sample <- function (object, ...) {
  object <- object[,-grep("[[:digit:]]", colnames(object)), drop = FALSE]
  return (object)
}

#' @title Get dataset from a JAGS analysis object
#'
#' @description
#' Returns the original dataset from a \code{jags_analysis} object.  
#' 
#' @param object a \code{jags_analysis} object.
#' @param ... further arguments passed to or from other methods.
#' @param converted a logical scalar indicating whether the data should be
#' converted.
#' @return The original dataset.
#' @seealso \code{\link{dataset}}, \code{\link{jags_analysis}}
#' and \code{\link{jaggernaut}}
#' @method dataset jags_analysis
#' @export
dataset.jags_analysis <- function (object, converted = FALSE, ...) {
  assert_that(is.flag(converted) && noNA(converted))
    
  if(!converted)
    return (object$data)
    
  data_list <- list()
  
  analyses <- analyses(object)
  for(i in 1:nanalyses(object)) {
    model <- analyses[[i]]
    
    data <- translate_data(select_data(model), object$data) 
    
    if (is.function(modify_data(model))) 
      data <- modify_data(model)(data)
    
    data_list[[i]] <- data
  }
    
  if(length(data_list) == 1)
    return (data_list[[1]])

  names(data_list) <- model_names(object, reference = TRUE)
  
  data_list
}

#' @title Convert jags_discrepancies object to a data.frame
#'
#' @description
#' Returns dataset for plotting posterior predictive checks
#' from a \code{jags_analysis} object.  
#' 
#' @param object a \code{jags_discrepancies} object.
#' @param ... further arguments passed to or from other methods.
#' @return data.frame
#' @importFrom reshape2 melt dcast
#' @method dataset jags_discrepancies
#' @export
dataset.jags_discrepancies <- function (object, ...) {
  
  object <- reshape2::melt(object, id.vars = c("parameter", "index"),
    variable.name = "iteration")
  object <- reshape2::dcast(object, ... ~ index)
  object
}

"dataset<-.jags_analysis" <- function (object, value) {
  stopifnot(is_convertible_data(value))
  
  object$data <- value
  
  return (object)
}
