
#' @title Get data from a JAGS analysis
#'
#' @description
#' Returns the input data set from a JAGS analysis or generates a data set with
#' one row specify the base values for each of the variables. For further information
#' on the use of base values see the \code{derived} function. 
#' 
#' @param object a janalysis object
#' @param base a logical scalar indicating whether to return the base values
#' @return a data frame of the data used in the analysis or the corresponding base values
#' @seealso \code{\link{jaggernaut}}, \link{analysis}, \link{derived}
#' @export
dataset <- function (object, base = FALSE) {
  if(!is.janalysis(object))
    stop("object should be of class janalysis")
  
  object <- object$analyses[[1]]

  if(!base) {
    return (object$data)
  }
  
  data <- generate_data (object$data)
  if (!is.null(object$block$select))
    data <- subset (data, select = process_select(object$block$select))
  
  return (data)
}
