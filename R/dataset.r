
#' @title Get data from a JAGS analysis
#'
#' @description
#' By default returns the input data set from a JAGS analysis 
#' or if \code{base = TRUE} generates a data set with one row specify the #' \emph{base} values for each of the variables. For further information
#' on the use of base values see the \code{predict.jags_analysis} function. 
#' 
#' @param object a jags_analysis object
#' @param base a logical scalar indicating whether to return the base values
#' @return a data frame of the data used in the analysis 
#' or the corresponding base values
#' @seealso \link{jags_analysis} and \link{predict.jags_analysis}
#' @export
dataset <- function (object, base = FALSE) {
  
  if(!is.jags_analysis(object))
    stop("object should be of class jags_analysis")
  
  if(base && is_data_list(object$analyses[[1]]))
    stop("if data is a data list base must be FALSE")
  
  object <- object$analyses[[1]]

  if(!base) {
    return (object$data)
  }
  
  data <- generate_data (object$data)
  if (!is.null(object$block$select))
    data <- subset (data, select = names_select(object$block$select))
  
  return (data)
}
