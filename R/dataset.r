
#' @title Get data from a JAGS analysis
#'
#' @description
#' Gets data from a JAGS analysis
#' 
#' @param object a janalysis object
#' @param base a logical scalar indicating whether to return the base values
#' @return a data frame of the data used in the analysis or the corresponding base values
#' @export
#' @examples
#' model <- jmodel("model { bLambda ~ dunif(0,10) for (i in 1:nrow) { x[i]~dpois(bLambda) } }")
#' data <- data.frame(x = rpois(100,1))
#' analysis <- janalysis (model, data)
#' dataset(analysis)
#' dataset(analysis, base = TRUE)
dataset <- function (object, base = FALSE) {
  if(!is.janalysis(object))
    stop("object should be of class janalysis")
  
  object <- object$analyses[[1]]$data

  if(!base) {
    return (object$data)
  }
  
  data <- generate_data (object$data)
  if (!is.null(object$block$select))
    data <- subset (data, select = process_select(object$block$select))
  
  return (data)
}
