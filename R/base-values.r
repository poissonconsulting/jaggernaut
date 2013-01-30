
#' Get base values for JAGS analysis
#'
#' Gets base values for JAGS analysis
#' 
#' @param object a janalysis object
#' @return a data.frame of the base values for the data used in the analysis
#' @export
#' @examples
#' model <- jmodel("model { bLambda ~ dunif(0,10) for (i in 1:nrow) { x[i]~dpois(bLambda) } }")
#' data <- data.frame(x = rpois(100,1))
#' analysis <- janalysis (model, data)
#' 
base_values <- function (object) {
  if(!is.janalysis(object))
    stop("object should be of class janalysis")
  
  object <- top_model(object)
  base <- generate_data (object$data)
  if (!is.null(object$block$select))
    base <- subset (base, select = process_select(object$block$select))
  
  return (base)
}
