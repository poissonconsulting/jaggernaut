
#' @title Get data from a JAGS analysis object
#'
#' @description
#' Returns data from a JAGS analysis (janalysis) object
#' 
#' @param object a JAGS analysis (janalysis) object
#' @return a data frame of the data used in the JAGS analysis
#' @export
#' @examples
#' model <- jmodel(
#'  model = "model { bLambda ~ dunif(0,10) for (i in 1:nrow) { x[i]~dpois(bLambda) } }"
#' )
#' data <- data.frame(x = rpois(100,1))
#' analysis <- janalysis (model, data)
#' get_data(analysis)

get_data <- function (object) {
  if(!is.janalysis (object)) 
    stop ("object must be of class janalysis")

  return (object$analyses[[1]]$data)  
}