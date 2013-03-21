
#' @title Calculate parameter estimates for a JAGS analysis
#'
#' @description
#' Calculate parameter estimates for a JAGS analysis
#' 
#' @param object a janalysis object
#' @param parameters a character vector of the parameters to calculate the estimates
#' @return a data.frame of the parameter estimates with the median estimate and 
#' lower and upper 95% credibility limits as well as the percent relative error and significance
#' @export
#' @examples
#' model <- jmodel("model { bLambda ~ dunif(0,10) for (i in 1:nrow) { x[i]~dpois(bLambda) } }")
#' data <- data.frame(x = rpois(100,1))
#' analysis <- janalysis (model, data)
#' estimates(analysis)
estimates <- function (object, parameters = "fixed") {
  if(!is.janalysis(object))
    stop ("object should be class janalysis")
  
  return (calc_estimates(object,parameters = parameters))
}
