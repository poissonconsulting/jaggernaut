
#' @title Calculate convergence for a JAGS analysis
#'
#' @description
#' Calculate convergence for a JAGS analysis
#' 
#' @param object a janalysis object
#' @param parameters a character vector of the parameters to calculate the convergence
#' @return a data.frame of the parameter convergence
#' @export
#' @examples
#' model <- jmodel("model { bLambda ~ dunif(0,10) for (i in 1:nrow) { x[i]~dpois(bLambda) } }")
#' data <- data.frame(x = rpois(100,1))
#' analysis <- janalysis (model, data)
#' convergence(analysis)
#' convergence(analysis,c("bLambda"))
convergence <- function (object, parameters = "fixed") {
  if(!is.janalysis(object))
    stop ("object should be class janalysis")
  
  return (calc_convergence.janalysis (object, summarise = FALSE, type = parameters))
}
