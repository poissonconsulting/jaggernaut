
#' @title Calculate parameter estimates
#'
#' @description
#' Calculates parameter estimates for a JAGS analysis
#' 
#' @param object a \code{janalysis} object
#' @param model an integer element specifying the model to select. 
#' If model = 0 then it selects the model with the lowest DIC.
#' @param parameters a character vector of the parameters to calculate the estimates
#' @return a data.frame of the parameter estimates with the median estimate and 
#' lower and upper 95\% credible limits as well as the percent relative error 
#' and significance
#' @seealso \code{\link{jaggernaut}} and \code{\link{analysis}}
#' @export
estimates <- function (object, model = 1, parameters = "fixed") {
  if(!is.janalysis(object))
    stop ("object should be class janalysis")
  
  object <- subset(object, model = model)
  
  est <- calc_estimates(object,parameters = parameters)
  
  est <- est[rownames(est) != "deviance",]

  return (est)
}
