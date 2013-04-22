
#' @title Calculate parameter estimates
#'
#' @description
#' Calculates parameter estimates for a JAGS analysis
#' 
#' @param object a \code{jags_analysis} object
#' @param model an integer element specifying the model to select. 
#' If model = 0 then it selects the model with the lowest DIC.
#' @param parameters a character vector of the parameters to calculate the estimates
#' @return a data.frame of the parameter estimates with the median estimate and 
#' lower and upper 95\% credible limits as well as the percent relative error 
#' and significance
#' @seealso \code{\link{jags_analysis}} and \code{\link{jaggernaut}}
#' @export
estimates <- function (object, model = 1, parameters = "fixed") {
  if(!is.jags_analysis(object))
    stop ("object should be class jags_analysis")
  
  object <- subset(object, model = model)
  
  est <- calc_estimates(object,parameters = parameters)
  
  est <- est[rownames(est) != "deviance",]

  return (est)
}
