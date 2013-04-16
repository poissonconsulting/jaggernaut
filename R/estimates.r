
#' @title Calculate parameter estimates for a JAGS analysis
#'
#' @description
#' Calculate parameter estimates for a JAGS analysis
#' 
#' @param object a \code{\link{janalysis}} object
#' @param parameters a character vector of the parameters to calculate the estimates
#' @param average a logical scalar indicating whether to model average - not yet implemented
#' @return a data.frame of the parameter estimates with the median estimate and 
#' lower and upper 95% credibility limits as well as the percent relative error and significance
#' @seealso \link[jaggernaut]{jmodel}, \link[jaggernaut]{janalysis}
#' @examples
#' # Poisson GLM analysis of peregrine breeding pairs (Kery & Schaub 2011 p.55-66)
#' model <- jmodel(" 
#'  model { 
#'    alpha ~ dunif(-20, 20)
#'    beta1 ~ dunif(-10, 10)
#'    beta2 ~ dunif(-10, 10)
#'    beta3 ~ dunif(-10, 10)
#'    
#'    for (i in 1:nrow) { 
#'      log(eCount[i]) <- alpha + beta1 * Year[i] 
#'        + beta2 * Year[i]^2 + beta3 * Year[i]^3
#'      Count[i] ~ dpois(eCount[i])
#'    } 
#'  }",
#' select = c("Count","Year*")
#')
#' data <- peregrine
#' data$Count <- data$Pairs
#' analysis <- janalysis (model, data)
#' estimates(analysis)
#' @export
estimates <- function (object, parameters = "fixed", average = FALSE) {
  if(!is.janalysis(object))
    stop ("object should be class janalysis")
  
  return (calc_estimates(object,parameters = parameters))
}
