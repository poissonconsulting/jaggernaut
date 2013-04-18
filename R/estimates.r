
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
#' lower and upper 95% credibility limits as well as the percent relative error 
#' and significance
#' @seealso \link{analysis}
#' @examples
#' # Poisson GLM analysis of peregrine breeding pairs (Kery & Schaub 2011 p.55-66)
#' mod <- model(" 
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
#' dat <- peregrine
#' dat$Count <- dat$Pairs
#' ana <- analysis (mod, dat)
#' estimates(ana)
#' @export
estimates <- function (object, model = 1, parameters = "fixed") {
  if(!is.janalysis(object))
    stop ("object should be class janalysis")
  
  object <- subset(object, model = model)
  
  est <- calc_estimates(object,parameters = parameters)
  
  est <- est[rownames(est) != "deviance",]

  return (est)
}
