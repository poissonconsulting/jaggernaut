
#' @title Calculate convergence values
#'
#' @description
#' Calculates convergence (R-hat) values for the parameters in a JAGS analysis
#' 
#' @param object a janalysis object
#' @param model an integer element specifying the model to select. 
#' If model = 0 then it selects the model with the lowest DIC.
#' @param parameters a character vector specifying the parameters for which to calculate the convergence
#' @return a data.frame of the parameters with their convergence (R-hat) values
#' @seealso \code{\link{analysis}}
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
#' convergence(ana)
#' @export
convergence <- function (object, model = 1, parameters = "fixed") {
  if(!is.janalysis(object))
    stop ("object should be class janalysis")
  
  object <- subset(object, model = model)
  
  con <- calc_convergence.janalysis (object, summarise = FALSE, type = parameters)
  
  con$independence <- NULL
  
  return (con)
}
