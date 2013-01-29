
summary.mcarray <- function (object) {
  dim <- dim (object)
  print(dim)
  
  return (NULL)
}

summary.gsmcmc <- function (object) {
  cat("\nDimensions:\n")
  dim <- c(chains = nchain(object), simulations = nsim(object))
  print(dim)
  
  for (i in seq_along(object$mcmc)) {
    print(names(object$mcmc)[i])
    summary(object$mcmc[[i]])
  }
  return (NULL)  
}

#' Summary for JAGS analysis
#'
#' Prints a summary of a JAGS analysis object parameter estimates for JAGS analysis
#' 
#' @param object a janalysis object
#' @return NULL
#' @S3method summary janalysis
#' @export
#' @examples
#' model <- jmodel("model { bLambda ~ dunif(0,10) for (i in 1:nrow) { x[i]~dpois(bLambda) } }")
#' data <- data.frame(x = rpois(100,1))
#' analysis <- janalysis (model, data)
#' summary(analysis)
summary.janalysis <- function (object)
{
  cat("\nGeneration:\n")
  print(c(iterations = object$iterations,time = round(object$time,2)))
  
  cat("\nDimensions:\n")
  print(c(simulations = nsim(object),chains = nchain(object)))
  
  cat("\nConvergence:\n")
  print(calc_convergence(object))
  
  cat("\nEstimates:\n")        
  print(calc_estimates(object))

  cat("\nDeviance Information Criterion:\n")        
  print(dic(object))
  
  invisible (NULL)  
}

summary.gspower <- function (object)
{  
  cat("\nDimensions:\n")
  print(c(values = object$nvalues,nreps = object$nreps))
  
  cat("\nConvergence (%):\n")
  percon <- (1 - object$nconfail / object$nreps) * 100
  names(percon) <- rownames(object$values)
  print(percon)

  cat("\nParameter Values:\n")
  pars <- object$parvalues
  names(pars) <- object$parnames
  print(pars)
  
  cat("\nPower:\n")        
  print(object$power)
  
  return (NULL)  
}