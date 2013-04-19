
summary.mcarray <- function (object, ...) {
  dim <- dim (object)
  print(dim)
  
  return (NULL)
}

summary.gsmcmc <- function (object, ...) {
  cat("\nDimensions:\n")
  dim <- c(chains = nchain(object), simulations = nsim(object))
  print(dim)
  
  for (i in seq_along(object$mcmc)) {
    print(names(object$mcmc)[i])
    summary(object$mcmc[[i]])
  }
  return (NULL)  
}

summary.jagr_analysis <- function (object, ...)
{
  summ <- list()
  
  summ[["Generation"]] <- c(iterations = object$iterations,time = round(object$time,2))
  
  summ[["Dimensions"]] <- c(simulations = nsim(object),chains = nchain(object))

  summ[["Convergence"]] <- calc_convergence(object)

  summ[["Estimates"]] <- calc_estimates(object)

  summ[["Deviance Information Criterion"]] <- DIC(object)
  
  class (summ) <- "summary_jagr_analysis"
  
  return (summ)  
}


#' @title Summary of JAGS analysis
#'
#' @description 
#' Produces a summary of a JAGS analysis
#'   
#' @param object a janalysis object to summarise
#' @param ... further arguments to pass to or from other methods.
#' @return a jsummary_analysis object
#' @seealso \code{\link{analysis}}, \code{\link{tits}}
#' @export
#' @method summary janalysis
summary.janalysis <- function (object, ...)
{  
  summ <- list()
  
  n <- length(object$analyses)
  
  for (i in 1:n) {
    summ[[paste0("Model",i)]] <- summary(object$analyses[[i]])
  }
  summ[["Model Comparison"]] <- object$dic
  
  class (summ) <- "summary_janalysis"
  
  return (summ)  
}


summary.gspower <- function (object, ...)
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
