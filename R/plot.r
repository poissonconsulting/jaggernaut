  
plot.gsmcmc <- function (x, parameters = parameters, ...) {
  if (!inherits (x, 'gsmcmc'))
    stop ('x should be of class gsmcmc')
  
  if (parameters != "all")
    warning("parameters argument not yet implemented")
  
  return (coda:::plot.mcmc.list(as.mcmc.list(x),...))
}

plot.jagr_analysis <- function (x, parameters, ...) {

  if (!is.jagr_analysis(x))
    stop ('x should be of class jagr_analysis')
  
  return (plot(x$mcmc, parameters = parameters, ...))
}

#' @title Plot a JAGS analysis
#'
#' @description 
#' Plots the MCMC samples from a JAGS analysis model 
#' using the coda::plot.mcmc function.
#'   
#' @param x a jags_analysis object
#' @param model_number an integer element specifying the model to select. 
#' If model_number = 0 then it selects the model with the lowest DIC.
#' @param parm a character vector of the parameters to plot.
#' @param ... arguments to pass to coda::plot.mcmc
#' @seealso \code{\link{jags_analysis}} and \code{\link[coda]{plot.mcmc}}
#' @method plot jags_analysis
#' @export
plot.jags_analysis <- function (x, model_number = 1, parm = "fixed", ...) {

  if (!is.jags_analysis(x))
    stop ("x must be class jags_analysis")
      
  x <- subset(x, model_number)
  
  parm <- parm(x, parm = parm)
    
  return (plot.jagr_analysis(x$analyses[[1]], parameters = parm, ...))
}
