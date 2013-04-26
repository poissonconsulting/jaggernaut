  
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
  
#  if (length(parameters) == 1 && parameters %in% c("all","fixed","random"))
#    parameters <- get_pars (x$model, type = parameters)
  
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
#' @param ... arguments to pass to coda::plot.mcmc
#' @seealso \code{\link{jags_analysis}} and \code{\link[coda]{plot.mcmc}}
#' @method plot jags_analysis
#' @export
plot.jags_analysis <- function (x, model_number = 1, ...) {

  if (!is.jags_analysis(x))
    stop ("x must be class jags_analysis")

  if (!is.numeric(model_number))
    stop ("model_number must be class numeric")
  
  if (!length(model_number) == 1)
    stop ("model_number must be length one")

#  if (!length(model_number) %in% 1:n)
#    stop ("model_number must be length one")
    
  parameters <- "all"
  
  x <- subset(x, model = model_number)
  
  return (plot.jagr_analysis(x$analyses[[1]], parameters = parameters, ...))
}
