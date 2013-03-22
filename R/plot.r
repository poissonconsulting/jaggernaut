  
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
#' Plots the JAGS analysis model with the lowest DIC using the coda::plot.mcmc function.
#'   
#' @param x a janalysis object
#' @param parameters a character vector of the parameters to plot or "all", "fixed" or "random".
#' Not yet implemented.
#' @param ... arguments to pass to coda::plot.mcmc
#' @seealso \code{\link{jmodel}}, \code{\link{janalysis}}, \code{\link[coda]{plot.mcmc}}
#' @examples
#' model <- jmodel("model { bLambda ~ dunif(0,10) for (i in 1:nrow) { x[i]~dpois(bLambda) } }")
#' data <- data.frame(x = rpois(100,1))
#' analysis <- janalysis (model, data)
#' plot(analysis)
#' @export
plot.janalysis <- function (x, parameters = "all", ...) {
  if (!is.janalysis(x))
    stop ('x should be of class janalysis')
  
  if (parameters != "all")
    warning("parameters argument not yet implemented")
  
  parameters <- "all"
  
  return (plot(top_model(x), parameters = parameters, ...))
}