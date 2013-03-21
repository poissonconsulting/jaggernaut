  
plot.gsmcmc <- function (x, parameters = parameters, ...) {
  if (!inherits (x, 'gsmcmc'))
    stop ('x should be of class gsmcmc')
  
  warning("need to subset parameters that are being plotted")
  
  return (plot(as.mcmc.list(x)), ...)
}

plot.jagr_analysis <- function (x, parameters, ...) {
  if (!is.jagr_analysis(x))
    stop ('x should be of class jagr_analysis')
  
  return (plot(x$mcmc, parameters = parameters, ...))
}

#' @title Plot a JAGS analysis (janalysis) object
#'
#' @description 
#' Plots a JAGS analysis (janalysis) object
#'   
#' @param x a JAGS analysis (janalysis) object
#' @export
plot.janalysis <- function (x, parameters = "all", model = "min", ...) {
  if (!is.janalysis(x))
    stop ('x should be of class janalysis')
  
  if(!model %in% c("min"))
    stop("model should be min")
  
  return (plot(top_model(x), parameters = parameters, ...))
}