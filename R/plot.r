  
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
#' Plots a JAGS analysis model using the coda::plot.mcmc function.
#'   
#' @param x a janalysis object
#' #' @param model an integer vector specifying the model to select. 
#' If model = 0 then it selects the model with the lowest DIC.
#' @param ... arguments to pass to coda::plot.mcmc
#' @seealso \code{\link{analysis}}, \code{\link[coda]{plot.mcmc}}
#' @examples
#' mod <- model("model { bLambda ~ dunif(0,10) for (i in 1:nrow) { x[i]~dpois(bLambda) } }")
#' dat <- data.frame(x = rpois(100,1))
#' ana <- analysis (mod, dat)
#' plot(ana)
#' @method plot janalysis
#' @export
plot.janalysis <- function (x, model = 1, ...) {
  if (!is.janalysis(x))
    stop ('x should be of class janalysis')
  
  parameters <- "all"
  
  x <- subset(x, model = model)
  
  return (plot.jagr_analysis(x$analyses[[1]], parameters = parameters, ...))
}
