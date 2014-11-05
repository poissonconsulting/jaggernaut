
plot.jagr_chains <- function (x, parm = "all", ...) {

  stopifnot(is.character(parm) && is_vector(parm))
  stopifnot(length(parm) > 0)
    
  mcmc <- as.mcmc.list (x)
  mcmc <- mcmc[,varnames(mcmc) %in% parm, drop = FALSE]
    
  return (plot(mcmc,...))
}

plot.jagr_analysis <- function (x, parm, ...) {  
  
  parm <- expand_parm(x, parm = parm)
  
  return (plot(as.jagr_chains(x), parm = parm, ...))
}

#' @title Plot a JAGS analysis
#'
#' @description 
#' Plots the MCMC samples from a JAGS analysis model 
#' using the coda::plot.mcmc function.
#'   
#' @param x a jags_analysis object
#' @param model_id a count or string specifying the jags model to select. 
#' @param parm a character vector of the parameters to plot.
#' @param ... arguments to pass to coda::plot.mcmc
#' @seealso \code{\link{jags_analysis}} and \code{\link[coda]{plot.mcmc}}
#' @method plot jags_analysis
#' @export
plot.jags_analysis <- function (x, model_id = default_model_id(x), parm = "fixed", ...) {

  assert_that(is.scalar(model))
  
  x <- subset(x, model_id = model_id)
  
  return (plot(analysis(x), parm = parm, ...))
}
