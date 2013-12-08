
plot.jagr_chains <- function (x, parm = "all", ...) {

  stopifnot(is.character(parm) && is_vector(parm))
  stopifnot(length(parm) > 0)
  
  parm <- expand_parm(x, parm = parm)
    
  mcmc <- as.mcmc.list (x)
  mcmc <- mcmc[,varnames(mcmc) %in% parm, drop = FALSE]
    
  return (plot(mcmc,...))
}

plot.jagr_analysis <- function (x, parm, ...) {  
  return (plot(as.jagr_chains(x), parm = parm, ...))
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

  x <- subset_jags(x, model_number = model_number)
  
  return (plot(analysis(x), parm = parm, ...))
}
