
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
#' @param model a count or string specifying the jags model to select. 
#' @param parm a character vector of the parameters to plot.
#' @param ... arguments to pass to coda::plot.mcmc
#' @seealso \code{\link{jags_analysis}} and \code{\link[coda]{plot.mcmc}}
#' @method plot jags_analysis
#' @export
plot.jags_analysis <- function (x, model = 1, parm = "fixed", ...) {

  assert_that(is.count(model) || is.string(model))
  
  x <- subset(x, model = model)
  
  return (plot(analysis(x), parm = parm, ...))
}

#' @title Plot a JAGS discrepancies object
#'
#' @description 
#' Plots a JAGS discrepancies object
#'   
#' @param x a jags_discrepancies object
#' @param ... additional arguments
#' @importFrom ggplot2 ggplot aes_string geom_point geom_abline facet_wrap expand_limits
#' @method plot jags_discrepancies
#' @export
plot.jags_discrepancies <- function (x, ...) {
  x <- dataset(x)
  
  gp <- ggplot(x, aes_string(x = "Actual", y = "Replicate"))
  gp <- gp + facet_wrap(~parameter, scales = "free")
  gp <- gp + geom_abline(intercept = 0, slope = 1)
  gp <- gp + geom_point()
  return(gp)
}
