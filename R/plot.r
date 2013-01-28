  
plot.gsmcmc <- function (x) {
  if (!inherits (x, 'gsmcmc'))
    stop ('x should be of class gsmcmc')
  
  return (plot(as.mcmc.list(x)))
}

#' Plot JAGS analysis
#'
#' Produces trace plots for a JAGS analysis object
#' 
#' @param object a JAGS analysis object
#' @return NULL
#' @S3method plot janalysis
#' @export
#' @examples
#' model <- jmodel("model { bLambda ~ dunif(0,10) for (i in 1:nrow) { x[i]~dpois(bLambda) } }")
#' data <- data.frame(x = rpois(100,1))
#' analysis <- janalysis (model, data)
#' plot(analysis)

plot.janalysis <- function (x) {
  if (!inherits (x, 'janalysis'))
    stop ('x should be of class janalysis')
  
  return (plot(x$mcmc))
}