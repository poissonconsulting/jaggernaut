  
plot.gsmcmc <- function (x) {
  if (!inherits (x, 'gsmcmc'))
    stop ('x should be of class gsmcmc')
  
  return (plot(as.mcmc.list(x)))
}

plot.jagr_analysis <- function (x) {
  if (!is.jagr_analysis(x))
    stop ('x should be of class jagr_analysis')
  
  return (plot(x$mcmc))
}

#' @export
plot.janalysis <- function (x) {
  if (!is.janalysis(x))
    stop ('x should be of class janalysis')
  
  return (plot(top_model(x)))
}