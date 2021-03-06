#' @title Bayesian Analysis with JAGS
#'
#' @description 
#' An R package to facilitate Bayesian analysis
#' using JAGS (Just Another Gibbs Sampler).
#' 
#' @details
#' In short an analysis proceeds first by the 
#' definition of the JAGS model in BUGS code using
#' the \code{\link{jags_model}} function. Multiple models can be
#' combined in a single \code{jags_model} object using \code{\link{combine}}.
#' Next the resultant \code{jags_model} object is passed together with 
#' a data set to 
#' the \code{\link{jags_analysis}} function which calls the JAGS software to 
#' perform the actual MCMC
#' sampling.  The resultant \code{jags_analysis} object can then be passed
#' to the \code{\link{plot.jags_analysis}} function to view the MCMC traces, the 
#' \code{\link{convergence}} function to check the Rhat values of individual parameters
#' and the \code{\link{coef.jags_analysis}} function to get the parameter estimates
#' with  credible limits.  The \code{\link{predict.jags_analysis}} function can
#' then be used to extract derived parameter estimates with credible intervals
#' from a \code{jags_analysis} object without the need for further
#' MCMC sampling.
#' 
#' 
#' Options are queried and set using the \code{\link{opts_jagr}} function.
#' 
#' @references 
#' Plummer M (2012) JAGS Version 3.3.0 User Manual \url{http://sourceforge.net/projects/mcmc-jags/files/Manuals/}
#' 
#' @docType package
#' @import assertthat coda rjags doParallel itertools datacheckr
#' @importFrom foreach getDoParWorkers foreach %dopar%
#' @importFrom magrittr %<>% %>%
#' @importFrom graphics par plot
#' @importFrom stats coef median predict update variable.names
#' @importFrom utils capture.output head
#' @name jaggernaut
#' @aliases package-jaggernaut jaggernaut-package
#' @seealso \code{\link{jags_model}},
#' \code{\link{jags_analysis}}
#' and \code{\link{opts_jagr}}.
#' @examples
#' 
#' mod <- jags_model("
#' model { 
#'  bLambda ~ dlnorm(0,10^-2) # $\\lambda$
#'  for (i in 1:length(x)) { 
#'    x[i]~dpois(bLambda) 
#'  } 
#'}")
#'
#' dat <- data.frame(x = rpois(100,1))
#' 
#' an <- jags_analysis (mod, dat, mode = "demo")
#' 
#' plot(an)
#' convergence(an)
#' coef(an)
#' coef(an, latex = TRUE)
#' summary(an)
#'
NULL
