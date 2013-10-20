
#' @title Perform a JAGS analysis
#'
#' @description 
#' Performs a JAGS analysis by fitting a 
#' \code{jags_model} or list of \code{jags_model}s  
#' to a data frame using JAGS (Plummer 2012). 
#' The resultant \code{jags_analysis} object can then be 
#' passed to other functions to
#' to get the \code{rhat} of particular parameters, parameter \code{coefficients}
#' with credible intervals and \code{predict} derived parameter
#' estimates.
#' 
#' @param model a \code{jags_model} specifying the JAGS model(s).
#' @param data the data.frame or list of data to analyse.
#' @param niters an integer element of the number of iterations to run per MCMC chain.
#' @param mode a character element indicating the mode for the analysis.
#' @details 
#' The \code{jags_analysis} function performs a Bayesian analysis of a data frame
#' for a \code{jags_model} or list of \code{jags_model}s. 
#' 
#' If \code{mode = "current"} (the default) then the analysis options are as currently
#' globally defined by \code{opts_jagr()} otherwise the \code{mode} argument specifies 
#' the analysis mode for that particular analysis. 
#' 
#' The \code{niters} argument specifies the total number of iterations including adaptive 
#' and burn in periods for each chain. The only exceptions are when the analysis is in 
#' debug mode in which case \code{niters} is set to be 100 or if \code{niters} is less
#' than \code{nsims * 2 / nchain} (where nsims is set by the mode) in which case 
#' \code{niters} is set to be \code{nsims * 2 / nchain} so that \code{nsims} can be 
#' drawn from the second halves of the chains.
#' @return a \code{jags_analysis} object
#' @references 
#' Plummer M (2012) JAGS Version 3.3.0 User Manual \url{http://sourceforge.net/projects/mcmc-jags/files/Manuals/}
#' @seealso \code{\link{jags_model}} 
#' and \code{\link{jaggernaut}} 
#' @examples
#' 
#' model <- jags_model("
#' model { 
#'  bLambda ~ dlnorm(0,10^-2) 
#'  for (i in 1:length(x)) { 
#'    x[i]~dpois(bLambda) 
#'  } 
#'}")
#'
#' data <- data.frame(x = rpois(100,1))
#' 
#' analysis <- jags_analysis (model, data, mode = "demo")
#' print(analysis)
#' 
#' analysis <- update(analysis, mode = "demo")
#' 
#' nchains(analysis)
#' nsims(analysis)
#' rhat(analysis)
#' 
#' @export
jags_analysis <- function (model, data, niters = 10^3, mode = "current") {

  if (!is.jags_model(model))
    stop("model must be a jags_model")
  
  if(is.numeric(niters)) {
    if(!is_scalar(niters))
      stop("niters must be a single value")  
    if(niters < 100 || niters > 10^6) 
      stop("niters must lie between 100 and 10^6")
  } else
    stop("niters must be numeric")
  
  check_modules()
  
  object <- list()
  
  class(object) <- "jags_analysis"
  
  data_jags(object) <- data
  
  niters <- as.integer(niters)
  
  if (mode != "current") {
    old_opts <- opts_jagr(mode = mode)
    on.exit(opts_jagr(old_opts))
  }
  
  quiet <- opts_jagr("quiet")
  
  if (quiet && options()$jags.pb != "none") {
    jags.pb <- options()$jags.pb
    options(jags.pb = "none")
    on.exit(options("jags.pb" = jags.pb), add = TRUE)
  }
      
  if (opts_jagr("mode") == "debug") {
    niters <- 100
  } 
  
  analyses <- list()
    
  analyses <- llply_jg(.data = models(model), .fun = jagr_analysis, 
                             data = data, niters = niters)
  
  analyses(object) <- analyses
  rhat_threshold(object) <- opts_jagr("rhat")
      
  return (object)
}

jags_analysis_datafirst <- function (data = data, model = model, niters = niters, 
                                  mode = "current") {
  return (jags_analysis(model = model, data = data, niters = niters, mode = mode))
}
