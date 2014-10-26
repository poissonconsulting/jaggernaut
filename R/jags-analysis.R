#' @title Perform a JAGS analysis
#'
#' @description 
#' Performs a JAGS analysis by fitting a 
#' \code{jags_model} or list of \code{jags_model}s  
#' to a data frame using JAGS (Plummer 2012). 
#' The resultant \code{jags_analysis} object can then be 
#' passed to other functions to
#' to get the \code{convergence} of particular parameters, parameter \code{coefficients}
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
#' than \code{nsamples * 2 / nchain} (where nsamples is set by the mode) in which case 
#' \code{niters} is set to be \code{nsamples * 2 / nchain} so that \code{nsamples} can be 
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
#' auto_corr(analysis)
#' cross_corr(analysis)
#' nchains(analysis)
#' nsamples(analysis)
#' convergence(analysis)
#' 
#' @export
jags_analysis <- function (model, data, niters = 10^3, mode = "current") {

  assert_that(is.jags_model(model))
  assert_that(is_convertible_data(data))
  assert_that(is.count(niters))
  assert_that(niters >= 100 && niters <= 10^6)
  assert_that(is.string(mode))
  
  if (options()$jags.pb != "none") {
    jags.pb <- options()$jags.pb
    options(jags.pb = "none")
    on.exit(options("jags.pb" = jags.pb), add = TRUE)
  }
  
  if (mode != "current") {
    old_opts <- opts_jagr(mode = mode)
    on.exit(opts_jagr(old_opts))
  }
  
  check_modules()
    
  nworkers <- getDoParWorkers()
  
  if(!opts_jagr("parallel") || opts_jagr("mode") == "debug")
    nworkers <- 1
  
  if (opts_jagr("mode") == "debug")
    niters <- 100
    
  object <- list()
  class(object) <- "jags_analysis"
  dataset(object) <- data
  
  data <- dataset(object)
  models <- models(model)
  nmodels <- nmodels(model)
  nchains <- opts_jagr("nchains")

  chunks <- floor(nworkers / nchains)
  chunks <- min(nmodels, chunks)
  if (chunks <= 1) {
    analyses <- jagr_analysis_list(models, data = data, niters = niters, 
                                   nworkers = nworkers)
  } else { 
    i <- NULL
    
    fun <- function (x1, x2) {
      n1 <- length(x1)
      n2 <- length(x2)
      x <- list()
      for (i in 1:n1)
        x[[i]] <- x1[[i]]
      for (i in 1:n2)
        x[[i + n1]] <- x2[[i]]
      return (x)
    }
    
    analyses <- foreach(i = isplitIndices(n = nmodels, 
                                          chunks = chunks),
                        .combine = fun, 
                        .export = "jagr_analysis_list") %dopar% {
      jagr_analysis_list(models[i], data = data, niters = niters, 
                         nworkers = nchains)
    }
  }
  
  analyses(object) <- analyses
  convergence_threshold(object) <- opts_jagr("convergence")
      
  return (object)
}

jags_analysis_datafirst <- function (data = data, model = model, niters = niters, 
                                  mode = "current") {
  return (jags_analysis(model = model, data = data, niters = niters, mode = mode))
}
