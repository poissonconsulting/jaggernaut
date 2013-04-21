
#' @title Perform a JAGS analysis
#'
#' @description 
#' Performs a JAGS analysis by fitting a 
#' \code{jags_model} or list of \code{jags_model}s  
#' to a data frame using JAGS (Plummer 2012). 
#' The resultant \code{jags_analysis} object can then be 
#' passed to other functions to
#' to get the \code{convergence} of particular parameters, parameter \code{estimates} 
#' and \code{derived} parameter
#' estimates.
#' 
#' @param models a \code{jags_model} or list of \code{jags_model}s  specifying the JAGS model(s).
#' @param data the data.frame to analyse.
#' @param niter an integer element of the number of iterations to run per MCMC chain.
#' @param mode a character element indicating the mode for the analysis.
#' @details 
#' The \code{analysis} function performs a Bayesian analysis of a data frame
#' for a \code{jags_model} or list of \code{jags_model}s. 
#' If \code{mode = "current"} (the default) then the analysis options are as currently
#' globally defined by \code{opts_jagr0()} otherwise the \code{mode} argument specifies 
#' the analysis mode for that particular analysis. 
#' 
#' The \code{niter} argument specifies the total number of iterations including adaptive 
#' and burn in periods for each chain. The only exceptions are when the analysis is in 
#' debug mode in which case \code{niter} is set to be 100 or if \code{niter} is less
#' than \code{nsims * 2 / nchain} in which case 
#' \code{niter} is set to be \code{nsims * 2 / nchain} so that \code{nsims} can be 
#' drawn from the second halves of the chains.
#' @return a \code{jags_analysis} object
#' @references 
#' Plummer M (2012) JAGS Version 3.3.0 User Manual \url{http://sourceforge.net/projects/mcmc-jags/files/Manuals/}
#' @seealso \code{\link{model}}, \code{\link{opts_jagr0}},
#' \code{\link{convergence}}, \code{\link{estimates}}, \code{\link{derived}} 
#' and \code{\link{jaggernaut}}
#' @examples
#' 
#' mod <- model("
#' model { 
#'  bLambda ~ dlnorm(0,10^-2) 
#'  for (i in 1:nrow) { 
#'    x[i]~dpois(bLambda) 
#'  } 
#'}")
#'
#' dat <- data.frame(x = rpois(100,1))
#' 
#' an <- analysis (mod, dat)
#' 
#' @export
#' @aliases jags_analysis
analysis <- function (
  models, data, niter = 10^3, mode = "current"
)
{ 
  old_opts <- opts_jagr0(mode = mode)
  on.exit(opts_jagr0(old_opts))
    
  nchains <- opts_jagr0("nchains")
  nsims <- opts_jagr0("nsims")
  convergence <- opts_jagr0("rhat")
  resample <- opts_jagr0("nresample")
  quiet <- opts_jagr0("quiet")
  parallelChains <- opts_jagr0("parallel_chains")
  parallelModels <- opts_jagr0("parallel_models")
  mode <- opts_jagr0("mode")
  
  niter <- max(niter, nsims * 2 / nchains)
  
  if (mode == "debug") {
    niter <- 100
  } 
  
  independence <- 0
  if(!"basemod" %in% list.modules())
    load.module("basemod")  
  
  if(!"bugs" %in% list.modules())
    load.module("bugs")
  
  if(!"dic" %in% list.modules())
    load.module("dic")
  
  if(!is.list(models) & !is.jmodel(models))
    stop ("models should be a jmodel object or list of jmodel")
  
  if(parallelModels && .Platform$OS.type == "windows") {
    warning("parallelModels is not currently defined for windows")
    parallelModels <- FALSE
  }
  
  if(is.jmodel(models)) {
    models <- list(models)
  }
  
  n.model <- length(models)
  
  if(n.model == 1) {
    parallelModels <- FALSE
  }
  
  object <- list()
  object$analyses <- list()
  
  if(parallelModels) {
    
    doMC::registerDoMC(cores=n.model)
    
    object$analyses <- foreach(i = 1:n.model) %dopar% { 
      jagr_analysis(models[[i]], data, 
                    n.iter = niter, n.chain = nchains, resample = resample,
                    convergence = convergence, independence = independence,
                    parallelChains = parallelChains,
                    debug = FALSE, quiet = quiet, n.sim = nsims)
    }
  } else {
    for (i in 1:n.model) {
      if (!quiet)
        cat(paste("\n\nModel",i,"of",n.model,"\n\n"))
      object$analyses[[i]] <- jagr_analysis(models[[i]], data,
                                            n.iter = niter, n.chain = nchains, resample = resample,
                                            convergence = convergence, independence = independence,
                                            parallelChains = parallelChains,
                                            debug = FALSE, quiet = quiet, n.sim = nsims)
    }
  }
  
  object$dic <- t(sapply(object$analyses,DIC_jagr_analysis))
  rownames(object$dic) <- paste0("Model",1:nrow(object$dic))
  names(object$analyses) <- rownames(object$dic)
  
  object$dic <- object$dic[order(object$dic[,"DIC",drop=T]),]
  object$n.model <- n.model
  
  class(object) <- "janalysis"
  
  return (object)
}