
#' @title Perform a JAGS analysis
#'
#' @description 
#' Performs a JAGS analysis by fitting a 
#' \code{jags_model} or \code{jags_models} object 
#' to a data frame using JAGS (Plummer 2012). 
#' The resultant \code{jags_analysis} object can then be 
#' passed to other function to
#' to get the \code{convergence} of particular parameters, parameter \code{estimates} 
#' and \code{derived} parameter
#' estimates.
#' 
#' @param models a \code{jags_model} or \code{jags_model} object specifying the JAGS model(s).
#' @param data the data.frame to analyse.
#' @param n_iters an integer element of the number of iterations to run per MCMC chain.
#' @param debug a boolean element indicating whether or not to debug the model.
#' @details 
#' The \code{analysis} function performs a Bayesian analysis of a data frame
#' for a \code{jags_model} or \code{jags_models} object. 
#' If \code{debug = FALSE} (the default) then the analysis options are as currently
#' defined by \code{options_jaggernaut()} and each MCMC chain is run for \code{n.iter} 
#' iterations.  However if \code{debug = TRUE} then only two chains of 
#' 200 iterations are run on a single process and messages are not suppressed. 
#' This is to facilitate debugging of the analysis.
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
  models, data, n_iters = 10^3, debug = FALSE
)
{  
  n.iter <- n_iters
  n.chain <- opts_jagr0("n_chains")
  convergence <- opts_jagr0("convergence")
  resample <- opts_jagr0("n_resamples")
  quiet <- opts_jagr0("quiet")
  parallelChains <- opts_jagr0("parallel_chains")
  parallelModels <- opts_jagr0("parallel_models")
  
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
  
  if(debug || n.model == 1) {
    parallelModels <- FALSE
  }
  
  object <- list()
  object$analyses <- list()
  
  if(parallelModels) {
    
    doMC::registerDoMC(cores=n.model)
    
    object$analyses <- foreach(i = 1:n.model) %dopar% { 
      jagr_analysis(models[[i]], data, 
                    n.iter = n.iter, n.chain = n.chain, resample = resample,
                    convergence = convergence, independence = independence,
                    parallelChains = parallelChains,
                    debug = debug, quiet = quiet)
    }
  } else {
    for (i in 1:n.model) {
      if (!quiet)
        cat(paste("\n\nModel",i,"of",n.model,"\n\n"))
      object$analyses[[i]] <- jagr_analysis(models[[i]], data,
                                            n.iter = n.iter, n.chain = n.chain, resample = resample,
                                            convergence = convergence, independence = independence,
                                            parallelChains = parallelChains,
                                            debug = debug, quiet = quiet)
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
