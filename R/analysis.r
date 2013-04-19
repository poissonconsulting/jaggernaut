
#' @title Perform a JAGS analysis
#'
#' @description 
#' Performs a JAGS analysis (\code{janalysis} object) by fitting a 
#' \code{jmodel} object or list of \code{jmodel}
#' objects to a data frame using JAGS (Plummer 2012). 
#' The remaining parameters control 
#' the number of iterations, the required convergence, 
#' whether the models and/or chains are run in parallel
#' and the output messages. The resultant \code{janalysis} object can then be 
#' queried using other functions
#' to get the \code{\link{convergence}} of particular parameters, parameter \code{\link{estimates}} 
#' and \code{\link{derived}} parameter
#' estimates.
#' 
#' @param models a \code{jmodel} object or list of \code{jmodel} objects specifying the JAGS model.
#' @param data a data.frame specifying the data to analyse.
#' @param n.iter an integer element of the number of iterations per MCMC chain.
#' @param resample an integer element of the number of times to resample 
#' until convergence is achieved.
#' @param convergence a numeric element of the R-hat threshold for convergence.
#' @param debug a boolean element indicating whether or not to debug the model
#' @param quiet a boolean element indicating whether or not to suppress messages
#' @param parallelChains a boolean element indicating whether the chains should
#' be run on separate processes (currently only available for unix-based systems)
#' @param parallelModels a boolean element indicating whether the models should
#' be run on separate processes (currently only available for unix-based systems)
#' @details 
#' The \code{analysis} function performs a Bayesian analysis of a data frame with
#' for a \code{jmodel} object or list of \code{jmodel} objects using JAGS. 
#' For ease of use only the model and data
#' frame need to be defined. However, the number of iterations can be specified as can the number of times the model should be resampled
#' until convergence is considered to have been achieved. 
#' 
#' The JAGS analysis retains 1,000 MCMC samples from the second halves of 
#' three MCMC chains.
#' For example if and \code{n.iter = 1000})
#' then 333 samples will be thinned
#' from the last 500 iterations of each chain.  
#'  
#' Convergence is considered to have been achieved when all the monitored
#' parameters have an R-hat less than the value of the \code{convergence} argument which
#' by default is 1.1 (Kery & Schaub 2011). If the initial number of iterations (\code{n.iter})
#' are performed and the convergence target has not been achieved and \code{resample} is greater than 0
#' then the value of \code{n.iter} is doubled, the MCMC sampling to date is 
#' considered the burn in period, the saved MCMC samples are discarded and 
#' MCMC sampling continues.  This process is continued until the convergence target is 
#' achieved or resampling exceed the value of the \code{resample} argument.
#' 
#' If \code{debug = TRUE} then only two chains of 200 iterations are run on a single
#' process and messages are not suppressed. This is to facilitate debugging of the
#' JAGS model definition, i.e., the analysis part is as quick as possible and
#' all messages are provided.
#' @return a \code{janalysis} (JAGS analysis) object
#' @aliases janalysis
#' @seealso \code{\link{jaggernaut}}, \code{\link{model}}, \code{\link{convergence}}, \code{\link{estimates}}, \code{\link{derived}} 
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
#' plot(an)
#' convergence(an)
#' estimates(an)
#' summary(an)
#' 
#'@references 
#' Kery M & Schaub M (2011) Bayesian Population Analysis
#' using WinBUGS. Academic Press. (\url{http://www.vogelwarte.ch/bpa})
#' 
#' Plummer M (2012) JAGS Version 3.3.0 User Manual \url{http://sourceforge.net/projects/mcmc-jags/files/Manuals/}
#' @export
analysis <- function (
  models, data, n.iter = 10^3, resample = 3, convergence = 1.1, debug = FALSE, 
  quiet = FALSE, parallelChains = .Platform$OS.type!="windows", 
  parallelModels = FALSE
)
{  
  n.chain <- 3
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
