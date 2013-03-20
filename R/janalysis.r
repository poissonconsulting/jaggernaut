
#' @title Perform a janalysis (JAGS analysis)
#'
#' @description 
#' Performs a janalysis (JAGS analysis) by applying one or more jmodels (JAGS models)
#' to a data frame. The remaining parameters control the number of iterations, 
#' the number of chains, the required convergence, whether the models and/or chains are run in parallel
#' and the output messages. The janalysis object can then be queried using other functions
#' to get parameter estimates, derived values and Deviance Information Criterion (DIC)
#' values.
#' 
#' @details 
#' The janalysis function performs a Bayesian analysis of a data frame using
#' one or more jmodels (JAGS models). For ease of use only the jmodel(s) and data
#' frame need to be defined. However, the number of iterations and the number of
#' chains can be specified as can the number of times the model should be resampled
#' until convergence is achieved. 
#' 
#' 1000 chains drawn from the second halfs of the chains
#' 
#' Convergence is achieved when all the monitored
#' parmeters have an R-hat less than the value of the convergence argument which
#' by default is 1.1. If the initial number of iterations are performed without
#' achieving convergence and resample > 0 then just convergence doubled and a 
#' 1000 iterations.
#' 
#' If debug = TRUE then only two chains of 200 iterations are run on a single
#' process and messages are not suppressed. This is to facilitate debugging of the
#' JAGS model definition, i.e., the analysis part is as quick as possible and
#' all messages are provided.
#' 
#' @param models a jmodel object or list of jmodel objects specifying the JAGS model
#' @param data a data.frame specifying the data to analyse
#' @param n.iter the number of iterations in each mcmc chain
#' @param n.chain the number of mcmc chains
#' @param resample the number of times to resample 
#' until convergence is are achieved
#' @param convergence the threshold for convergence
#' @param parallelChains a boolean indicating whether the chains should
#' be run on separate processes (currently only available for unix-based systems)
#' @param parallelModels a boolean indicating whether the models should
#' be run on separate processes (currently only available for unix-based systems)
#' @param debug a boolean indicating whether or not to debug the model
#' @param quiet a boolean indicating whether or not to suppress messages
#' @return a janalysis (JAGS analysis) object
#' @export
#' @examples
#' model <- jmodel("model { bLambda ~ dunif(0,10) for (i in 1:nrow) { x[i]~dpois(bLambda) } }")
#' data <- data.frame(x = rpois(100,1))
#' analysis <- janalysis (model, data)
janalysis <- function (
  models, data, n.iter = 1000, n.chain = 3, resample = 3,
  convergence = 1.1,
  parallelChains = .Platform$OS.type != "windows",
  parallelModels = .Platform$OS.type != "windows",
  debug = FALSE, quiet = FALSE
)
{  
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
