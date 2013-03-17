

#' Perform JAGS analysis
#'
#' Performs JAGS analysis
#' 
#' @param model a jmodel object specifying the JAGS model
#' @param data a data.frame specifying the data to analyse
#' @param n.iter the number of iterations in each mcmc chain
#' @param n.chain the number of mcmc chains
#' @param resample the number of times to resample 
#' until convergence and independence are achieved
#' @param convergence the threshold for convergence
#' @param independence the threshold for indepedence
#' @param parallelChains a boolean indicating whether the chains should
#' be run on separate processes
#' @param parallelModels a boolean indicating whether the models should
#' be run on separate processes
#' @param debug a boolean indicating whether or not the intent is to debug the model
#' @param quiet a boolean indicating whether or not to suppress messages
#' @return a JAGS analysis object
#' @export
#' @examples
#' model <- jmodel("model { bLambda ~ dunif(0,10) for (i in 1:nrow) { x[i]~dpois(bLambda) } }")
#' data <- data.frame(x = rpois(100,1))
#' analysis <- janalysis (model, data)
janalysis <- function (
  models, data, n.iter = 1000, n.chain = 3, resample = 3,
  convergence = 1.1, independence = 0,
  parallelChains = .Platform$OS.type != "windows",
  parallelModels = .Platform$OS.type != "windows",
  debug = FALSE, quiet = FALSE
)
{  
  
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

  object$dic <- t(sapply(object$analyses,dic))
  rownames(object$dic) <- paste0("Model",1:nrow(object$dic))
  names(object$analyses) <- rownames(object$dic)
  
  object$dic <- object$dic[order(object$dic[,"DIC",drop=T]),]
  object$n.model <- n.model
  
  class(object) <- "janalysis"
  
  return (object)
}




