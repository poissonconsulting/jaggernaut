
janalysis <- function (
  models, data, n.iter = 10^3, n.chain = 3, resample = 3,
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
  
  class(object) <- "jags_analysis"
  
  return (object)
}
