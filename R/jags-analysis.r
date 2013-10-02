
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
#' @param niter an integer element of the number of iterations to run per MCMC chain.
#' @param mode a character element indicating the mode for the analysis.
#' @details 
#' The \code{jags_analysis} function performs a Bayesian analysis of a data frame
#' for a \code{jags_model} or list of \code{jags_model}s. 
#' 
#' If \code{mode = "current"} (the default) then the analysis options are as currently
#' globally defined by \code{opts_jagr()} otherwise the \code{mode} argument specifies 
#' the analysis mode for that particular analysis. 
#' 
#' The \code{niter} argument specifies the total number of iterations including adaptive 
#' and burn in periods for each chain. The only exceptions are when the analysis is in 
#' debug mode in which case \code{niter} is set to be 100 or if \code{niter} is less
#' than \code{nsims * 2 / nchain} (where nsims is set by the mode) in which case 
#' \code{niter} is set to be \code{nsims * 2 / nchain} so that \code{nsims} can be 
#' drawn from the second halves of the chains.
#' @return a \code{jags_analysis} object
#' @references 
#' Plummer M (2012) JAGS Version 3.3.0 User Manual \url{http://sourceforge.net/projects/mcmc-jags/files/Manuals/}
#' @seealso \code{\link{jags_model}}, \code{\link{opts_jagr}},
#' \code{\link{rhat}}, \code{\link{coef.jags_analysis}}, 
#' \code{\link{predict.jags_analysis}} 
#' and \code{\link{jaggernaut}}
#' @examples
#' 
#' model <- jags_model("
#' model { 
#'  bLambda ~ dlnorm(0,10^-2) 
#'  for (i in 1:nrow) { 
#'    x[i]~dpois(bLambda) 
#'  } 
#'}")
#'
#' data <- data.frame(x = rpois(100,1))
#' 
#' analysis <- jags_analysis (model, data, mode = "demo")
#' 
#' analysis <- update_jags(analysis, mode = "demo")
#' 
#' @export
jags_analysis <- function (
  model, data, niter = 10^3, mode = "current"
) {

  if (!is.jags_model(model))
    stop("model must be a jags_model")
  
  models <- model
  rm(model)
  
  if (!is.data.frame (data)) {
    if (!is.list(data)) {
      stop("data must be a data.frame or a data list")
    }
    names <- names(data)
    if(is.null(names)) {
      stop("variables in data must be named")
    }
    classes <- c("logical","integer","numeric","factor",
                 "Date","POSIXt","matrix","array")
      bol <- sapply(data, inherits,classes[1])
    for (class in classes[-1]) {
      bol <- bol | sapply(data,inherits,class)
    }
    if (!all(bol)) {
      stop(paste("variables in data list must be class",classes))
    }
    if (!is_data_list (data)) {
      stop("data must be a data.frame or a data list")
    }
  } else {
    if (!nrow(data)) {
      stop("data must include at least one row")
    }
    if (!ncol(data)) {
      stop("data must include at least one column")
    }
  }

  if (is.numeric(niter)) {
    if (length(niter) != 1) {
      stop("niter must be length one")  
    }
    if(niter < 100 || niter > 10^6) {
      stop("niter must lie between 100 and 10^6")
    }
  } else {
    stop("niter must be numeric")
  }
  
  niter <- as.integer(niter)
    
  old_opts <- opts_jagr(mode = mode)
  on.exit(opts_jagr(old_opts))
    
  nchains <- opts_jagr("nchains")
  nsims <- opts_jagr("nsims")
  rhat <- opts_jagr("rhat")
  resample <- opts_jagr("nresample")
  quiet <- opts_jagr("quiet")
  parallelChains <- opts_jagr("parallel_chains")
  parallelModels <- opts_jagr("parallel_models")
  
  if (opts_jagr("mode") == "debug") {
    niter <- 100
  } 
  
  niter <- max(niter, nsims * 2 / nchains)
  
  n.model <- number_of_models(models)
  
  if(n.model == 1) {
    parallelModels <- FALSE
  }

  if(!"basemod" %in% list.modules())
    load.module("basemod")  
  
  if(!"bugs" %in% list.modules())
    load.module("bugs")
  
  if(!"dic" %in% list.modules())
    load.module("dic")
  
  analyses <- list()
  
  if(parallelModels) {
    
    doMC::registerDoMC(cores=n.model)
    
    analyses <- foreach::foreach(i = 1:n.model) %dopar% { 
      jagr_analysis(subset_jags(models,i), data, 
                    n.iter = niter, n.chain = nchains, resample = resample,
                    rhat = rhat,
                    parallelChains = parallelChains,
                    quiet = quiet, n.sim = nsims)
    }
  } else {
    for (i in 1:n.model) {
      if (!quiet)
        cat(paste("\n\nModel",i,"of",n.model,"\n\n"))
      analyses[[i]] <- jagr_analysis(subset_jags(models,i),
                                     data,n.iter = niter, n.chain = nchains,
                                     resample = resample, 
                                     rhat = rhat,
                                     parallelChains = parallelChains,
                                    quiet = quiet, n.sim = nsims)
    }
  }
  
  dic <- t(sapply(analyses,DIC_jagr_analysis))
  rownames(dic) <- paste0("Model",1:nrow(dic))
  
  dic <- dic[order(dic[,"DIC",drop=T]),]
  
  object <- list(data = data,
                analyses = analyses,
                rhat = rhat,
                dic = dic)
  
  class(object) <- "jags_analysis"
  
  object$derived_code <- models$derived_code
  object$random_effects <- models$random_effects
  
  return (object)
}
