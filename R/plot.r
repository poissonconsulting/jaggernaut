
plot.jags_mcmc <- function (x, parm, ...) {

  stopifnot(is.jags_mcmc(x))
  stopifnot(is.character(parm))
  stopifnot(length(parm) > 0)
  
  mcmc <- as.mcmc.list (x)
  
  vars<-coda::varnames(mcmc)
  
  bol<-rep(F, length(vars))
  
  svars <- sapply(vars,strsplit,split="[",fixed=T)
  for (i in 1:length(vars)) {
    bol[i] <- svars[[i]][1] %in% parm
  }
  
  vars<-vars[bol]

  mcmc <- mcmc[,vars, drop = FALSE]
    
  return (plot.mcmc.list(mcmc,...))
}

plot.jagr_analysis <- function (x, parm, ...) {

  stopifnot(is.jagr_analysis(x))
  stopifnot(is.character(parm))
  stopifnot(length(parm) > 0)
  
  return (plot(x$mcmc, parm = parm, ...))
}

#' @title Plot a JAGS analysis
#'
#' @description 
#' Plots the MCMC samples from a JAGS analysis model 
#' using the coda::plot.mcmc function.
#'   
#' @param x a jags_analysis object
#' @param model_number an integer element specifying the model to select. 
#' If model_number = 0 then it selects the model with the lowest DIC.
#' @param parm a character vector of the parameters to plot.
#' @param ... arguments to pass to coda::plot.mcmc
#' @seealso \code{\link{jags_analysis}} and \code{\link[coda]{plot.mcmc}}
#' @method plot jags_analysis
#' @export
plot.jags_analysis <- function (x, model_number = 1, parm = "fixed", ...) {

  if (!is.jags_analysis(x))
    stop ("x must be class jags_analysis")
      
  x <- subset(x, model_number)
  
  parm <- get_parm(x, parm = parm)
    
  return (plot.jagr_analysis(as.jagr_analysis(x), parm = parm, ...))
}
