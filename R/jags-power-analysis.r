
#' @title Perform a JAGS simulation
#'
#' @description 
#' Performs a JAGS simulation by using a 
#' \code{jags_data_model} and a data frame of values  
#' to generate a simulation data frame using JAGS (Plummer 2012). 
#' 
#' @param data_model a \code{jags_data_model}.
#' @param nrep an integer element indicating the number of datasets to generate for each set of input values.
#' @param values a data.frame of input values.
#' @param mode a character element indicating the mode for the analysis.
#' @return a \code{jags_simulation} object
#' @references 
#' Plummer M (2012) JAGS Version 3.3.0 User Manual \url{http://sourceforge.net/projects/mcmc-jags/files/Manuals/}
#' @seealso \code{\link{jags_data_model}} and \code{\link{jaggernaut}}
#' @examples
#' 
#' data_model <- jags_data_model("
#' data { 
#'  for (i in 1:ny) { 
#'    y[i] ~ dpois(bIntercept) 
#'  } 
#'}    
#' ")
#' 
#' model <- jags_data_model("
#' model { 
#' bIntercept ~ dunif(0, 100)
#'  for (i in 1:ny) { 
#'    y[i] ~ dpois(bIntercept) 
#'  } 
#'}    
#' ")
#'
#' values <- data.frame(ny = c(10, 100), bIntercept = c(10,10))
#' 
#' # power <- jags_power (model, data_model, values = values, nrep = 100, mode = "demo")
#' @export
jags_power_analysis <- function (model, data_model, values, nrep = 100, niter = 10^3, mode = "current") {
  if (!is.jags_model(model)) 
    stop("model must be class jags_model")
  
  if (!is.jags_data_model(data_model)) 
    stop("data_model must be class jags_data_model")
  
  if(!is.data.frame(values))
    stop ("values must be a data frame")
  
  if(!is.data.frame(values))
    stop ("values must be a data frame")
  
  if(nrow(values) == 0)
    stop ("values must have at least one row of data")
  
  if(ncol(values) == 0)
    stop ("values must have at least one column of data")
  
  if(!is.numeric(nrep))
    stop("nrep must be class integer")
  
  if(!length(nrep) == 1)
    stop("nrep must be a single value")
  
  if(nrep < 1)
    stop("nrep must be positive")
  
  nrep <- as.integer(nrep)
  
  if(!"basemod" %in% list.modules())
    load.module("basemod")  
  
  if(!"bugs" %in% list.modules())
    load.module("bugs")
  
  old_opts <- opts_jagr(mode = mode)
  on.exit(opts_jagr(old_opts))
  
  if(opts_jagr("mode") == "debug") {
    nrep <- 1
  }
  nvalues <- nrow(values)
  
  sim <- jags_simulation(data_model = data_model, 
                         values = values, 
                         nrep = 1, mode = "explore")
  
  analyses <- list()
  convergence <- matrix(nvalues, 1)
  
  for (value in 1:nvalues) {
    analyses[[value]] <- list()
    rep <- 1
    if (!opts_jagr("quiet"))
      print(paste0("Value: ",value," of ",nvalues,"  Rep: ", rep," of ",nrep))
    
    an <- jags_analysis(models = model, 
                        data = data_jags(sim, rep = rep, value = value),
                        niter = niter)
    
    analyses[[value]][[rep]] <- an
    
    convergence[value,rep] <- convergence(an)$convergence
  }
  
  object <- list(
    model = model,
    data_model = data_model, 
    values = values,
    nvalues = nrow(values),
    nrep = 1,
    simulated = sim,
    niter = niter,
    analyses = analyses,
    convergence = convergence)

#  for (rep in 2:nrep) {
#    object <- update_jags(object, nrep = 1)
#  }
  #   
  #   object <- calc_power(object, pars = pars)
  # 
  
  class(object) <- "jags_power"

  return (object)
}
