
#' @title Perform a JAGS power analysis
#'
#' @description 
#' Performs a JAGS power analysis. 
#' 
#' @param model a \code{jags_model}.
#' @param data_model a \code{jags_data_model}.
#' @param values a data.frame of input values.
#' @param nreps an integer element indicating the number of datasets to generate for each set of input values.
#' @param niters an integer element indicating the number of iterations.
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
#' model <- jags_model("
#' model { 
#' bIntercept ~ dunif(0, 100)
#'  for (i in 1:length(y)) { 
#'    y[i] ~ dpois(bIntercept) 
#'  } 
#'} ",
#'select = c("y"))
#'
#' values <- data.frame(ny = c(10, 100), bIntercept = c(10,10))
#' 
#' power <- jags_power_analysis (model = model, data_model = data_model, 
#'                              values = values, nreps = 10, mode = "demo")
#' nvalues(power)
#' nreps(power)
#' nchains(power)
#' niters(power)
#' nsims(power)
#' rhat(power)
#' is_converged(power, percent = TRUE)
#' 
#' @export
jags_power_analysis <- function (model, data_model, values, nreps = 100, 
                                 niters = 10^3, parm = c(fixed = 0), mode = "current") {
  
  if(!is.jags_model(model) && !is_one_model(model))
    stop("model must be a jags_model with a single model")
    
  old_opts <- opts_jagr(mode = mode)
  on.exit(opts_jagr(old_opts))
  
  if(opts_jagr("mode") == "debug")
    nreps <- 1
  
  quiet <- opts_jagr("quiet")
  
  if (!quiet)
    cat("\ngenerating data\n")
  
  object <- jags_simulation(data_model, values = values, nreps = nreps)

  if (!quiet)
    cat("\nanalysing data\n")
  
  analyses <- list()
  
  for (value in 1:nvalues(object)) {
    analyses[[value]] <- list()
    for (rep in 1:nreps) {
      if (!quiet)
        cat(paste0("value: ",value," of ",nvalues(object),"  replicate: ", rep," of ",nreps,"\n"))
            
      analysis <- jags_analysis(model = model, 
                    data = data_jags(subset_jags(object,value,rep))[[1]][[1]], 
                    niters = niters)  
      
      analyses[[value]][[rep]] <- as.jagr_power_analysis(analysis(analysis))
    }
  }

  class(object) <- c("jags_power_analysis","jags_simulation")
  
  model(object) <- model
  rhat_threshold(object) <- opts_jagr("rhat")
  analyses(object) <- analyses
  
#  object <- revise(object, parm = parm, level = level)

  return (object)
}
