 
#' @export
power<- function (object, parm = c(fixed = 0), level = "current") {
  
  if(!is.jags_power_analysis(object))
    stop("object must be a jags_power_analysis")
  
  old_opts <- opts_jagr(mode = mode)
  on.exit(opts_jagr(old_opts))
    
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
  
  #  object <- revise(object)
  
  return (object)
}
