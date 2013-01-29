
gssimulation <- function (
  block, values, debug = F
)
{
  if(!is.gsdata(block))
    stop ("block should be class gsdata")
  
  if(!(is.data.frame(values) && nrow(values) == 1))
    stop ("values should be a data frame with one row of parameter values")
  
  if (debug) {
    quiet <- FALSE
  } else {
    quiet <- TRUE
  }
  
  data_analysis <- translate_data(block, values)  
  
  if (is.function(block$gen_inits)) {
    inits <- list()
    inits[[1]] <- block$gen_inits(data_analysis)
  } else
    inits <- NULL
  
  file <- tempfile(fileext=".bug")
  cat(block$block, file=file)
  
  monitor <- block$monitor  
  
  mcmc <- jags_analysis ( 
      data = data_analysis, file=file, monitor = monitor, 
      inits = inits, n.chain = 1, 
      n.adapt = 0, n.burnin = 0, n.sim = 1, n.thin = 1, 
      quiet = quiet
    )
  
  if(is.null(monitor)) {
    monitor <- names(mcmc$mcmc)
    monitor <- sort(monitor)
    block$monitor <- monitor
  }

  data <- extract_estimates(mcmc)
  
  if(!is.null(block$extract_data)) {
    data <- block$extract_data(data)
  } 
    
  
  object <- list(
    block = block, 
    values = values, 
    inits = inits, 
    mcmc = mcmc,
    data = data
  )
  
  class(object) <- c("gssimulation")
  
  return (object)
}




