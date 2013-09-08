
jagr_simulation <- function (model, data, quiet = FALSE)
{    
  stopifnot(is.jags_model(model))
      
  if (quiet) {
    options(jags.pb = "none")
  } else {
    options(jags.pb = "text")
  }
  
  if(!is.null(model$monitor)) {
    model$monitor <- sort(unique(model$monitor))
  }
  
  data_analysis <- translate_data(model$select, data)
  
  if (is.function(model$modify_data)) {
    if("analysis" %in% names(formals(model$modify_data))) {
      data_analysis <- model$modify_data (data_analysis, analysis = TRUE)
    } else {
      data_analysis <- model$modify_data (data_analysis)
    }
  }
  data_analysis$nrow <- NULL
  
  if (is.function(model$gen_inits)) {
    inits <- list()
    for (i in 1:n.chain) {   
      inits[[i]] <- model$gen_inits(data_analysis)
    }
  } else {
    inits <- NULL
  }
  
  ptm <- proc.time()
  
  file <- tempfile(fileext=".bug")
  cat(paste(model$model,"model { deviance <- 1}"), file=file)
  
  mcmc <- jags_analysis_internal (
    data = data_analysis, file = file, monitor = model$monitor, 
    inits = inits, n.chain = 1, 
    n.adapt = 0, n.burnin = 0, n.sim = 1, n.thin = 1, 
    quiet = quiet
  )

  if(is.null(model$monitor)) {
    model$monitor <- names(mcmc$mcmc)
    model$monitor <- sort(model$monitor)
  }
  
  object <- list(
    model = model, 
    data = data, 
    inits = inits, 
    mcmc = mcmc,
    time = ((proc.time () - ptm)[3]) / (60 * 60)
  )
  
  class(object) <- c("jagr_simulation")

  return (object)
}
