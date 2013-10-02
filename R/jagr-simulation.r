
jagr_simulation <- function (model, data, quiet = FALSE)
{    
  stopifnot(is.jags_data_model(model))
  
  model <- as.jagr_model(model)
      
  if (quiet) {
    options(jags.pb = "none")
  } else {
    options(jags.pb = "text")
  }
  
  if(!is.null(monitor(model))) {
    monitor(model) <- sort(unique(monitor(model)))
  }
  
  data_analysis <- translate_data(select(model), data)
  
  if (is.function(modify_data(model))) {
    if("analysis" %in% names(formals(modify_data(model)))) {
      data_analysis <- modify_data(model) (data_analysis, analysis = TRUE)
    } else {
      data_analysis <- modify_data(model) (data_analysis)
    }
  }
  data_analysis$nrow <- NULL
  
  if (is.function(gen_inits(model))) {
    inits <- list()
    inits[[i]] <- gen_inits(model)(data_analysis)
  } else {
    inits <- NULL
  }
  
  ptm <- proc.time()
  
  file <- tempfile(fileext=".bug")
  cat(paste(model_code(model),"model { deviance <- 1}"), file=file)
  
  mcmc <- jags_analysis_internal (
    data = data_analysis, file = file, monitor = monitor(model), 
    inits = inits, n.chain = 1, 
    n.adapt = 0, n.burnin = 0, n.sim = 1, n.thin = 1, 
    quiet = quiet
  )

  if(is.null(monitor(model))) {
    monitor(model) <- names(mcmc$mcmc)
    monitor(model) <- sort(monitor(model))
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
