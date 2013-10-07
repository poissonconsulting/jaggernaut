
get_derived<- function (object, ...) {
  UseMethod("get_derived", object)
}

get_derived.jags_analysis <- function (object, monitor, data) {
  
  stopifnot(nmodels(object) == 1)
  stopifnot(is.character(monitor))
  stopifnot(length(monitor) > 0)
  stopifnot(is.data.frame(data) || is_data_list(data))

  dat <- translate_data(select(object), data_jags(object), data) 
    
  chains <- zero_random (object, dat)
  
  if (is.function(modify_data_derived(object)))
    dat <- modify_data_derived(object)(dat)
  
  model <- jags_model (derived_code(object), monitor = monitor)
  options(jags.pb = "none")
  file <- tempfile(fileext=".bug")
  cat(model_code(model), file=file)
    
  nchains <- nchains (chains)
  nsims <- nsims (chains) / nchains
  
  list <- list ()
  for (j in 1:nchains) {
    
    list[[j]] <- get_samples (model,data = c(dat,as.list(subset_jags(chains, sim = 1, chain = j))),file = file)    
    
    if (nsims > 1) {
      for (i in 2:nsims) {
        samples <- get_samples (model,data = c(dat,as.list(subset_jags(chains, sim = i, chain = j))), file = file)
        
        list[[j]] <- add_jags (list[[j]], samples, by = "sims")
      }
    }
  }    
  samples <- list [[1]]
    
  if (nchains > 1) {
    for (j in 2:nchains)
      samples <- add_jags (samples, list[[j]], by = "chains")
  }
    
  chains <- jagr_chains(samples, jags = list(NULL))
  
  return (chains)
}
